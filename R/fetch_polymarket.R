# =============================================================================
# R/fetch_polymarket.R — Polymarket Gamma API client
# =============================================================================
# Uses the public Gamma API (no auth required for reads):
#   https://gamma-api.polymarket.com/markets
#
# Each market has outcomePrices as a JSON string: '["0.72","0.28"]'
# prices[1] = P(YES) = our event probability.
# =============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)

# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

#' Parse a single raw market object into a clean tibble row
.parse_market <- function(m) {
  prices <- tryCatch(
    as.numeric(fromJSON(m$outcomePrices %||% '["0.5","0.5"]')),
    error = function(e) c(0.5, 0.5)
  )
  yes_prob <- prices[1]
  if (is.na(yes_prob) || yes_prob < 0 || yes_prob > 1) yes_prob <- 0.5

  tags <- if (!is.null(m$tags) && length(m$tags) > 0)
    paste(map_chr(m$tags, ~ .x$label %||% ""), collapse = ", ")
  else ""

  tibble(
    market_id   = as.character(m$id %||% NA_character_),
    question    = m$question %||% NA_character_,
    description = substr(m$description %||% "", 1, 800),
    slug        = m$slug %||% NA_character_,
    end_date    = m$endDate %||% NA_character_,
    yes_prob    = yes_prob,
    volume      = as.numeric(m$volume %||% 0),
    liquidity   = as.numeric(m$liquidity %||% 0),
    tags        = tags,
    fetched_at  = Sys.time()
  )
}

# -----------------------------------------------------------------------------
# Public API
# -----------------------------------------------------------------------------

#' Fetch one page of active markets from Polymarket Gamma API
#'
#' @param limit   Records per page (max 100)
#' @param offset  Pagination offset
#' @return Raw list of market objects, or empty list on error
fetch_markets_page <- function(limit = 100, offset = 0) {
  url <- paste0(
    POLYMARKET_BASE_URL, "/markets",
    "?active=true&closed=false",
    "&limit=", limit,
    "&offset=", offset,
    "&order=volume&ascending=false"
  )
  tryCatch({
    resp <- request(url) |>
      req_timeout(30) |>
      req_retry(max_tries = 3, backoff = ~ 2) |>
      req_perform()
    resp_body_json(resp)
  }, error = function(e) {
    message("  [fetch_markets_page] Error at offset ", offset, ": ", e$message)
    list()
  })
}

#' Fetch ALL active markets, paginating until exhausted
#'
#' @param max_markets Safety cap on total markets fetched
#' @return Tibble of all active markets; empty tibble if API unreachable
fetch_all_markets <- function(max_markets = MAX_MARKETS) {
  message("Fetching active markets from Polymarket...")
  all_rows <- list()
  offset   <- 0

  repeat {
    page <- fetch_markets_page(limit = MARKETS_PER_PAGE, offset = offset)

    if (length(page) == 0) break

    parsed <- map(page, safely(.parse_market)) |>
      keep(~ is.null(.x$error)) |>
      map(~ .x$result)

    all_rows <- c(all_rows, parsed)
    message("  ", length(all_rows), " markets fetched so far...")

    if (length(page) < MARKETS_PER_PAGE) break   # last page
    if (length(all_rows) >= max_markets)  break   # safety cap

    offset <- offset + MARKETS_PER_PAGE
    Sys.sleep(API_SLEEP_SEC)
  }

  if (length(all_rows) == 0) {
    warning("fetch_all_markets: no markets returned. Check API connectivity.")
    return(tibble())
  }

  bind_rows(all_rows) |>
    filter(!is.na(market_id), !is.na(question))
}

#' Refresh only the YES probability for a set of markets (for daily updates)
#'
#' Fetches each market individually by ID.
#' NOTE: If the Gamma API changes its single-market endpoint, adjust the URL
#'       in this function (currently: /markets?id={id}).
#'
#' @param market_ids Character vector of market IDs
#' @return Tibble with columns: market_id, yes_prob, refreshed_at
refresh_market_prices <- function(market_ids) {
  message("Refreshing prices for ", length(market_ids), " markets...")

  map_dfr(market_ids, function(mid) {
    url <- paste0(POLYMARKET_BASE_URL, "/markets?id=", mid, "&limit=1")
    result <- tryCatch({
      resp <- request(url) |> req_timeout(15) |> req_perform()
      page <- resp_body_json(resp)
      if (length(page) == 0) stop("empty response")
      m <- page[[1]]
      prices <- tryCatch(
        as.numeric(fromJSON(m$outcomePrices %||% '["0.5","0.5"]')),
        error = function(e) c(0.5, 0.5)
      )
      tibble(market_id = mid, yes_prob = prices[1], refreshed_at = Sys.time())
    }, error = function(e) {
      message("  Price refresh failed for ", mid, ": ", e$message)
      tibble(market_id = mid, yes_prob = NA_real_, refreshed_at = Sys.time())
    })
    Sys.sleep(API_SLEEP_SEC)
    result
  })
}

