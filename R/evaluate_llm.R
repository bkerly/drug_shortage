# =============================================================================
# R/evaluate_llm.R — LLM evaluation via Ollama (Mistral)
# =============================================================================
# Calls a local Ollama instance to classify each Polymarket question for:
#   a) Does it affect pharmaceutical supply chain at all?
#   b) Which source country's pharma manufacturing/exports are affected?
#   c) Disruption magnitude: LARGE DISRUPTION / SMALL DISRUPTION / NO DISRUPTION
#
# Results are cached to EVAL_CACHE_FILE; already-evaluated markets are skipped.
# =============================================================================

library(httr2)
library(jsonlite)
library(purrr)
library(dplyr)
library(tibble)
library(stringr)

# -----------------------------------------------------------------------------
# Prompt construction
# -----------------------------------------------------------------------------

# Countries that are major pharma manufacturers / API exporters — used to
# anchor the LLM's country identification.
PHARMA_SOURCE_NATIONS <- paste(
  "China (largest API producer), India (largest generics exporter),",
  "Germany, Switzerland, Ireland, USA, Italy, France, Belgium, Japan,",
  "South Korea, Singapore, UK, Netherlands, Israel, Canada, Australia"
)

#' Build the structured evaluation prompt for a single market
.build_prompt <- function(question, description, yes_prob) {
  desc_block <- if (nchar(trimws(description)) > 5)
    paste0("\nAdditional context: ", substr(description, 1, 600))
  else ""

  paste0(
    "You are a pharmaceutical supply chain risk analyst specialising in API ",
    "(Active Pharmaceutical Ingredient) manufacturing, raw material sourcing, ",
    "finished drug logistics, and export controls.\n\n",

    "Major pharma source nations: ", PHARMA_SOURCE_NATIONS, "\n\n",

    "PREDICTION MARKET\n",
    "Question: ", question, desc_block, "\n",
    "Current probability this resolves YES: ", round(yes_prob * 100, 1), "%\n\n",

    "Assume the event DOES occur (resolves YES). Assess the pharmaceutical ",
    "supply chain impact.\n\n",

    "Rules:\n",
    "- LARGE DISRUPTION: major production shutdowns, export bans, sanctions, ",
    "  war or severe instability in a key pharma-manufacturing nation, ",
    "  or loss of >5% of global API/generics capacity for a drug class.\n",
    "- SMALL DISRUPTION: minor trade friction, localised strikes, brief port ",
    "  closures, regulatory delays with modest pharma exposure.\n",
    "- NO DISRUPTION: event unrelated to pharma supply (sports, entertainment, ",
    "  elections in non-pharma nations with no trade implications, etc.).\n",
    "- source_countries: the countries whose MANUFACTURING or EXPORT of pharma ",
    "  inputs would be disrupted — NOT the countries that would suffer shortages.\n",
    "- Use an empty array [] for source_countries when NO DISRUPTION.\n\n",

    "Respond with ONLY a valid JSON object — no markdown, no commentary:\n",
    "{\n",
    '  "affects_pharma_supply": true or false,\n',
    '  "source_countries": ["Country Name", ...],\n',
    '  "disruption_level": "LARGE DISRUPTION" or "SMALL DISRUPTION" or "NO DISRUPTION",\n',
    '  "reasoning": "one concise sentence"\n',
    "}"
  )
}

# -----------------------------------------------------------------------------
# Ollama client
# -----------------------------------------------------------------------------

#' POST a prompt to the local Ollama /api/generate endpoint
#' @return Raw response string, or NULL on error
.call_ollama <- function(prompt, model = OLLAMA_MODEL) {
  tryCatch({
    resp <- request(paste0(OLLAMA_BASE_URL, "/api/generate")) |>
      req_body_json(list(
        model   = model,
        prompt  = prompt,
        stream  = FALSE,
        options = list(temperature = 0.05)   # near-deterministic for structured output
      )) |>
      req_timeout(90) |>
      req_perform()

    resp_body_json(resp)$response
  }, error = function(e) {
    message("  [Ollama] Request failed: ", e$message)
    NULL
  })
}

#' Extract a JSON object from an LLM response (handles stray markdown fences)
.extract_json <- function(text) {
  if (is.null(text) || nchar(trimws(text)) == 0) return(NULL)

  # Try ```json ... ``` block first
  m <- str_match(text, "```(?:json)?\\s*\\n?(\\{[\\s\\S]*?\\})\\s*\\n?```")
  if (!is.na(m[1, 2])) return(m[1, 2])

  # Fall back to first {...} in the response
  m2 <- str_match(text, "(\\{[\\s\\S]*\\})")
  if (!is.na(m2[1, 2])) return(m2[1, 2])

  NULL
}

#' Parse LLM JSON into a validated named list
.parse_response <- function(text) {
  json_str <- .extract_json(text)
  if (is.null(json_str)) return(NULL)

  tryCatch({
    p <- fromJSON(json_str)

    valid_levels <- c("LARGE DISRUPTION", "SMALL DISRUPTION", "NO DISRUPTION")
    dlevel <- p$disruption_level %||% "NO DISRUPTION"
    if (!dlevel %in% valid_levels) dlevel <- "NO DISRUPTION"

    list(
      affects_pharma_supply = isTRUE(p$affects_pharma_supply),
      source_countries      = paste(p$source_countries %||% character(0), collapse = "; "),
      disruption_level      = dlevel,
      reasoning             = substr(p$reasoning %||% "", 1, 400)
    )
  }, error = function(e) NULL)
}

# -----------------------------------------------------------------------------
# Public evaluation API
# -----------------------------------------------------------------------------

#' Evaluate a single market with the LLM, with retry on parse failure
#'
#' @return Single-row tibble with evaluation columns
evaluate_market <- function(market_id, question, description, yes_prob) {
  prompt <- .build_prompt(question, description, yes_prob)
  result <- NULL

  for (attempt in seq_len(MAX_LLM_RETRIES)) {
    raw    <- .call_ollama(prompt)
    result <- .parse_response(raw)
    if (!is.null(result)) break
    message("  [LLM] Retry ", attempt, "/", MAX_LLM_RETRIES,
            " — ", substr(question, 1, 70), "...")
    Sys.sleep(1)
  }

  if (is.null(result)) {
    message("  [LLM] Giving up on: ", substr(question, 1, 70))
    result <- list(
      affects_pharma_supply = NA,
      source_countries      = NA_character_,
      disruption_level      = "NO DISRUPTION",
      reasoning             = "parse_error"
    )
  }

  tibble(
    market_id             = market_id,
    affects_pharma_supply = result$affects_pharma_supply,
    source_countries      = result$source_countries,
    disruption_level      = result$disruption_level,
    reasoning             = result$reasoning,
    evaluated_at          = Sys.time()
  )
}

#' Evaluate all markets, using an on-disk cache to skip already-evaluated ones
#'
#' The cache is keyed on market_id only — if a market's question never changes
#' (it doesn't on Polymarket), there is no need to re-evaluate it.
#'
#' @param markets_df Tibble with at minimum: market_id, question, description, yes_prob
#' @param cache_file Path to .rds cache (created if absent)
#' @return Complete tibble of evaluations (cached + newly evaluated)
evaluate_markets_with_cache <- function(markets_df,
                                        cache_file = EVAL_CACHE_FILE) {
  cache <- if (file.exists(cache_file)) readRDS(cache_file) else tibble()

  already_done <- if (nrow(cache) > 0) cache$market_id else character(0)
  to_do        <- markets_df |> filter(!market_id %in% already_done)

  message(sprintf(
    "LLM evaluation — %d new markets, %d already cached",
    nrow(to_do), nrow(markets_df) - nrow(to_do)
  ))

  if (nrow(to_do) == 0) return(cache)

  new_evals <- map_dfr(seq_len(nrow(to_do)), function(i) {
    m <- to_do[i, ]
    if (i %% 25 == 0 || i == 1)
      message(sprintf("  Evaluating %d / %d ...", i, nrow(to_do)))

    result <- evaluate_market(m$market_id, m$question,
                              m$description, m$yes_prob)
    Sys.sleep(LLM_SLEEP_SEC)
    result
  })

  updated <- bind_rows(cache, new_evals)
  saveRDS(updated, cache_file)
  message("Evaluation cache saved: ", nrow(updated), " total records → ", cache_file)

  updated
}

