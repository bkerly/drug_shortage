# =============================================================================
# R/aggregate_risk.R — Per-country pharma supply chain risk aggregation
# =============================================================================
#
# Independence assumption:
#   P(at least one disruption from events 1..n) = 1 - ∏(1 - pᵢ)
#
# This is applied separately for LARGE and SMALL disruption classes,
# then combined into a composite risk score:
#   composite = Σ pᵢ × wᵢ   where w_large = 1.0, w_small = 0.3
#
# The composite score is NOT a probability — it's a weighted exposure index.
# =============================================================================

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(stringr)

# -----------------------------------------------------------------------------
# Internal helper
# -----------------------------------------------------------------------------

#' P(at least one) = 1 - prod(1 - p), clamping p away from boundary
.p_at_least_one <- function(probs) {
  if (length(probs) == 0) return(0)
  probs <- pmax(0.001, pmin(0.999, probs))
  1 - prod(1 - probs)
}

# -----------------------------------------------------------------------------
# Public API
# -----------------------------------------------------------------------------

#' Join markets (probabilities) with LLM evaluations into one analysis frame
#'
#' @param markets_df  Output of fetch_all_markets()
#' @param evals_df    Output of evaluate_markets_with_cache()
#' @return Tibble ready for aggregate_country_risk()
build_combined_df <- function(markets_df, evals_df) {
  markets_df |>
    inner_join(evals_df, by = "market_id") |>
    select(
      market_id,
      question,
      yes_prob,
      volume,
      liquidity,
      affects_pharma_supply,
      source_countries,
      disruption_level,
      reasoning,
      evaluated_at
    )
}

#' Aggregate per-country pharma supply chain disruption likelihood
#'
#' @param combined_df  Output of build_combined_df()
#' @return Tibble — one row per affected country, sorted by p_large_disruption desc
aggregate_country_risk <- function(combined_df) {

  # ── 1. Filter to pharma-relevant, disruption-causing markets ───────────────
  pharma_df <- combined_df |>
    filter(
      isTRUE(affects_pharma_supply),
      disruption_level != "NO DISRUPTION",
      !is.na(source_countries),
      nchar(trimws(source_countries)) > 0,
      !is.na(yes_prob)
    ) |>
    mutate(
      p          = pmax(0.001, pmin(0.999, yes_prob)),
      is_large   = disruption_level == "LARGE DISRUPTION",
      is_small   = disruption_level == "SMALL DISRUPTION",
      # Disruption weight for composite score
      weight     = case_when(
        is_large ~ 1.0,
        is_small ~ 0.3,
        TRUE     ~ 0
      )
    )

  if (nrow(pharma_df) == 0) {
    message("aggregate_country_risk: no pharma-relevant markets found.")
    return(tibble())
  }

  # ── 2. Expand to one row per (event × country) ─────────────────────────────
  country_events <- pharma_df |>
    mutate(country = str_split(source_countries, ";\\s*")) |>
    unnest(country) |>
    mutate(country = str_trim(country)) |>
    filter(nchar(country) > 0)

  n_country_events <- nrow(country_events)
  message(sprintf(
    "Aggregating risk across %d country-event pairs (%d unique countries)...",
    n_country_events,
    n_distinct(country_events$country)
  ))

  # ── 3. Per-country risk summary ────────────────────────────────────────────
  risk_summary <- country_events |>
    group_by(country) |>
    summarise(
      # Counts
      n_events            = n(),
      n_large             = sum(is_large),
      n_small             = sum(is_small),

      # Independence-aggregated probabilities
      p_any_disruption    = .p_at_least_one(p),
      p_large_disruption  = .p_at_least_one(p[is_large]),
      p_small_disruption  = .p_at_least_one(p[is_small]),

      # Weighted exposure index (not a probability; additive)
      composite_risk      = sum(p * weight),

      # Human-readable: top driving events for each tier
      top_large_events    = {
        sub <- cur_data() |> filter(is_large) |> arrange(desc(p))
        paste(head(sub$question, 3), collapse = " || ")
      },
      top_small_events    = {
        sub <- cur_data() |> filter(is_small) |> arrange(desc(p))
        paste(head(sub$question, 2), collapse = " || ")
      },

      .groups = "drop"
    ) |>
    arrange(desc(p_large_disruption), desc(composite_risk)) |>
    mutate(
      across(c(p_any_disruption, p_large_disruption, p_small_disruption),
             ~ round(.x, 4)),
      composite_risk = round(composite_risk, 4),
      risk_tier = case_when(
        p_large_disruption >= 0.20 ~ "HIGH",
        p_large_disruption >= 0.05 ~ "MEDIUM",
        p_small_disruption >= 0.10 ~ "LOW",
        TRUE                       ~ "MINIMAL"
      )
    )

  risk_summary
}

#' Pretty-print the risk summary to console
print_risk_summary <- function(risk_df, n = 15) {
  cat("\n╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   PHARMA SUPPLY CHAIN RISK MONITOR — Country Risk Summary   ║\n")
  cat(sprintf("║   Generated: %-47s║\n", format(Sys.time(), "%Y-%m-%d %H:%M %Z")))
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  top <- head(risk_df |> filter(n_large > 0 | p_small_disruption > 0.05), n)
  if (nrow(top) == 0) {
    cat("No significant disruption risks identified.\n")
    return(invisible(NULL))
  }

  for (i in seq_len(nrow(top))) {
    r <- top[i, ]
    cat(sprintf(
      "[%s] %-18s  Large: %4.1f%%  Small: %4.1f%%  Any: %4.1f%%  Score: %.3f\n",
      r$risk_tier, r$country,
      r$p_large_disruption * 100,
      r$p_small_disruption * 100,
      r$p_any_disruption   * 100,
      r$composite_risk
    ))
    if (nchar(r$top_large_events) > 0)
      cat(sprintf("         ↳ %s\n", substr(r$top_large_events, 1, 90)))
  }
  cat("\n")
}
