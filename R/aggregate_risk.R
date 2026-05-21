# =============================================================================
# R/aggregate_risk.R — Per-country pharma supply chain risk aggregation
# =============================================================================
#
# TWO-STAGE AGGREGATION MODEL
# ───────────────────────────
# Stage 1  Within-cluster (correlated events)
#   Events in the same risk_cluster measure the same underlying scenario.
#   "Trump threatens Iran" and "Strait of Hormuz closure" are both symptoms
#   of iran_hormuz_tensions — treating them as independent inflates the total.
#   → Per cluster: P(cluster causes LARGE) = max(pᵢ | LARGE events in cluster)
#                  P(cluster causes SMALL) = max(pᵢ | SMALL events in cluster)
#
# Stage 2  Across-cluster (independent scenarios)
#   Different clusters represent genuinely distinct geopolitical scenarios
#   (e.g. iran_hormuz_tensions vs india_export_restrictions are unrelated).
#   → Across clusters: P(at least one) = 1 − ∏(1 − P_cluster_k)
#
# Backwards compatibility: markets without a risk_cluster (old cache entries)
# are each treated as their own cluster (same as naive independence, preserving
# the old behaviour for those rows).
#
# composite_risk = Σ_k max(cluster_large_p_k × 1.0, cluster_small_p_k × 0.3)
#   — a weighted exposure index, NOT a probability.
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
  # Defensive deduplication: markets_df may repeat a condition_id if the
  # Gamma API returned the same market on two pagination pages; evals_df may
  # have accumulated duplicates across partial runs.
  markets_clean <- markets_df |>
    arrange(desc(fetched_at)) |>
    distinct(market_id, .keep_all = TRUE)

  evals_clean <- evals_df |>
    arrange(desc(evaluated_at)) |>
    distinct(market_id, .keep_all = TRUE)

  markets_clean |>
    left_join(evals_clean, by = "market_id", relationship = "many-to-one") |>
    select(
      market_id,
      question,
      yes_prob,
      volume,
      liquidity,
      affects_pharma_supply,
      source_countries,
      disruption_level,
      risk_cluster,          # may be NA for old cache entries — handled in aggregation
      reasoning,
      evaluated_at
    )
}

#' Aggregate per-country pharma supply chain disruption likelihood
#'
#' Uses a two-stage model: max() within clusters (correlated events),
#' then independence formula across clusters (distinct scenarios).
#' See file header for full methodology notes.
#'
#' @param combined_df  Output of build_combined_df()
#' @return Tibble — one row per affected country, sorted by p_large_disruption desc
aggregate_country_risk <- function(combined_df) {

  # ── 1. Filter to pharma-relevant, disruption-causing markets ──────────────
  pharma_df <- combined_df |>
    filter(
      isTRUE(affects_pharma_supply),
      disruption_level != "NO DISRUPTION",
      !is.na(source_countries),
      nchar(trimws(source_countries)) > 0,
      !is.na(yes_prob)
    ) |>
    mutate(
      p        = pmax(0.001, pmin(0.999, yes_prob)),
      is_large = disruption_level == "LARGE DISRUPTION",
      is_small = disruption_level == "SMALL DISRUPTION",
      weight   = if_else(is_large, 1.0, 0.3),
      # Backwards compat: events without a cluster become their own singleton cluster
      cluster  = if_else(!is.na(risk_cluster) & nchar(trimws(risk_cluster)) > 0,
                         risk_cluster, market_id)
    )

  if (nrow(pharma_df) == 0) {
    message("aggregate_country_risk: no pharma-relevant markets found.")
    return(tibble())
  }

  # ── 2. Expand to one row per (event × country) ────────────────────────────
  country_events <- pharma_df |>
    mutate(country_list = str_split(source_countries, ";\\s*")) |>
    unnest(country_list) |>
    rename(country = country_list) |>
    mutate(country = str_trim(country)) |>
    filter(nchar(country) > 0)

  message(sprintf(
    "Two-stage aggregation: %d country-event pairs | %d countries | %d clusters",
    nrow(country_events),
    n_distinct(country_events$country),
    n_distinct(country_events$cluster)
  ))

  # ── Stage 1: Within-cluster max (correlated events) ───────────────────────
  # Within each (country, cluster), take the maximum probability per disruption
  # tier. This treats correlated events as measuring the same underlying risk.
  cluster_summary <- country_events |>
    group_by(country, cluster) |>
    summarise(
      # max() on an empty subset returns -Inf; prepending 0 avoids the warning
      cluster_large_p  = suppressWarnings(max(c(0, p[is_large]))),
      cluster_small_p  = suppressWarnings(max(c(0, p[is_small]))),
      n_in_cluster     = n(),
      # Most representative event for display (highest weighted probability)
      top_event        = {
        best <- which.max(p * weight)
        if (length(best) > 0) question[best[1]] else NA_character_
      },
      .groups = "drop"
    )

  # ── Stage 2: Across-cluster independence (distinct scenarios) ─────────────
  country_risk <- cluster_summary |>
    group_by(country) |>
    summarise(
      n_clusters         = n(),
      n_events           = sum(n_in_cluster),
      n_large            = sum(cluster_large_p > 0),
      n_small            = sum(cluster_small_p > 0),

      # Independence across clusters, applied to per-cluster max probabilities
      p_large_disruption = .p_at_least_one(cluster_large_p[cluster_large_p > 0]),
      p_small_disruption = .p_at_least_one(cluster_small_p[cluster_small_p > 0]),

      # P(any disruption) treats large and small within each cluster as jointly
      # possible but independent across clusters
      p_any_disruption   = 1 - prod((1 - cluster_large_p) * (1 - cluster_small_p)),

      # Composite exposure index (NOT a probability; additive across clusters)
      composite_risk     = sum(cluster_large_p * 1.0 + cluster_small_p * 0.3),

      # Top driving events (by cluster-level max probability)
      top_large_events   = {
        idx  <- order(-cluster_large_p)
        evts <- top_event[idx][cluster_large_p[idx] > 0]
        if (length(evts) > 0) paste(head(evts, 3), collapse = " || ") else NA_character_
      },
      top_small_events   = {
        idx  <- order(-cluster_small_p)
        evts <- top_event[idx][cluster_small_p[idx] > 0]
        if (length(evts) > 0) paste(head(evts, 2), collapse = " || ") else NA_character_
      },
      top_clusters       = paste(
        head(cluster[order(-pmax(cluster_large_p, cluster_small_p))], 5),
        collapse = ", "
      ),

      .groups = "drop"
    ) |>
    arrange(desc(p_large_disruption), desc(composite_risk)) |>
    mutate(
      # Cap at 0.9999 to avoid exactly-1 artefacts from floating point
      across(c(p_any_disruption, p_large_disruption, p_small_disruption),
             ~ round(pmin(.x, 0.9999), 4)),
      composite_risk = round(composite_risk, 4),
      risk_tier = case_when(
        p_large_disruption >= 0.20 ~ "HIGH",
        p_large_disruption >= 0.05 ~ "MEDIUM",
        p_small_disruption >= 0.10 ~ "LOW",
        TRUE                       ~ "MINIMAL"
      )
    )

  country_risk
}

#' Pretty-print the risk summary to console
print_risk_summary <- function(risk_df, n = 15) {
  cat("\n╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   PHARMA SUPPLY CHAIN RISK MONITOR — Country Risk Summary   ║\n")
  cat(sprintf("║   Generated: %-47s║\n", format(Sys.time(), "%Y-%m-%d %H:%M %Z")))
  cat(sprintf("║   Horizon:   %-47s║\n",
              paste0(HORIZON_MONTHS, " months  |  two-stage cluster aggregation")))
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  top <- head(risk_df |> filter(n_large > 0 | p_small_disruption > 0.05), n)
  if (nrow(top) == 0) {
    cat("No significant disruption risks identified.\n")
    return(invisible(NULL))
  }

  for (i in seq_len(nrow(top))) {
    r <- top[i, ]
    n_cl <- r$n_clusters %||% r$n_events   # fall back for old data
    cat(sprintf(
      "[%s] %-18s  Large: %4.1f%%  Small: %4.1f%%  Any: %4.1f%%  (%d clusters / %d events)\n",
      r$risk_tier, r$country,
      r$p_large_disruption * 100,
      r$p_small_disruption * 100,
      r$p_any_disruption   * 100,
      n_cl, r$n_events
    ))
    if (!is.na(r$top_large_events) && nchar(r$top_large_events) > 0)
      cat(sprintf("         ↳ %s\n", substr(r$top_large_events, 1, 90)))
    if (!is.null(r$top_clusters) && !is.na(r$top_clusters) && nchar(r$top_clusters) > 0)
      cat(sprintf("         clusters: %s\n", r$top_clusters))
  }
  cat("\n")
}
