#!/usr/bin/env Rscript
# =============================================================================
# run_daily.R — Daily refresh of LARGE DISRUPTION market probabilities
# =============================================================================
# Schedule: cron  0 7 * * *   Rscript /path/to/pharma_risk/run_daily.R
#
# What it does (fast — no new LLM calls):
#   1. Load cached markets + evaluations from last weekly run
#   2. Re-fetch CURRENT prices ONLY for LARGE DISRUPTION markets
#   3. Recompute per-country risk with updated probabilities
#   4. Save updated risk_summary.rds + timestamped daily CSV
#
# Prerequisite: run_weekly.R must have been run at least once.
# =============================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path) %||%
        normalizePath("."))

source("config.R")
source("R/fetch_polymarket.R")
source("R/evaluate_llm.R")   # needed for build_combined_df
source("R/aggregate_risk.R")

run_daily <- function() {
  message(rep("=", 60))
  message("DAILY LARGE DISRUPTION REFRESH — ", Sys.time())
  message(rep("=", 60))

  # ── Prereq check ───────────────────────────────────────────────────────────
  for (f in c(MARKETS_CACHE_FILE, EVAL_CACHE_FILE)) {
    if (!file.exists(f))
      stop("Cache file not found: ", f, "\nRun run_weekly.R first.")
  }

  markets_df <- readRDS(MARKETS_CACHE_FILE)
  evals_df   <- readRDS(EVAL_CACHE_FILE)

  # ── Identify LARGE DISRUPTION markets ─────────────────────────────────────
  large_ids <- evals_df |>
    dplyr::filter(
      isTRUE(affects_pharma_supply),
      disruption_level == "LARGE DISRUPTION"
    ) |>
    dplyr::pull(market_id)

  message("LARGE DISRUPTION markets to refresh: ", length(large_ids))

  if (length(large_ids) == 0) {
    message("No LARGE DISRUPTION markets in cache — nothing to refresh.")
    message("(This is either very good news or the weekly run hasn't been done.)")
    return(invisible(NULL))
  }

  # ── Refresh prices ─────────────────────────────────────────────────────────
  fresh_prices <- refresh_market_prices(large_ids)

  # How many prices actually changed?
  old_probs <- markets_df |>
    dplyr::filter(market_id %in% large_ids) |>
    dplyr::select(market_id, yes_prob_old = yes_prob)

  changes <- fresh_prices |>
    dplyr::left_join(old_probs, by = "market_id") |>
    dplyr::mutate(delta = abs(yes_prob - yes_prob_old)) |>
    dplyr::arrange(dplyr::desc(delta))

  n_changed <- sum(changes$delta > 0.005, na.rm = TRUE)
  message(sprintf(
    "Price updates: %d markets changed by >0.5pp", n_changed
  ))

  if (n_changed > 0) {
    cat("\nTop movers:\n")
    changes |>
      dplyr::filter(delta > 0.005) |>
      dplyr::left_join(
        markets_df |> dplyr::select(market_id, question),
        by = "market_id"
      ) |>
      dplyr::mutate(
        direction = dplyr::if_else(yes_prob > yes_prob_old, "▲", "▼")
      ) |>
      dplyr::slice_head(n = 10) |>
      dplyr::select(direction, delta, yes_prob, yes_prob_old, question) |>
      print()
    cat("\n")
  }

  # ── Update markets cache with fresh prices ─────────────────────────────────
  markets_updated <- markets_df |>
    dplyr::rows_update(
      fresh_prices |>
        dplyr::filter(!is.na(yes_prob)) |>
        dplyr::select(market_id, yes_prob),
      by = "market_id",
      unmatched = "ignore"
    )
  saveRDS(markets_updated, MARKETS_CACHE_FILE)

  # ── Recompute risk ─────────────────────────────────────────────────────────
  combined_df <- build_combined_df(markets_updated, evals_df)
  risk_df     <- aggregate_country_risk(combined_df)

  # ── Save ───────────────────────────────────────────────────────────────────
  saveRDS(risk_df, RISK_FILE)

  csv_path <- file.path(DATA_DIR,
    paste0("risk_daily_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  readr::write_csv(risk_df, csv_path)

  # ── Console output ─────────────────────────────────────────────────────────
  print_risk_summary(risk_df)

  message("Daily CSV saved: ", csv_path)
  message(rep("=", 60))
  message("DAILY REFRESH COMPLETE — ", Sys.time())

  invisible(list(markets = markets_updated, risk = risk_df))
}

if (!interactive()) {
  tryCatch(
    run_daily(),
    error = function(e) {
      message("FATAL: ", e$message)
      quit(status = 1)
    }
  )
} else {
  message("Source loaded. Run:  results <- run_daily()")
}
