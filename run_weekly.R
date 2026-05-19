#!/usr/bin/env Rscript
# =============================================================================
# run_weekly.R — Full pipeline
# =============================================================================
# Schedule: cron  0 6 * * 1   Rscript /path/to/pharma_risk/run_weekly.R
#
# What it does:
#   1. Fetch ALL active Polymarket markets
#   2. Evaluate any NEW markets with Mistral via Ollama (cached markets skipped)
#   3. Aggregate per-country disruption probabilities
#   4. Save risk_summary.rds + a timestamped CSV
# =============================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path) %||%
        normalizePath("."))   # works from both RStudio and Rscript CLI

source("config.R")
source("R/fetch_polymarket.R")
source("R/evaluate_llm.R")
source("R/aggregate_risk.R")

run_weekly <- function() {
  message(rep("=", 60))
  message("WEEKLY PHARMA SUPPLY CHAIN RISK RUN — ", Sys.time())
  message(rep("=", 60))

  if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

  # ── Step 1: Fetch markets ──────────────────────────────────────────────────
  markets_df <- fetch_all_markets(max_markets = MAX_MARKETS)

  if (nrow(markets_df) == 0) {
    stop("No markets returned from Polymarket API. ",
         "Check connectivity to ", POLYMARKET_BASE_URL)
  }
  message("Total active markets: ", nrow(markets_df))
  saveRDS(markets_df, MARKETS_CACHE_FILE)

  # ── Step 2: LLM evaluation (cache-aware) ───────────────────────────────────
  message("\nChecking Ollama at ", OLLAMA_BASE_URL, " ...")
  ping <- tryCatch(
    request(paste0(OLLAMA_BASE_URL, "/api/tags")) |>
      req_timeout(5) |> req_perform(),
    error = function(e) NULL
  )
  if (is.null(ping)) {
    stop("Cannot reach Ollama at ", OLLAMA_BASE_URL,
         ". Ensure Ollama is running and 'mistral' is pulled.")
  }

  evals_df <- evaluate_markets_with_cache(markets_df, EVAL_CACHE_FILE)

  # ── Step 3: Build combined dataframe ──────────────────────────────────────
  combined_df <- build_combined_df(markets_df, evals_df)

  pharma_relevant <- combined_df |>
    dplyr::filter(isTRUE(affects_pharma_supply))

  message(sprintf(
    "\nEvaluated: %d markets | Pharma-relevant: %d | Large disruption: %d | Small: %d",
    nrow(combined_df),
    nrow(pharma_relevant),
    sum(pharma_relevant$disruption_level == "LARGE DISRUPTION", na.rm = TRUE),
    sum(pharma_relevant$disruption_level == "SMALL DISRUPTION", na.rm = TRUE)
  ))

  # ── Step 4: Aggregate risk ─────────────────────────────────────────────────
  risk_df <- aggregate_country_risk(combined_df)

  # ── Step 5: Save outputs ───────────────────────────────────────────────────
  saveRDS(risk_df, RISK_FILE)

  csv_path <- file.path(DATA_DIR,
    paste0("risk_weekly_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  readr::write_csv(risk_df, csv_path)

  detail_path <- file.path(DATA_DIR,
    paste0("market_detail_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  readr::write_csv(combined_df, detail_path)

  # ── Step 6: Console summary ────────────────────────────────────────────────
  print_risk_summary(risk_df)

  message("Outputs saved:")
  message("  Risk summary : ", csv_path)
  message("  Market detail: ", detail_path)
  message("  RDS cache    : ", RISK_FILE)
  message(rep("=", 60))
  message("WEEKLY RUN COMPLETE — ", Sys.time())

  invisible(list(
    markets     = markets_df,
    evaluations = evals_df,
    combined    = combined_df,
    risk        = risk_df
  ))
}

# Entry point when called via Rscript
if (!interactive()) {
  tryCatch(
    run_weekly(),
    error = function(e) {
      message("FATAL: ", e$message)
      quit(status = 1)
    }
  )
} else {
  # In RStudio: just call run_weekly() manually
  message("Source loaded. Run:  results <- run_weekly()")
}
