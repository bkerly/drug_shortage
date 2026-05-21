# =============================================================================
# config.R — Pharma Supply Chain Risk Monitor
# =============================================================================

# --- API endpoints -----------------------------------------------------------
POLYMARKET_BASE_URL <- "https://gamma-api.polymarket.com"
OLLAMA_BASE_URL     <- Sys.getenv("OLLAMA_HOST", unset = "http://localhost:11434")
OLLAMA_MODEL        <- "mistral"

# --- File paths --------------------------------------------------------------
DATA_DIR            <- "data"
MARKETS_CACHE_FILE  <- file.path(DATA_DIR, "markets_cache.rds")
EVAL_CACHE_FILE     <- file.path(DATA_DIR, "evaluations_cache.rds")
RISK_FILE           <- file.path(DATA_DIR, "risk_summary.rds")

# --- Fetch settings ----------------------------------------------------------
MARKETS_PER_PAGE    <- 100    # max per Gamma API page
MAX_MARKETS         <- 2000   # safety cap to avoid runaway pagination
API_SLEEP_SEC       <- 0.3    # polite delay between Polymarket calls

# --- LLM settings ------------------------------------------------------------
LLM_SLEEP_SEC       <- 0.5    # delay between Ollama calls (local, but still)
MAX_LLM_RETRIES     <- 3      # retries on parse failure

# --- Analysis settings -------------------------------------------------------
HORIZON_MONTHS      <- 6      # look-ahead window for disruption assessment.
                               # Only events whose pharma impact could materialise
                               # within this window are classified as disruptions.
                               # IMPORTANT: changing this invalidates the evaluation
                               # cache — delete data/evaluations_cache.rds and rerun.

# --- Correlation / clustering settings ---------------------------------------
# Events within the same risk_cluster are treated as CORRELATED (max aggregation).
# Events across different clusters are treated as INDEPENDENT (product formula).
# This two-stage approach prevents the near-100% country probabilities that arise
# when many correlated markets (e.g. 20 "US-China tensions" questions) are
# naively treated as independent.
RUN_CLUSTER_NORMALIZATION <- TRUE   # batch-normalize cluster names via LLM (weekly)

# --- Null coalescing helper (used across all modules) ------------------------
`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && !is.na(x[1])) x else y
