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

# Standard cluster names the LLM should prefer. New clusters can be created
# with snake_case when none of these fit. Kept in one place so normalize_clusters()
# can reference the same list.
STANDARD_CLUSTERS <- c(
  "china_taiwan_conflict",
  "us_china_trade_war",
  "iran_hormuz_tensions",
  "india_export_restrictions",
  "russia_ukraine_war",
  "middle_east_instability",
  "global_shipping_disruption",
  "us_drug_pricing_policy",
  "us_sanctions_policy",
  "southeast_asia_supply",
  "european_energy_crisis",
  "latin_america_instability",
  "north_korea_tensions",
  "pandemic_biosecurity",
  "south_asia_regional"
)

#' Build the structured evaluation prompt for a single market
.build_prompt <- function(question, description, yes_prob) {
  desc_block <- if (nchar(trimws(description)) > 5)
    paste0("\nAdditional context: ", substr(description, 1, 600))
  else ""

  horizon_str   <- paste0("within the next ", HORIZON_MONTHS, " month",
                           if (HORIZON_MONTHS != 1) "s" else "")
  clusters_hint <- paste(STANDARD_CLUSTERS, collapse=", ")

  paste0(
    "You are a pharmaceutical supply chain risk analyst specialising in API ",
    "(Active Pharmaceutical Ingredient) manufacturing, raw material sourcing, ",
    "finished drug logistics, and export controls.\n\n",

    "Major pharma source nations: ", PHARMA_SOURCE_NATIONS, "\n\n",

    "PREDICTION MARKET\n",
    "Question: ", question, desc_block, "\n",
    "Current probability this resolves YES: ", round(yes_prob * 100, 1), "%\n\n",

    "TIME WINDOW: Assess impact ", horizon_str, " only.\n",
    "If the pharma supply impact would not materialise ", horizon_str,
    ", classify as NO DISRUPTION regardless of long-run effects.\n\n",

    "Assume the event DOES occur (resolves YES). Assess the pharmaceutical ",
    "supply chain impact.\n\n",

    "Rules:\n",
    "- LARGE DISRUPTION: major production shutdowns, export bans, sanctions, ",
    "  war or severe instability in a key pharma-manufacturing nation, ",
    "  or loss of >5% of global API/generics capacity — AND impact plausible ",
    horizon_str, ".\n",
    "- SMALL DISRUPTION: minor trade friction, localised strikes, brief port ",
    "  closures, regulatory delays with modest pharma exposure — ", horizon_str, ".\n",
    "- NO DISRUPTION: event unrelated to pharma supply, or pharma impact ",
    "  would take longer than ", HORIZON_MONTHS, " months to materialise.\n",
    "- source_countries: countries whose MANUFACTURING or EXPORT of pharma ",
    "  inputs would be disrupted — NOT countries that would suffer shortages.\n",
    "- Use [] for source_countries when NO DISRUPTION.\n",
    "- risk_cluster: a snake_case name grouping this market with other markets ",
    "  that describe the SAME underlying geopolitical/economic scenario. ",
    "  Prefer standard names: ", clusters_hint, ". ",
    "  Create a new name (snake_case) only if none of these fit.\n\n",

    "Respond with ONLY a valid JSON object — no markdown, no commentary:\n",
    "{\n",
    '  "affects_pharma_supply": true or false,\n',
    '  "source_countries": ["Country Name", ...],\n',
    '  "disruption_level": "LARGE DISRUPTION" or "SMALL DISRUPTION" or "NO DISRUPTION",\n',
    '  "risk_cluster": "snake_case_cluster_name",\n',
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
      risk_cluster          = p$risk_cluster %||% NA_character_,
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
      risk_cluster          = NA_character_,
      reasoning             = "parse_error"
    )
  }

  tibble(
    market_id             = market_id,
    affects_pharma_supply = result$affects_pharma_supply,
    source_countries      = result$source_countries,
    disruption_level      = result$disruption_level,
    risk_cluster          = result$risk_cluster,
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
  cache <- if (file.exists(cache_file)) {
    readRDS(cache_file) |>
      # Guard against duplicates left by previous partial runs:
      # keep the most recent evaluation for each market_id
      arrange(desc(evaluated_at)) |>
      distinct(market_id, .keep_all = TRUE)
  } else tibble()

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

  updated <- bind_rows(cache, new_evals) |>
    arrange(desc(evaluated_at)) |>
    distinct(market_id, .keep_all = TRUE)
  saveRDS(updated, cache_file)
  message("Evaluation cache saved: ", nrow(updated), " total records → ", cache_file)

  updated
}

# =============================================================================
# Cluster normalisation
# =============================================================================

#' Batch-normalise cluster names across all pharma-relevant events via one LLM call.
#'
#' Per-event evaluation assigns cluster names guided by STANDARD_CLUSTERS, but
#' Mistral may still produce slight variations ("iran_tensions" vs "iran_hormuz").
#' This function sends all current cluster assignments + questions to the LLM in
#' one pass and asks for a canonical mapping.
#'
#' Should be run once per weekly pipeline, after evaluate_markets_with_cache().
#'
#' @param evals_df   Evaluations tibble
#' @param markets_df Markets tibble (provides question text for context)
#' @param cache_file Path to .rds cache to update in-place
#' @return Updated evals_df with normalised risk_cluster values
normalize_clusters <- function(evals_df, markets_df, cache_file = EVAL_CACHE_FILE) {

  # Only process pharma-relevant, disruption-tagged markets that have a cluster
  to_norm <- evals_df |>
    filter(isTRUE(affects_pharma_supply), disruption_level != "NO DISRUPTION") |>
    left_join(markets_df |> select(market_id, question), by = "market_id") |>
    mutate(current_cluster = if_else(!is.na(risk_cluster), risk_cluster, "unassigned"))

  if (nrow(to_norm) == 0) {
    message("normalize_clusters: no markets to normalise.")
    return(evals_df)
  }

  n_before <- n_distinct(to_norm$current_cluster)
  message(sprintf("normalize_clusters: %d pharma markets across %d clusters → calling LLM...",
                  nrow(to_norm), n_before))

  # Cap at 300 markets to stay within Mistral's context window
  if (nrow(to_norm) > 300) {
    message("  Truncating to 300 most recent for normalization pass.")
    to_norm <- to_norm |> slice_max(order_by = evaluated_at, n = 300)
  }

  # Build compact JSON lines for the prompt
  market_lines <- paste(
    sprintf('  {"id":"%s","cluster":"%s","q":"%s"}',
            to_norm$market_id,
            to_norm$current_cluster,
            substr(to_norm$question %||% "", 1, 100)),
    collapse = ",\n"
  )

  std_list <- paste(STANDARD_CLUSTERS, collapse=", ")

  prompt <- paste0(
    "Normalise cluster names for prediction market events related to pharmaceutical ",
    "supply chain risk.\n\n",
    "Standard cluster names (prefer these): ", std_list, "\n\n",
    "Rules:\n",
    "- Merge clusters describing the same underlying geopolitical/economic scenario\n",
    "- Use snake_case, no spaces, concise (e.g. 'iran_hormuz_tensions' not 'tensions_in_iran_and_around_hormuz')\n",
    "- Each market belongs to exactly ONE cluster\n",
    "- Do NOT over-split; prefer broader clusters over highly specific ones\n",
    "- If a market has cluster 'unassigned', assign it a suitable cluster\n\n",
    "Markets:\n[\n", market_lines, "\n]\n\n",
    "Return ONLY a flat JSON object — market_id as key, canonical cluster as value:\n",
    '{"market_id_1": "cluster_name", "market_id_2": "cluster_name"}'
  )

  raw      <- .call_ollama(prompt)
  json_str <- .extract_json(raw)

  if (is.null(json_str)) {
    message("normalize_clusters: failed to parse LLM response — no changes made.")
    return(evals_df)
  }

  mapping <- tryCatch(fromJSON(json_str), error = function(e) NULL)
  if (is.null(mapping) || length(mapping) == 0) {
    message("normalize_clusters: empty mapping returned — no changes made.")
    return(evals_df)
  }

  mapping_df <- tibble(
    market_id    = names(mapping),
    risk_cluster = as.character(unlist(mapping))
  )

  updated <- evals_df |>
    rows_update(mapping_df, by = "market_id", unmatched = "ignore")

  n_after <- n_distinct(updated$risk_cluster[!is.na(updated$risk_cluster)])
  message(sprintf("normalize_clusters: %d clusters → %d canonical clusters", n_before, n_after))

  saveRDS(updated, cache_file)
  updated
}

