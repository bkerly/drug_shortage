# Pharma Supply Chain Risk Monitor

Pulls all active Polymarket prediction markets, classifies each for pharmaceutical
supply chain relevance using Mistral (via Ollama), and aggregates per-country
disruption probabilities assuming event independence.

---

## Prerequisites

| Tool | Notes |
|---|---|
| R ≥ 4.2 | |
| [Ollama](https://ollama.com) | Running locally on port 11434 |
| Mistral model | `ollama pull mistral` |

```bash
# First-time setup
Rscript install_packages.R
```

---

## Usage

### Weekly full run (Monday morning)
```bash
Rscript run_weekly.R
# or in RStudio: source("run_weekly.R"); results <- run_weekly()
```
- Fetches all active Polymarket markets (~1,000–2,000)  
- Classifies **new** markets with Mistral (cached markets skipped)  
- Saves `data/risk_weekly_YYYYMMDD.csv` and `data/market_detail_YYYYMMDD.csv`  
- Runtime: ~30–90 min depending on how many new markets need LLM evaluation

### Daily large-disruption refresh
```bash
Rscript run_daily.R
```
- Re-fetches **only** current prices for LARGE DISRUPTION markets  
- No new LLM calls — probabilities update, classifications don't  
- Saves `data/risk_daily_YYYYMMDD.csv`  
- Runtime: 1–3 min

### Suggested cron schedule
```cron
# Daily refresh at 07:00
0 7 * * *  Rscript /path/to/pharma_risk/run_daily.R >> /var/log/pharma_risk_daily.log 2>&1

# Full weekly run at 06:00 on Monday
0 6 * * 1  Rscript /path/to/pharma_risk/run_weekly.R >> /var/log/pharma_risk_weekly.log 2>&1
```

---

## Output schema

### `risk_summary.rds` / `risk_weekly_*.csv`

| Column | Description |
|---|---|
| `country` | Pharma source/manufacturing nation |
| `n_events` | # prediction markets affecting this country |
| `n_large` / `n_small` | Count by disruption tier |
| `p_large_disruption` | P(≥1 large disruption) assuming independence |
| `p_small_disruption` | P(≥1 small disruption) assuming independence |
| `p_any_disruption` | P(≥1 disruption of any kind) |
| `composite_risk` | Weighted index: Σ pᵢ × wᵢ (w=1.0 large, 0.3 small) |
| `risk_tier` | HIGH / MEDIUM / LOW / MINIMAL |
| `top_large_events` | Top 3 driving large-disruption questions |

### `market_detail_*.csv`

One row per Polymarket market with all LLM classifications attached.

| Column | Description |
|---|---|
| `market_id` | Polymarket condition ID |
| `question` | Market question text |
| `yes_prob` | Current Polymarket implied probability |
| `affects_pharma_supply` | TRUE/FALSE (LLM) |
| `source_countries` | Semicolon-separated country names (LLM) |
| `disruption_level` | LARGE / SMALL / NO DISRUPTION (LLM) |
| `reasoning` | One-sentence LLM explanation |

---

## Caching strategy

```
data/
├── markets_cache.rds     ← full market list, updated every run
├── evaluations_cache.rds ← LLM classifications, keyed on market_id
│                            (never re-evaluated unless you delete this)
├── risk_summary.rds      ← latest aggregated risk, overwritten each run
├── risk_weekly_*.csv     ← dated snapshots (keep for trend analysis)
├── risk_daily_*.csv      ← dated daily refreshes
└── market_detail_*.csv   ← full detail, one file per week
```

To force re-evaluation of all markets (e.g. after a prompt update):
```r
file.remove("data/evaluations_cache.rds")
run_weekly()
```

---

## Independence assumption

Risk is aggregated as:

```
P(at least one disruption) = 1 - ∏(1 - pᵢ)
```

This is an upper bound — correlated events (e.g. US-China trade conflict
simultaneously affecting multiple markets) will be slightly overcounted.
For a correlation-adjusted model, consider a copula approach or simply note
that the composite_risk_score (additive) is the more conservative figure.

---

## Adjusting the LLM prompt

The prompt is in `R/evaluate_llm.R` in the `.build_prompt()` function.
Key tuning levers:
- `PHARMA_SOURCE_NATIONS` list (add/remove countries)
- LARGE vs SMALL guidance text
- `temperature = 0.05` (raise to ~0.2 for more varied classifications)

After changing the prompt, delete `data/evaluations_cache.rds` and rerun.
