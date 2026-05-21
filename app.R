# ============================================================
# pharma_risk_app/app.R
# Pharma Supply Chain Risk Monitor — Interactive World Map
# ============================================================
# Place this file at:  <project_root>/pharma_risk_app/app.R
# Data expected at:    <project_root>/data/
#
# Run from RStudio:    shiny::runApp("pharma_risk_app")
# Run from terminal:   Rscript -e "shiny::runApp('pharma_risk_app')"
#
# Required packages (run install_shiny_packages.R once first):
#   shiny, leaflet, dplyr, tidyr, stringr, sf,
#   rnaturalearth, rnaturalearthdata, DT
# ============================================================

library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(DT)

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && !is.na(x[1])) x else y

# ── 1. Country name normalisation ─────────────────────────────────────────────
# Maps LLM output names → Natural Earth `name` field values
COUNTRY_ALIASES <- c(
  "USA"="United States", "U.S."="United States", "U.S.A."="United States",
  "US"="United States",  "United States of America"="United States",
  "UK"="United Kingdom", "Great Britain"="United Kingdom",
  "Britain"="United Kingdom", "England"="United Kingdom",
  "Russia"="Russia",           "Russian Federation"="Russia",
  "China"="China",             "PRC"="China",
  "People's Republic of China"="China",
  "South Korea"="South Korea", "Republic of Korea"="South Korea",
  "North Korea"="North Korea", "Dem. Rep. Korea"="North Korea",
  "Czech Republic"="Czech Rep.", "Czechia"="Czech Rep.",
  "DR Congo"="Dem. Rep. Congo", "DRC"="Dem. Rep. Congo",
  "UAE"="United Arab Emirates",
  "Iran"="Iran", "Islamic Republic of Iran"="Iran",
  "Myanmar"="Myanmar", "Burma"="Myanmar",
  "Bosnia"="Bosnia and Herz.", "Bosnia and Herzegovina"="Bosnia and Herz.",
  "North Macedonia"="Macedonia", "Taiwan"="Taiwan",
  "Palestinian Territories"="Palestine", "West Bank"="Palestine"
)

norm_country <- function(x) {
  x <- str_trim(x)
  ifelse(x %in% names(COUNTRY_ALIASES), COUNTRY_ALIASES[x], x)
}

# ── 2. Load world polygons (once at startup) ───────────────────────────────────
world_sf <- ne_countries(scale = "medium", returnclass = "sf") |>
  select(name, iso_a3, geometry) |>
  st_transform(4326)

# ── 3. Fallback demo data (used when data/ directory is absent) ────────────────
DEMO_RISK <- tibble(
  country            = c("China","India","Germany","Switzerland",
                         "United States","Ireland","Japan","Singapore"),
  p_large_disruption = c(0.38,0.25,0.12,0.08,0.06,0.04,0.03,0.03),
  p_small_disruption = c(0.55,0.40,0.30,0.20,0.18,0.12,0.10,0.08),
  p_any_disruption   = c(0.70,0.52,0.38,0.26,0.22,0.14,0.12,0.10),
  composite_risk     = c(0.92,0.62,0.41,0.28,0.24,0.15,0.13,0.11),
  risk_tier          = c("HIGH","MEDIUM","MEDIUM","LOW","LOW","MINIMAL","MINIMAL","MINIMAL"),
  n_large            = c(6L,4L,2L,1L,1L,0L,0L,0L),
  n_small            = c(9L,7L,5L,3L,3L,2L,2L,1L),
  n_events           = c(15L,11L,7L,4L,4L,2L,2L,1L),
  top_large_events   = c(
    "Will China impose API export restrictions in 2025?",
    "Will India ban generic drug exports this year?",
    "Will Germany face energy rationing affecting pharma plants?",
    "Will Swiss franc controls affect pharma exports?",
    "Will US impose drug price controls cutting manufacturer margins?",
    NA_character_, NA_character_, NA_character_
  ),
  top_small_events = c(
    "Will China-US trade tensions worsen in Q1?",
    "Will Indian port strikes disrupt shipments?",
    rep(NA_character_, 6)
  )
)

DEMO_EVENTS <- tibble(
  market_id        = paste0("demo", 1:6),
  question         = c(
    "Will China impose API export restrictions in 2025?",
    "Will India ban generic drug exports this year?",
    "Will Germany face energy rationing affecting pharma plants?",
    "Will US-China trade war escalate to pharma tariffs?",
    "Will Switzerland face banking stress affecting pharma financing?",
    "Will Japan impose semiconductor-linked pharma export rules?"
  ),
  yes_prob         = c(0.38,0.25,0.12,0.45,0.08,0.05),
  disruption_level = c("LARGE DISRUPTION","LARGE DISRUPTION","LARGE DISRUPTION",
                       "SMALL DISRUPTION","SMALL DISRUPTION","SMALL DISRUPTION"),
  source_countries = c("China","India","Germany","China; United States",
                       "Switzerland","Japan"),
  reasoning = c(
    "China controls ~80% of global API production; export controls would be catastrophic.",
    "India supplies ~40% of global generic medicines; an export ban would cause immediate shortages.",
    "German pharma plants are highly energy-intensive; rationing could halt production.",
    "Escalating tariffs raise input costs and incentivise supply chain relocation.",
    "Banking stress could freeze capital expenditure for major pharma producers.",
    "Semiconductor controls could extend to pharmaceutical manufacturing equipment."
  ),
  volume = c(250000L,180000L,95000L,320000L,45000L,28000L)
)

# ── 4. Load real data with demo fallback ───────────────────────────────────────
# When launched via runApp("pharma_risk_app"), getwd() = project root
DATA_DIR <- "data"

load_app_data <- function() {
  risk_df <- tryCatch(
    readRDS(file.path(DATA_DIR, "risk_summary.rds")),
    error = function(e) { message("[app] data/risk_summary.rds not found — using demo data"); DEMO_RISK }
  )

  events_df <- tryCatch({
    m <- readRDS(file.path(DATA_DIR, "markets_cache.rds"))
    e <- readRDS(file.path(DATA_DIR, "evaluations_cache.rds")) |>
      arrange(desc(evaluated_at)) |>
      distinct(market_id, .keep_all = TRUE)
    m |>
      inner_join(e, by = "market_id") |>
      distinct(market_id, .keep_all = TRUE) |>
      filter(isTRUE(affects_pharma_supply), disruption_level != "NO DISRUPTION") |>
      select(market_id, question, yes_prob, disruption_level, source_countries, reasoning, volume) |>
      mutate(yes_prob = round(yes_prob, 4)) |>
      arrange(disruption_level, desc(yes_prob))
  }, error = function(e) { message("[app] Cache files not found — using demo events"); DEMO_EVENTS })

  list(risk = risk_df, events = events_df)
}

app_data  <- load_app_data()
risk_df   <- app_data$risk
events_df <- app_data$events

last_update <- tryCatch(
  format(file.info(file.path(DATA_DIR, "risk_summary.rds"))$mtime, "%d %b %Y %H:%M"),
  error = function(e) paste(format(Sys.Date(), "%d %b %Y"), "(demo)")
)

# ── 5. Attach risk data to world polygons ──────────────────────────────────────
risk_norm <- risk_df |>
  mutate(map_name = norm_country(country))

world_risk <- world_sf |>
  left_join(risk_norm, by = c("name" = "map_name")) |>
  mutate(
    p_large_disruption = replace_na(p_large_disruption, 0),
    p_small_disruption = replace_na(p_small_disruption, 0),
    p_any_disruption   = replace_na(p_any_disruption,   0),
    composite_risk     = replace_na(composite_risk,     0),
    n_events           = replace_na(n_events, 0L)
  )

# ── 6. Colour palette ──────────────────────────────────────────────────────────
RISK_RAMP <- colorRampPalette(
  c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272",
    "#FB6A4A","#EF3B2C","#CB181D","#67000D")
)(256)

risk_pal <- colorNumeric(palette = RISK_RAMP, domain = c(0,1), na.color = "#EBEBEB")

TIER_COLS <- c(HIGH="#67000D", MEDIUM="#EF3B2C", LOW="#FC9272", MINIMAL="#FCBBA1")

# ── 7. Pre-compute hover labels (drop geometry for speed) ──────────────────────
world_df <- world_risk |> st_drop_geometry()

make_label <- function(i) {
  r <- world_df[i, ]
  if (is.na(r$country) || r$n_events == 0) {
    return(sprintf(
      '<div style="font-family:\'DM Sans\',Arial,sans-serif;padding:2px">
         <b style="font-size:13px">%s</b><br>
         <span style="color:#999;font-size:11px">No pharma risk data</span>
       </div>', r$name
    ))
  }
  tc      <- TIER_COLS[r$risk_tier %||% "MINIMAL"] %||% "#FCBBA1"
  bar_pct <- round(r$p_large_disruption * 100)
  top_ev  <- if (!is.na(r$top_large_events) && nchar(r$top_large_events) > 2) {
    sprintf(
      '<div style="margin-top:6px;padding-top:6px;border-top:1px solid #eee;
               font-style:italic;color:#666;font-size:11px;line-height:1.4">
         &#8220;%s&#8221;
       </div>',
      substr(r$top_large_events, 1, 90)
    )
  } else ""

  sprintf(
    '<div style="font-family:\'DM Sans\',Arial,sans-serif;min-width:220px;padding:2px">
       <div style="display:flex;align-items:center;gap:6px;margin-bottom:6px">
         <b style="font-size:14px">%s</b>
         <span style="background:%s;color:#fff;padding:1px 8px;border-radius:10px;
                      font-size:10px;font-weight:700;letter-spacing:.3px">%s</span>
       </div>
       <div style="background:#f0f0f0;height:5px;border-radius:3px;margin-bottom:8px">
         <div style="background:%s;width:%d%%;height:5px;border-radius:3px"></div>
       </div>
       <table style="width:100%%;font-size:12px;border-collapse:collapse">
         <tr><td style="color:#555">Large disruption</td>
             <td align="right"><b style="color:#CB181D">%.1f%%</b></td></tr>
         <tr><td style="color:#555">Small disruption</td>
             <td align="right" style="color:#E65100">%.1f%%</td></tr>
         <tr><td style="color:#555">Any disruption</td>
             <td align="right">%.1f%%</td></tr>
         <tr><td style="color:#555">Events tracked</td>
             <td align="right" style="font-weight:600">%d</td></tr>
       </table>%s
     </div>',
    r$country, tc, r$risk_tier %||% "MINIMAL", tc, bar_pct,
    r$p_large_disruption * 100,
    r$p_small_disruption * 100,
    r$p_any_disruption   * 100,
    r$n_events, top_ev
  )
}

world_risk$label <- sapply(seq_len(nrow(world_df)), make_label)

# ── 8. UI ──────────────────────────────────────────────────────────────────────
APP_CSS <- "
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600;700&family=DM+Mono:wght@400;500&display=swap');

*, *::before, *::after { box-sizing: border-box; }
html, body { margin:0; padding:0; height:100%; overflow:hidden;
             font-family:'DM Sans',sans-serif; background:#0d1117; }

/* ── Header ── */
#app-header {
  height: 52px;
  background: linear-gradient(135deg, #0d1117 0%, #161b22 100%);
  border-bottom: 1px solid #21262d;
  display: flex; align-items: center; justify-content: space-between;
  padding: 0 18px; flex-shrink: 0;
}
.header-left { display:flex; align-items:center; gap:12px; }
.header-logo { font-size:20px; }
.header-title { font-size:15px; font-weight:700; color:#e6edf3; letter-spacing:-.2px; }
.header-sub   { font-size:11px; color:#6e7681; margin-top:1px; font-weight:400; }
.header-badge {
  background:#21262d; border:1px solid #30363d; border-radius:20px;
  padding:3px 10px; font-size:11px; color:#6e7681; font-family:'DM Mono',monospace;
}
.header-badge b { color:#58a6ff; }

/* ── Layout ── */
#app-body {
  display: flex; height: calc(100vh - 52px);
}

/* ── Sidebar ── */
#sidebar {
  width: 360px; flex-shrink: 0;
  background: #0d1117;
  border-right: 1px solid #21262d;
  display: flex; flex-direction: column;
  overflow: hidden;
}
#sidebar-top {
  padding: 14px 14px 10px;
  border-bottom: 1px solid #21262d;
  flex-shrink: 0;
}
.sidebar-title {
  font-size: 11px; font-weight: 700; color: #6e7681;
  letter-spacing: 1px; text-transform: uppercase; margin-bottom: 10px;
}
.filter-pills { display:flex; gap:6px; flex-wrap:wrap; }
.filter-pill {
  border: 1px solid #30363d; border-radius: 20px;
  padding: 3px 12px; font-size: 11px; font-weight: 600;
  cursor: pointer; transition: all .15s; background: transparent;
  color: #8b949e; font-family: 'DM Sans', sans-serif;
}
.filter-pill:hover { border-color: #6e7681; color: #e6edf3; }
.filter-pill.active-all   { background:#21262d; color:#e6edf3; border-color:#6e7681; }
.filter-pill.active-large { background:#67000D; color:#fff;    border-color:#CB181D; }
.filter-pill.active-small { background:#7d3c00; color:#fff;    border-color:#E65100; }
.filter-pill.clear-btn    { margin-left:auto; color:#6e7681; }
.filter-pill.clear-btn:hover { color:#f85149; border-color:#f85149; }

#event-list { flex:1; overflow-y:auto; padding:6px 0; }
#event-list::-webkit-scrollbar { width:4px; }
#event-list::-webkit-scrollbar-track { background:transparent; }
#event-list::-webkit-scrollbar-thumb { background:#30363d; border-radius:2px; }

.event-row {
  padding: 10px 14px; cursor: pointer;
  border-bottom: 1px solid #161b22;
  transition: background .1s;
}
.event-row:hover   { background: #161b22; }
.event-row.selected { background: #1c2128; border-left: 3px solid #58a6ff; padding-left: 11px; }
.event-q  { font-size:12px; font-weight:500; color:#c9d1d9; line-height:1.4; margin-bottom:5px; }
.event-meta { display:flex; align-items:center; gap:6px; }
.ev-badge {
  font-size:9px; font-weight:700; padding:1px 6px; border-radius:3px;
  letter-spacing:.3px; text-transform:uppercase;
}
.ev-large { background:#67000D22; color:#ff7b7b; border:1px solid #67000D55; }
.ev-small { background:#7d3c0022; color:#ffa94d; border:1px solid #7d3c0055; }
.ev-prob  { font-size:11px; font-weight:700; color:#58a6ff; font-family:'DM Mono',monospace; }
.ev-countries { font-size:10px; color:#6e7681; }
.ev-count { font-size:10px; color:#6e7681; margin-left:auto; }

/* ── Map container ── */
#map-container {
  flex:1; position:relative; overflow:hidden;
}

/* ── Country detail card ── */
#country-card {
  position: absolute; bottom: 24px; right: 16px; z-index: 800;
  width: 260px;
  background: rgba(13,17,23,0.95);
  border: 1px solid #30363d;
  border-radius: 10px;
  backdrop-filter: blur(12px);
  overflow: hidden;
  box-shadow: 0 8px 32px rgba(0,0,0,0.4);
  animation: slideUp .2s ease;
}
@keyframes slideUp {
  from { opacity:0; transform:translateY(8px); }
  to   { opacity:1; transform:translateY(0); }
}
.card-header {
  padding: 10px 14px 8px;
  border-bottom: 1px solid #21262d;
}
.card-country  { font-size:15px; font-weight:700; color:#e6edf3; }
.card-tier {
  display:inline-block; font-size:9px; font-weight:700;
  padding:1px 7px; border-radius:10px; margin-left:6px;
  letter-spacing:.5px; text-transform:uppercase;
}
.card-body { padding:10px 14px; }
.risk-bar-bg   { background:#21262d; height:5px; border-radius:3px; margin:6px 0 10px; }
.risk-bar-fill { height:5px; border-radius:3px; transition:width .4s ease; }
.stat-row { display:flex; justify-content:space-between; align-items:center;
            padding:3px 0; font-size:12px; }
.stat-label { color:#6e7681; }
.stat-val   { font-weight:600; font-family:'DM Mono',monospace; font-size:12px; }
.stat-large { color:#ff7b7b; }
.stat-small { color:#ffa94d; }
.stat-any   { color:#58a6ff; }
.card-event {
  margin-top:8px; padding:8px; background:#161b22; border-radius:6px;
  font-size:11px; color:#8b949e; font-style:italic; line-height:1.5;
  border-left:2px solid #30363d;
}
.card-hint  { font-size:10px; color:#30363d; text-align:right;
              padding:6px 14px; border-top:1px solid #161b22; }

/* ── Leaflet overrides ── */
.leaflet-container { background:#0d1117 !important; }
.leaflet-control-attribution { background:rgba(13,17,23,.7) !important; color:#6e7681 !important; }
.leaflet-control-attribution a { color:#58a6ff !important; }
.leaflet-control-zoom a { background:#161b22 !important; color:#e6edf3 !important;
                           border-color:#30363d !important; }
.leaflet-control-zoom a:hover { background:#21262d !important; }
.info.legend { background:rgba(13,17,23,.92) !important; color:#c9d1d9 !important;
               border:1px solid #30363d !important; border-radius:8px !important;
               padding:10px 12px !important; font-family:'DM Sans',sans-serif !important; }

/* ── Selection highlight info bar ── */
#event-info-bar {
  position: absolute; top: 10px; left: 50%; transform: translateX(-50%);
  z-index: 800; background: rgba(13,17,23,.92); border:1px solid #30363d;
  border-radius:20px; padding:6px 16px; font-size:12px; color:#e6edf3;
  backdrop-filter:blur(8px); white-space:nowrap;
  box-shadow:0 4px 16px rgba(0,0,0,0.3);
  display:none;
}
#event-info-bar.visible { display:block; animation:fadeIn .2s ease; }
@keyframes fadeIn { from{opacity:0} to{opacity:1} }
"

ui <- fluidPage(
  tags$head(
    tags$style(HTML(APP_CSS)),
    tags$link(rel="icon", type="image/png",
              href="data:image/png;base64,iVBORw0KGgo=")
  ),

  # ── Header ────────────────────────────────────────────────
  tags$div(id="app-header",
    tags$div(class="header-left",
      tags$div(class="header-logo", "🧬"),
      tags$div(
        tags$div(class="header-title", "Pharma Supply Chain Risk Monitor"),
        tags$div(class="header-sub",
                 "Polymarket prediction markets · Mistral LLM classification")
      )
    ),
    tags$div(class="header-badge",
      HTML(paste0("Last updated: <b>", last_update, "</b>"))
    )
  ),

  # ── Body ──────────────────────────────────────────────────
  tags$div(id="app-body",

    # ── Left sidebar ────────────────────────────────────────
    tags$div(id="sidebar",
      tags$div(id="sidebar-top",
        tags$div(class="sidebar-title",
          textOutput("event_count_label", inline=TRUE)
        ),
        tags$div(class="filter-pills",
          actionButton("filter_all",   "All",         class="filter-pill active-all"),
          actionButton("filter_large", "⬤ Large",     class="filter-pill"),
          actionButton("filter_small", "◉ Small",     class="filter-pill"),
          actionButton("clear_sel",    "✕ Clear",     class="filter-pill clear-btn")
        )
      ),
      tags$div(id="event-list",
        uiOutput("event_rows")
      )
    ),

    # ── Map panel ──────────────────────────────────────────
    tags$div(id="map-container",

      leafletOutput("world_map", width="100%", height="100%"),

      # Floating event label (shown when event selected)
      tags$div(id="event-info-bar",
        uiOutput("event_info_text")
      ),

      # Country detail card
      conditionalPanel(
        condition = "output.show_country_card",
        tags$div(id="country-card",
          uiOutput("country_detail_card"),
          tags$div(class="card-hint", "click map to dismiss")
        )
      )
    )
  ),

  # JS for event row clicks & pill state
  tags$script(HTML("
    Shiny.addCustomMessageHandler('highlight_event_bar', function(msg) {
      var bar = document.getElementById('event-info-bar');
      if (msg.visible) {
        bar.classList.add('visible');
      } else {
        bar.classList.remove('visible');
      }
    });

    Shiny.addCustomMessageHandler('update_pills', function(msg) {
      ['filter_all','filter_large','filter_small'].forEach(function(id) {
        var el = document.getElementById(id);
        el.className = 'filter-pill';
        if (id === 'filter_' + msg.active) {
          if (msg.active === 'all')   el.classList.add('active-all');
          if (msg.active === 'large') el.classList.add('active-large');
          if (msg.active === 'small') el.classList.add('active-small');
        }
      });
    });
  "))
)

# ── 9. Server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    filter         = "all",
    selected_event = NULL,   # market_id of selected event
    clicked_country = NULL   # data frame row for clicked country
  )

  # ── Filter buttons ──────────────────────────────────────────────────────────
  observeEvent(input$filter_all, {
    rv$filter <- "all"; rv$selected_event <- NULL
    session$sendCustomMessage("update_pills", list(active="all"))
    session$sendCustomMessage("highlight_event_bar", list(visible=FALSE))
    leafletProxy("world_map") |> clearGroup("highlight")
  })
  observeEvent(input$filter_large, {
    rv$filter <- "large"; rv$selected_event <- NULL
    session$sendCustomMessage("update_pills", list(active="large"))
    session$sendCustomMessage("highlight_event_bar", list(visible=FALSE))
    leafletProxy("world_map") |> clearGroup("highlight")
  })
  observeEvent(input$filter_small, {
    rv$filter <- "small"; rv$selected_event <- NULL
    session$sendCustomMessage("update_pills", list(active="small"))
    session$sendCustomMessage("highlight_event_bar", list(visible=FALSE))
    leafletProxy("world_map") |> clearGroup("highlight")
  })
  observeEvent(input$clear_sel, {
    rv$selected_event   <- NULL
    rv$clicked_country  <- NULL
    session$sendCustomMessage("highlight_event_bar", list(visible=FALSE))
    leafletProxy("world_map") |> clearGroup("highlight")
  })

  # ── Filtered event list ──────────────────────────────────────────────────────
  filtered_events <- reactive({
    df <- events_df
    if (rv$filter == "large") df <- filter(df, disruption_level == "LARGE DISRUPTION")
    if (rv$filter == "small") df <- filter(df, disruption_level == "SMALL DISRUPTION")
    df
  })

  output$event_count_label <- renderText({
    sprintf("%d Prediction Markets", nrow(filtered_events()))
  })

  # ── Render event rows as custom HTML ────────────────────────────────────────
  output$event_rows <- renderUI({
    df  <- filtered_events()
    sel <- rv$selected_event

    rows <- lapply(seq_len(nrow(df)), function(i) {
      ev      <- df[i, ]
      is_sel  <- !is.null(sel) && sel == ev$market_id
      is_lrg  <- ev$disruption_level == "LARGE DISRUPTION"
      row_cls <- paste("event-row", if (is_sel) "selected" else "")
      badge   <- if (is_lrg)
        '<span class="ev-badge ev-large">Large</span>'
      else
        '<span class="ev-badge ev-small">Small</span>'

      tags$div(class=row_cls,
        id=paste0("erow_", ev$market_id),
        onclick=sprintf("Shiny.setInputValue('event_click','%s',{priority:'event'})", ev$market_id),
        tags$div(class="event-q",
          substr(ev$question, 1, 80),
          if (nchar(ev$question) > 80) "…" else ""
        ),
        tags$div(class="event-meta",
          HTML(badge),
          tags$span(class="ev-prob", sprintf("%.0f%%", ev$yes_prob * 100)),
          tags$span(class="ev-countries",
            substr(ev$source_countries, 1, 35),
            if (nchar(ev$source_countries) > 35) "…" else ""
          )
        )
      )
    })
    tagList(rows)
  })

  # ── Event click → highlight countries on map ────────────────────────────────
  observeEvent(input$event_click, {
    mid <- input$event_click
    rv$selected_event  <- mid
    rv$clicked_country <- NULL

    ev <- events_df |> filter(market_id == mid)
    if (nrow(ev) == 0) return()

    affected <- str_split(ev$source_countries[1], ";\\s*")[[1]] |>
      str_trim() |>
      sapply(norm_country, USE.NAMES=FALSE)

    affected_sf <- world_risk |> filter(name %in% affected)

    is_large <- ev$disruption_level[1] == "LARGE DISRUPTION"
    fill_col  <- if (is_large) "#FFD700" else "#FFF176"
    line_col  <- if (is_large) "#FF6D00" else "#FFC107"

    proxy <- leafletProxy("world_map") |>
      clearGroup("highlight") |>
      addPolygons(
        data         = affected_sf,
        group        = "highlight",
        fillColor    = fill_col,
        fillOpacity  = 0.40,
        color        = line_col,
        weight       = 2.5,
        opacity      = 1,
        label        = lapply(affected_sf$label, HTML),
        labelOptions = labelOptions(
          style       = list("background"="rgba(13,17,23,.95)",
                             "color"="#e6edf3",
                             "border"="1px solid #30363d",
                             "border-radius"="8px",
                             "padding"="10px 12px",
                             "box-shadow"="0 4px 20px rgba(0,0,0,.4)",
                             "font-family"="'DM Sans',sans-serif"),
          textsize    = "13px",
          direction   = "auto"
        )
      )

    # Fly to single-country events
    if (nrow(affected_sf) == 1) {
      bbox <- st_bbox(affected_sf)
      proxy |> fitBounds(bbox[["xmin"]], bbox[["ymin"]],
                         bbox[["xmax"]], bbox[["ymax"]])
    } else if (nrow(affected_sf) > 1) {
      bbox <- st_bbox(affected_sf)
      proxy |> fitBounds(bbox[["xmin"]], bbox[["ymin"]],
                         bbox[["xmax"]], bbox[["ymax"]])
    }

    session$sendCustomMessage("highlight_event_bar", list(visible=TRUE))
  })

  # ── Event info bar text ──────────────────────────────────────────────────────
  output$event_info_text <- renderUI({
    mid <- rv$selected_event
    req(!is.null(mid))
    ev <- events_df |> filter(market_id == mid)
    req(nrow(ev) > 0)
    is_large <- ev$disruption_level[1] == "LARGE DISRUPTION"
    col <- if (is_large) "#ff7b7b" else "#ffa94d"
    HTML(sprintf(
      '<span style="color:%s;font-weight:700">%s</span>
       &nbsp;·&nbsp; %s
       &nbsp;·&nbsp; <span style="color:#58a6ff;font-family:\'DM Mono\',monospace">%.0f%% P(YES)</span>',
      col,
      if (is_large) "⬤ LARGE DISRUPTION" else "◉ SMALL DISRUPTION",
      substr(ev$question[1], 1, 70),
      ev$yes_prob[1] * 100
    ))
  })

  # ── Base map (rendered once) ─────────────────────────────────────────────────
  output$world_map <- renderLeaflet({
    leaflet(world_risk,
            options = leafletOptions(minZoom=2, maxZoom=8,
                                     zoomControl=TRUE, scrollWheelZoom=TRUE)) |>
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels,
                       options = tileOptions(opacity = 0.8)) |>
      setView(lng=15, lat=20, zoom=2) |>
      setMaxBounds(-200,-85,200,85) |>
      addPolygons(
        group       = "choropleth",
        layerId     = ~name,
        fillColor   = ~risk_pal(p_large_disruption),
        fillOpacity = 0.80,
        color       = "#1a1a2e",
        weight      = 0.6,
        opacity     = 0.9,
        smoothFactor = 0.8,
        label        = lapply(world_risk$label, HTML),
        labelOptions = labelOptions(
          style    = list("background"="rgba(13,17,23,.95)",
                          "color"="#e6edf3",
                          "border"="1px solid #30363d",
                          "border-radius"="8px",
                          "padding"="10px 12px",
                          "box-shadow"="0 4px 20px rgba(0,0,0,.4)",
                          "font-family"="'DM Sans',sans-serif"),
          textsize    = "13px",
          direction   = "auto"
        ),
        highlightOptions = highlightOptions(
          fillOpacity  = 0.95,
          color        = "#58a6ff",
          weight       = 1.5,
          bringToFront = FALSE
        )
      ) |>
      addLegend(
        position  = "bottomleft",
        pal       = risk_pal,
        values    = ~p_large_disruption,
        title     = "Large Disruption<br>Probability",
        labFormat = labelFormat(suffix="%", transform=function(x) round(x*100)),
        opacity   = 0.9
      )
  })

  # ── Country polygon click → detail card ─────────────────────────────────────
  observeEvent(input$world_map_shape_click, {
    click <- input$world_map_shape_click
    if (is.null(click)) return()
    if (click$group != "choropleth") return()

    hit <- world_df |> filter(name == click$id)
    if (nrow(hit) == 0 || is.na(hit$country[1])) {
      rv$clicked_country <- NULL
      return()
    }
    rv$clicked_country <- hit[1, ]
  })

  output$show_country_card <- reactive({
    !is.null(rv$clicked_country)
  })
  outputOptions(output, "show_country_card", suspendWhenHidden=FALSE)

  # ── Country detail card UI ────────────────────────────────────────────────────
  output$country_detail_card <- renderUI({
    r <- rv$clicked_country
    req(!is.null(r))

    tc      <- TIER_COLS[r$risk_tier %||% "MINIMAL"] %||% "#FCBBA1"
    bar_pct <- round(r$p_large_disruption * 100)

    top_ev <- if (!is.na(r$top_large_events) && nchar(r$top_large_events) > 2)
      tags$div(class="card-event",
        paste0("\u201C", substr(r$top_large_events, 1, 100), "\u2026\u201D"))
    else NULL

    tagList(
      tags$div(class="card-header",
        tags$span(class="card-country", r$country),
        tags$span(class="card-tier",
                  style=sprintf("background:%s22;color:%s;border:1px solid %s55",tc,tc,tc),
                  r$risk_tier %||% "MINIMAL")
      ),
      tags$div(class="card-body",
        tags$div(class="risk-bar-bg",
          tags$div(class="risk-bar-fill",
                   style=sprintf("background:%s;width:%d%%", tc, bar_pct))
        ),
        tags$div(class="stat-row",
          tags$span(class="stat-label", "Large disruption"),
          tags$span(class="stat-val stat-large",
                    sprintf("%.1f%%", r$p_large_disruption * 100))
        ),
        tags$div(class="stat-row",
          tags$span(class="stat-label", "Small disruption"),
          tags$span(class="stat-val stat-small",
                    sprintf("%.1f%%", r$p_small_disruption * 100))
        ),
        tags$div(class="stat-row",
          tags$span(class="stat-label", "Any disruption"),
          tags$span(class="stat-val stat-any",
                    sprintf("%.1f%%", r$p_any_disruption * 100))
        ),
        tags$div(class="stat-row",
          tags$span(class="stat-label", "Markets tracked"),
          tags$span(class="stat-val", r$n_events %||% 0)
        ),
        top_ev
      )
    )
  })
}

shinyApp(ui, server)
