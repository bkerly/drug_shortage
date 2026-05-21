# install_shiny_packages.R — run once to install Shiny app dependencies

required <- c(
  "shiny",
  "leaflet",
  "dplyr",
  "tidyr",
  "stringr",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "DT"
)

missing <- required[!required %in% rownames(installed.packages())]

if (length(missing) > 0) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All packages already installed.")
}

# Verify all load cleanly
invisible(lapply(required, library, character.only = TRUE))
message("\nAll Shiny app packages ready.")
message("\nTo launch the app:")
message("  shiny::runApp('pharma_risk_app')")
