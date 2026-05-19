# =============================================================================
# install_packages.R — Run once to install all required packages
# =============================================================================

required <- c(
  "httr2",      # HTTP client (replaces httr)
  "jsonlite",   # JSON parsing
  "dplyr",      # Data manipulation
  "tidyr",      # Pivoting / unnesting
  "purrr",      # Functional tools (map, safely, etc.)
  "tibble",     # Modern data frames
  "stringr",    # String helpers
  "readr"       # CSV writing
)

missing <- required[!required %in% rownames(installed.packages())]

if (length(missing) > 0) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All packages already installed.")
}

# Verify
lapply(required, library, character.only = TRUE)
message("All packages loaded successfully.")
