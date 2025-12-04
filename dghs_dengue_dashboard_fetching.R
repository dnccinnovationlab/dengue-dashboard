library(httr)
library(stringr)
library(jsonlite)
library(dplyr)
library(readr)

# ---------------------------
# Helper function to extract chart block
# ---------------------------
extract_chart_block <- function(html, chart_id) {
  pattern <- paste0("Highcharts\\.chart\\s*\\(\\s*['\"]", chart_id, "['\"]([\\s\\S]*?)\\}\\s*\\)")
  block <- str_extract(html, pattern)
  return(block)
}

# ---------------------------
# Helper to extract data array
# ---------------------------
extract_series_data <- function(block) {
  if (is.na(block)) return(NA)
  
  data_str <- str_extract(block, "data:\\s*\\[[^\\]]*\\]")
  if (is.na(data_str)) return(NA)
  
  data_json <- gsub("data:", "\"data\":", data_str)
  data_json <- paste0("{", data_json, "}")

  as.numeric(fromJSON(data_json)$data)
}

# ---------------------------
# Helper to extract category names
# ---------------------------
extract_categories <- function(block) {
  if (is.na(block)) return(NA)
  
  cats_str <- str_extract(block, "categories:\\s*\\[[^\\]]*\\]")
  if (is.na(cats_str)) return(NA)
  
  cats_json <- gsub("categories:", "\"categories\":", cats_str)
  cats_json <- paste0("{", cats_json, "}")

  fromJSON(cats_json)$categories
}

# ---------------------------
# 1. Download dashboard page
# ---------------------------
url <- "https://dashboard.dghs.gov.bd/pages/heoc_dengue_v1.php"

res <- GET(
  url,
  add_headers(
    `User-Agent` = "Mozilla/5.0",
    `Accept` = "*/*",
    `Referer` = "https://dashboard.dghs.gov.bd/"
  )
)

raw <- content(res, "text", encoding = "UTF-8")

# ---------------------------
# 2. Extract each chart block
# ---------------------------
block_affected  <- extract_chart_block(raw, "affected_case_last_24_hour")
block_death     <- extract_chart_block(raw, "death_case_last_24_hour")
block_city_case <- extract_chart_block(raw, "div_city_cor_case_last_24_hour")
block_city_death<- extract_chart_block(raw, "div_city_cor_death_last_24_hour")

# ---------------------------
# 3. Extract simple values (affected, death)
# ---------------------------
affected_val <- extract_series_data(block_affected)
death_val    <- extract_series_data(block_death)

# ---------------------------
# 4. Extract categories & data for DNCC/DSCC extraction
# ---------------------------
categories_case  <- extract_categories(block_city_case)
values_case      <- extract_series_data(block_city_case)

categories_death <- extract_categories(block_city_death)
values_death     <- extract_series_data(block_city_death)

# ---------------------------
# 5. Get DNCC / DSCC values
# ---------------------------

# Case values
dncc_case <- if ("DNCC" %in% categories_case) {
  values_case[which(categories_case == "DNCC")]
} else 0

dscc_case <- if ("DSCC" %in% categories_case) {
  values_case[which(categories_case == "DSCC")]
} else 0

# Death values
dncc_death <- if ("DNCC" %in% categories_death) {
  values_death[which(categories_death == "DNCC")]
} else 0

dscc_death <- if ("DSCC" %in% categories_death) {
  values_death[which(categories_death == "DSCC")]
} else 0

# ---------------------------
# 6. Build final database row
# ---------------------------
final_row <- tibble(
  date = Sys.Date(),
  affected = affected_val %||% 0,
  death = death_val %||% 0,
  dncc_case = dncc_case %||% 0,
  dncc_death = dncc_death %||% 0,
  dscc_case = dscc_case %||% 0,
  dscc_death = dscc_death %||% 0,
  dhaka_case = dncc_case + dscc_case,
  dhaka_death = dncc_death + dscc_death
)

print(final_row)

# Append to CSV inside repo
csv_path <- "dengue-dashboard/daily_dengue_dghs.csv"

existing <- read_csv(csv_path, show_col_types = FALSE)
updated <- bind_rows(existing, final_row)

write_csv(updated, csv_path)



