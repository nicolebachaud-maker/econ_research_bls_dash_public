####################################################################################
##
## BLS metrics for dashboard 
##
## Written by Nicole Bachaud 07/30/2026
##
####################################################################################

rm(list = ls())
library("pacman")

pacman::p_load(httr, jsonlite, dplyr, purrr, DBI, RSQLite, tibble, tidyr)

dirData <- "data/"
dirRun  <- "run/"

dir.create(dirData, recursive = TRUE, showWarnings = FALSE)
dir.create(dirRun,  recursive = TRUE, showWarnings = FALSE)

# ==========================================
# 1. SETUP & SERIES MAPPING DICTIONARY
# ==========================================

BLS_API_KEY <- Sys.getenv("BLS_API_KEY")

# ------------------------------------------
# Helper 1: Generate JOLTS Industry Metrics
# ------------------------------------------
gen_jolts_industry <- function(ind_code, ind_name) {
  metrics <- tribble(
    ~suffix, ~type, ~measure,
    "000000000JOL", "Job Openings Level", "JOLTS - Openings",
    "000000000JOR", "Job Openings Rate", "JOLTS - Openings",
    "000000000HIL", "Hires Level", "JOLTS - Hires",
    "000000000HIR", "Hires Rate", "JOLTS - Hires",
    "000000000QUL", "Quits Level", "JOLTS - Quits",
    "000000000QUR", "Quits Rate", "JOLTS - Quits",
    "000000000LDL", "Layoffs & Discharges Level", "JOLTS - Layoffs",
    "000000000LDR", "Layoffs & Discharges Rate", "JOLTS - Layoffs",
    "000000000OSL", "Other Separations Level", "JOLTS - Other Sep",
    "000000000OSR", "Other Separations Rate", "JOLTS - Other Sep",
    "000000000TSL", "Total Separations Level", "JOLTS - Total Sep",
    "000000000TSR", "Total Separations Rate", "JOLTS - Total Sep"
  )
  
  metrics %>%
    mutate(
      series_id = paste0("JTS", ind_code, suffix),
      series_name = paste0(ind_name, ": ", type),
      category = measure
    ) %>%
    select(series_id, series_name, category)
}

# Harmonized Industry List (Aligned with CES NAICS Codes)
jolts_industries <- list(
  c("000000", "Total Nonfarm"),
  c("100000", "Mining & Logging"),
  c("230000", "Construction"),
  c("300000", "Manufacturing"),
  c("400000", "Trade, Transp & Util"),
  c("420000", "Wholesale Trade"),
  c("440000", "Retail Trade"),
  c("480000", "Transportation, Warehousing & Utilities"),
  c("510000", "Information"),
  c("510099", "Financial Activities"),
  c("520000", "Finance & Insurance"),
  c("530000", "Real Estate & Rental"),
  c("540099", "Prof & Business Services"),
  c("600000", "Edu & Health Services"),
  c("610000", "Educational Services"),
  c("620000", "Health Care & Social Assist"),
  c("700000", "Leisure & Hospitality"),
  c("810000", "Other Services"),
  c("900000", "Government"),
  c("910000", "Federal Government"),
  c("920000", "State & Local Government"),
  c("923000", "State & Local Government Education"),
  c("929000", "State & Local Government, Excl Education")
)
jolts_industry_lookup <- map_dfr(jolts_industries, ~gen_jolts_industry(.x[1], .x[2]))

# ------------------------------------------
# Helper 2: Generate JOLTS Size Class Metrics (Exact BLS 21-Char Format)
# ------------------------------------------
gen_jolts_size <- function(size_code, size_name) {
  metrics <- tribble(
    ~element, ~type_code, ~type_name, ~measure,
    "JO", "L", "Job Openings Level", "JOLTS Size Class - Openings",
    "JO", "R", "Job Openings Rate",  "JOLTS Size Class - Openings",
    "HI", "L", "Hires Level",        "JOLTS Size Class - Hires",
    "HI", "R", "Hires Rate",         "JOLTS Size Class - Hires",
    "QU", "L", "Quits Level",        "JOLTS Size Class - Quits",
    "QU", "R", "Quits Rate",         "JOLTS Size Class - Quits",
    "LD", "L", "Layoffs & Discharges Level", "JOLTS Size Class - Layoffs",
    "LD", "R", "Layoffs & Discharges Rate",  "JOLTS Size Class - Layoffs",
    "OS", "L", "Other Separations Level",     "JOLTS Size Class - Other Sep",
    "OS", "R", "Other Separations Rate",     "JOLTS Size Class - Other Sep",
    "TS", "L", "Total Separations Level",     "JOLTS Size Class - Total Sep",
    "TS", "R", "Total Separations Rate",     "JOLTS Size Class - Total Sep"
  )
  
  metrics %>%
    mutate(
      series_id   = paste0("JTS1000000000000", size_code, element, type_code),
      series_name = paste0("Size ", size_name, ": ", type_name),
      category    = measure
    ) %>%
    select(series_id, series_name, category)
}

jolts_sizes <- list(
  c("01", "1-9"),
  c("02", "10-49"),
  c("03", "50-249"),
  c("04", "250-999"),
  c("05", "1000-4999"),
  c("06", "5000+")
)

jolts_size_lookup <- map_dfr(jolts_sizes, ~gen_jolts_size(.x[1], .x[2]))

# ------------------------------------------
# Non-JOLTS & CES Size Class Lookup Table
# ------------------------------------------
base_lookup <- tibble::tribble(
  ~series_id, ~series_name, ~category,
  
  # --- CES: Employment (Totals, Detailed NAICS, & Government Breakouts) ---
  "CES0000000001", "Total Nonfarm: Employment Level", "CES - Employment",
  "CES1000000001", "Mining & Logging: Employment Level", "CES - Employment",
  "CES2000000001", "Construction: Employment Level", "CES - Employment",
  "CES3000000001", "Manufacturing: Employment Level", "CES - Employment",
  "CES4000000001", "Trade, Transp & Util: Employment Level", "CES - Employment",
  "CES4142000001", "Wholesale Trade: Employment Level", "CES - Employment",
  "CES4200000001", "Retail Trade: Employment Level", "CES - Employment",
  "CES4300000001", "Transportation & Warehousing: Employment Level", "CES - Employment",
  "CES4348400001", "Truck Transportation: Employment Level", "CES - Employment",
  "CES4422000001", "Utilities: Employment Level", "CES - Employment",
  "CES5000000001", "Information: Employment Level", "CES - Employment",
  "CES5500000001", "Financial Activities: Employment Level", "CES - Employment",
  "CES5200000001", "Finance & Insurance: Employment Level", "CES - Employment",
  "CES5300000001", "Real Estate & Rental: Employment Level", "CES - Employment",
  "CES6000000001", "Prof & Business Services: Employment Level", "CES - Employment",
  "CES6500000001", "Edu & Health Services: Employment Level", "CES - Employment",
  "CES6561000001", "Educational Services: Employment Level", "CES - Employment",
  "CES6562000001", "Health Care & Social Assist: Employment Level", "CES - Employment",
  "CES7000000001", "Leisure & Hospitality: Employment Level", "CES - Employment",
  "CES8000000001", "Other Services: Employment Level", "CES - Employment",
  "CES9000000001", "Government: Employment Level", "CES - Employment",
  "CES9091000001", "Federal Government: Employment Level", "CES - Employment",
  "CES9092000001", "State Government: Employment Level", "CES - Employment",
  "CES9093000001", "Local Government: Employment Level", "CES - Employment",
  "CES9092161101", "State Government Education: Employment Level", "CES - Employment",
  "CES9093161101", "Local Government Education: Employment Level", "CES - Employment",
  
  # --- CES: Average Hourly Earnings (Industry Wages) ---
  "CES0500000003", "Total Private: Average Hourly Earnings", "CES - Earnings",
  "CES1000000003", "Mining & Logging: Average Hourly Earnings", "CES - Earnings",
  "CES2000000003", "Construction: Average Hourly Earnings", "CES - Earnings",
  "CES3000000003", "Manufacturing: Average Hourly Earnings", "CES - Earnings",
  "CES4000000003", "Trade, Transp & Util: Average Hourly Earnings", "CES - Earnings",
  "CES4142000003", "Wholesale Trade: Average Hourly Earnings", "CES - Earnings",
  "CES4200000003", "Retail Trade: Average Hourly Earnings", "CES - Earnings",
  "CES4300000003", "Transportation & Warehousing: Average Hourly Earnings", "CES - Earnings",
  "CES4348400003", "Truck Transportation: Average Hourly Earnings", "CES - Earnings",
  "CES4422000003", "Utilities: Average Hourly Earnings", "CES - Earnings",
  "CES5000000003", "Information: Average Hourly Earnings", "CES - Earnings",
  "CES5500000003", "Financial Activities: Average Hourly Earnings", "CES - Earnings",
  "CES5200000003", "Finance & Insurance: Average Hourly Earnings", "CES - Earnings",
  "CES5300000003", "Real Estate & Rental: Average Hourly Earnings", "CES - Earnings",
  "CES6000000003", "Prof & Business Services: Average Hourly Earnings", "CES - Earnings",
  "CES6500000003", "Edu & Health Services: Average Hourly Earnings", "CES - Earnings",
  "CES6561000003", "Educational Services: Average Hourly Earnings", "CES - Earnings",
  "CES6562000003", "Health Care & Social Assist: Average Hourly Earnings", "CES - Earnings",
  "CES7000000003", "Leisure & Hospitality: Average Hourly Earnings", "CES - Earnings",
  "CES8000000003", "Other Services: Average Hourly Earnings", "CES - Earnings",
  
  # --- LNS / LNU: Household Survey (CPS) & Demographics ---
  "LNS11000000", "Total: Civilian Labor Force Level", "CPS - Labor Force",
  "LNS11300000", "Total: Labor Force Participation Rate", "CPS - Rates",
  "LNS11300060", "Prime-Age (25-54): Labor Force Participation Rate", "CPS - Rates",
  "LNS11300001", "Men: Labor Force Participation Rate", "CPS - Demographics",
  "LNS11300002", "Women: Labor Force Participation Rate", "CPS - Demographics",
  "LNU01373413", "Native-Born: Labor Force Participation Rate (NSA)", "CPS - Demographics",
  "LNU01373395", "Foreign-Born: Labor Force Participation Rate (NSA)", "CPS - Demographics",
  "LNS12000000", "Total: Civilian Employment Level", "CPS - Employment",
  "LNS12000001", "Men: Employment Level", "CPS - Demographics",
  "LNS12000002", "Women: Employment Level", "CPS - Demographics",
  "CES0000000010", "Women Nonfarm: Employment Level", "CPS - Demographics", 
  "LNS12300000", "Total: Employment-Population Ratio", "CPS - Rates",
  "LNS12500000", "Total: Employed Full-Time Level", "CPS - Employment",
  "LNS12600000", "Total: Employed Part-Time Level", "CPS - Employment",
  "LNS13000000", "Total: Unemployment Level", "CPS - Unemployment",
  "LNS14000000", "Total: Unemployment Rate", "CPS - Rates",
  "LNS14000012", "16-19: Unemployment Rate", "CPS - Demographics",
  "LNS14000025", "Men 20+: Unemployment Rate", "CPS - Demographics",
  "LNS14000026", "Women 20+: Unemployment Rate", "CPS - Demographics",
  "LNS14000003", "White: Unemployment Rate", "CPS - Demographics",
  "LNS14000006", "Black or African American: Unemployment Rate", "CPS - Demographics",
  "LNS14032183", "Asian: Unemployment Rate", "CPS - Demographics",
  "LNS14000009", "Hispanic or Latino: Unemployment Rate", "CPS - Demographics",
  "LNS14027659", "Less Than High School: Unemployment Rate", "CPS - Education",
  "LNS14027660", "High School Graduates: Unemployment Rate", "CPS - Education",
  "LNS14027689", "Some College or Assoc Degree: Unemployment Rate", "CPS - Education",
  "LNS14027662", "Bachelor's Degree & Higher: Unemployment Rate", "CPS - Education",
  
  # --- CPS: Duration of Unemployment ---
  "LNS13008396", "Total: Unemployed Less Than 5 Wks", "CPS - Duration",
  "LNS13008756", "Total: Unemployed 5 to 14 Wks", "CPS - Duration",
  "LNS13008876", "Total: Unemployed 15 to 26 Wks", "CPS - Duration",
  "LNS13008636", "Total: Unemployed 27 Wks & Over", "CPS - Duration",
  "LNS13008275", "Total: Average Duration of Unemployment (Wks)", "CPS - Duration",
  "LNS13008276", "Total: Median Duration of Unemployment (Wks)", "CPS - Duration",
  
  # --- CPS: Reason for Unemployment ---
  "LNS13023621", "Total: Job Losers Level", "CPS - Reason",
  "LNS13023705", "Total: Job Leavers Level", "CPS - Reason",
  "LNS13023557", "Total: Reentrants Level", "CPS - Reason",
  "LNS13023569", "Total: New Entrants Level", "CPS - Reason",
  
  # --- CPS: Labor Slack & Other Metrics ---
  "LNS12032194", "Total: Part-Time for Economic Reasons Level", "CPS - Part-Time",
  "LNS15000000", "Total: Not in Labor Force Level", "CPS - Labor Force",
  "LNS15026642", "Total: Want a Job Now Level", "CPS - Labor Force",
  "LNS15026645", "Total: Marginally Attached to Labor Force Level", "CPS - Labor Force",
  "LNS13327709", "Total: U-6 Unemployment Rate (Broad)", "CPS - Rates",
  "LNS12026619", "Total: Multiple Jobholders Level", "CPS - Employment",
  "LNS12026620", "Total: Multiple Jobholders Percent", "CPS - Rates",
  "LNU02036012", "Total: Part-Time Slack Work Level (NSA)", "CPS - Part-Time (NSA)",
  "LNU02033224", "Total: Part-Time Could Find Full Level (NSA)", "CPS - Part-Time (NSA)",
  
  # --- CPI ---
  "CUSR0000SA0", "Total: Consumer Price Index (CPI-U)", "CPI - Inflation"
)

# Combine base, JOLTS industry, and JOLTS size class dictionaries
series_lookup <- bind_rows(base_lookup, jolts_industry_lookup, jolts_size_lookup) %>% 
  distinct(series_id, .keep_all = TRUE)

all_series <- series_lookup$series_id

current_year <- as.numeric(format(Sys.Date(), "%Y"))
start_year   <- as.character(current_year - 7)
end_year     <- as.character(current_year)

# ==========================================
# 2. CHUNKING & CALLING BLS API
# ==========================================

# Chunk into groups of 40 (API payload limit)
series_chunks <- split(all_series, ceiling(seq_along(all_series) / 40))

fetch_bls_chunk <- function(chunk) {
  # 1. Clean chunk vector (remove NAs and empty strings)
  clean_chunk <- as.character(chunk[!is.na(chunk) & chunk != ""])
  
  # 2. Build payload with explicit scalar unboxing
  body_data <- list(
    seriesid        = clean_chunk,
    startyear       = jsonlite::unbox(as.character(start_year)),
    endyear         = jsonlite::unbox(as.character(end_year)),
    registrationkey = jsonlite::unbox(as.character(BLS_API_KEY))
  )
  
  res <- httr::POST(
    url = "https://api.bls.gov/publicAPI/v2/timeseries/data/",
    body = jsonlite::toJSON(body_data),
    httr::add_headers("Content-Type" = "application/json")
  )
  
  json_content <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  # Return notice if BLS rejects payload or returns empty data
  if (is.null(json_content$Results$series) || length(json_content$Results$series) == 0) {
    if (!is.null(json_content$message)) {
      message("BLS API Notice: ", paste(json_content$message, collapse = " "))
    }
    return(NULL)
  }
  
  series_list <- json_content$Results$series
  
  purrr::map_dfr(series_list, function(s) {
    if (!is.null(s$data) && length(s$data) > 0) {
      df <- dplyr::bind_rows(s$data)
      df$seriesID <- s$seriesID
      return(df)
    }
    return(NULL)
  })
}

message("Fetching ", length(all_series), " BLS Series across ", length(series_chunks), " API chunks...")
raw_combined_df <- map_dfr(series_chunks, fetch_bls_chunk)

# ==========================================
# 3. CLEANING, INTERPOLATION & JOINING
# ==========================================

clean_bls_df <- raw_combined_df %>%
  filter(grepl("^M[0-1][0-9]$", period)) %>%
  mutate(
    month_num    = gsub("M", "", period),
    date         = as.Date(paste(year, month_num, "01", sep = "-"), format = "%Y-%m-%d"),
    value        = as.numeric(value),
    last_updated = Sys.time()
  ) %>%
  left_join(series_lookup, by = c("seriesID" = "series_id")) %>%
  
  # --- DATA PADDING FIX FOR TABLEAU STACKED CHARTS ---
  group_by(seriesID, series_name, category) %>%
  complete(date = seq.Date(min(date), max(date), by = "month")) %>%
  fill(value, .direction = "down") %>% 
  ungroup() %>%
  
  select(
    series_id = seriesID,
    series_name,
    category,
    date,
    value,
    last_updated
  )

# ==========================================
# 4. EXPORT TO CSV FOR TABLEAU
# ==========================================

write.csv(clean_bls_df, file.path(dirRun, "bls_tableau_master.csv"), row.names = FALSE, na = "")

message("Success! Saved ", nrow(clean_bls_df), " rows to CSV across ", length(unique(clean_bls_df$series_id)), " series.")
