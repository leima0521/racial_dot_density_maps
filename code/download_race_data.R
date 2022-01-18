## Download racial/ethnic group population from Census in 2000, 2010, and 2020

rm(list = ls())
library(tidyverse)
library(tidycensus)
library(tidyr)
options(tigris_use_cache = TRUE)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
datapath <- "../data/"

# Define function to download race data at census tract level ----
# choose year in 2000, 2010, and 2020
download_race_data <- function(geo_level = "tract", state, county, year) {
  if (year == 2000) {
    survey <- "sf1"
    varnames <- load_variables(year, survey, cache = FALSE)
    summaryvar <- "P008001"
    variable <- c(
      White = "P008003",
      Black = "P008004",
      Asian = "P008006",
      Hispanic = "P008010"
    )
  } else if (year == 2010) {
    survey <- "sf1"
    varnames <- load_variables(year, survey, cache = FALSE)
    summaryvar <- "P005001"
    variable <- c(
      White = "P005003",
      Black = "P005004",
      Asian = "P005006",
      Hispanic = "P005010"
    )
  } else if (year == 2020) {
    survey <- "pl"
    varnames <- load_variables(year, survey, cache = FALSE)
    summaryvar <- "P2_001N"
    variable <- c(
      White = "P2_005N",
      Black = "P2_006N",
      Asian = "P2_008N",
      Hispanic = "P2_002N"
    )
  }
  df <- get_decennial(
    geography = geo_level,
    state = state,
    county = county,
    variables = variable,
    summary_var = summaryvar,
    year = year,
    geometry = TRUE
  ) %>%
    rename(
      race = variable,
      pop = value,
      tot_pop = summary_value
    ) %>%
    mutate(
      pop_share = pop / tot_pop,
      year = year
    )
}


## Example ----

## select the 10 largest CBSAs
cbsa2fipsxw <- read_csv(paste0(datapath, "cbsa2fipsxw.csv"))
cbsa2fipsxw <- cbsa2fipsxw %>% filter(!is.na(cbsacode))
largest_cbsa <- cbsa2fipsxw %>%
  filter(
    cbsatitle %in% c(
      "New York-Newark-Jersey City, NY-NJ-PA",
      "Los Angeles-Long Beach-Anaheim, CA",
      "Chicago-Naperville-Elgin, IL-IN-WI",
      "Dallas-Fort Worth-Arlington, TX",
      "Houston-The Woodlands-Sugar Land, TX",
      "Washington-Arlington-Alexandria, DC-VA-MD-WV",
      "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD",
      "Miami-Fort Lauderdale-West Palm Beach, FL",
      "Atlanta-Sandy Springs-Roswell, GA",
      "Boston-Cambridge-Newton, MA-NH"
    ),
    centraloutlyingcounty == "Central"
  )

sample_year <- c(2000, 2010, 2020)
n <- length(sample_year)
sample_state <- rep(largest_cbsa$fipsstatecode, n)
sample_county <- rep(largest_cbsa$fipscountycode, n)

# download data ----
race_tract_df <- pmap_df(
  list(
    state = sample_state,
    county = sample_county,
    year = sapply(sample_year, function(x) rep(x, length(sample_state) / n)) %>% as.vector()
  ),
  download_race_data
)
race_tract_df <- race_tract_df %>%
  arrange(GEOID, year) %>%
  select(year, everything()) %>%
  filter(tot_pop > 0)
race_tract_df <- race_tract_df %>%
  separate(NAME, c("tract", "countycountyequivalent", "statename"), sep = ",") %>%
  mutate(
    tract = str_trim(str_remove(tract, "Census Tract")),
    countycountyequivalent = str_trim(countycountyequivalent),
    statename = str_trim(statename)
  )
race_tract_df <- race_tract_df %>%
  left_join(.,
    cbsa2fipsxw %>%
      select(countycountyequivalent, statename, cbsatitle, cbsacode),
    by = c("countycountyequivalent", "statename")
  )
saveRDS(race_tract_df, paste0(datapath, "race_tract_decennial_census.Rds"))
