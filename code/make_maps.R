## make racial dot density maps

rm(list = ls())
library(tidyverse)
library(sf)
library(tmap)
options(tigris_class = "sf")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
datapath <- "../data/"

## load data ----
race_tract_df <- readRDS(paste0(datapath, "race_tract_decennial_census.Rds"))
race_tract_df <- race_tract_df %>%
  rename(variable = race, value = pop) %>%
  select(-tot_pop, -pop_share)

## make dot density map ----

make_dot_density_map <- function(cbsa, t, dotsize = 150, transparency = 0.7) {
  df <- subset(race_tract_df, cbsatitle == cbsa & year == t)
  groups <- unique(df$variable)
  dots <- map_dfr(groups, ~ {
    df %>%
      filter(variable == .x) %>%
      st_transform(26915) %>%
      mutate(dotsize = as.integer(value / dotsize)) %>%
      st_sample(size = .$dotsize, exact = FALSE) %>%
      st_sf() %>%
      mutate(group = .x)
  }) %>%
    slice_sample(prop = 1) %>%
    mutate(group = factor(group, levels = c("Asian", "White", "Black", "Hispanic")))

  background_tracts <- filter(df, variable == "White")
  m <- tm_shape(background_tracts,
    projection = sf::st_crs(4326)
  ) +
    tm_polygons(
      col = "white",
      border.col = "grey80",
      lwd = 0.5
    ) +
    tm_shape(dots) +
    tm_dots(
      col = "group",
      alpha = transparency,
      palette = "Spectral",
      size = 0.001,
      legend.show = FALSE
    ) +
    tm_add_legend("symbol",
      size = c(0.3, 0.3, 0.3, 0.3),
      labels = c("Asian", "White", "Black", "Hispanic"),
      col = brewer.pal(4, "Spectral")
    ) +
    tm_layout(
      main.title = paste0(cbsa, ", ", t),
      main.title.position = c("left", "top"),
      main.title.size = 0.6,
      frame = FALSE
    )
  tmap_save(m, paste0(cbsa, "-", t, ".png"),
    width = 6, height = 4
  )
}

## Examples ----

## produce one single map
# make_dot_density_map("Los Angeles-Long Beach-Anaheim, CA", 2000)
# make_dot_density_map("Chicago-Naperville-Elgin, IL-IN-WI", 2010)
# make_dot_density_map("Chicago-Naperville-Elgin, IL-IN-WI", 2020)

## produce multiple maps
sample_year <- c(2000, 2010, 2020)
n <- length(sample_year)
sample_cbsa <- rep(c(
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
), n)

pmap(
  list(
    cbsa = sample_cbsa,
    t = sapply(sample_year, function(x) rep(x, length(sample_cbsa) / n)) %>% as.vector()
  ),
  make_dot_density_map
)
