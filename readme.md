# Racial dot density map
## Lei Ma 

I produce racial dot maps at the census tract level to visualize the segregation/integration pattern of different racial/ethnic groups. It uses the Decennial Census data in 2000, 2010, and 2020. I focus on four racial/ethnic groups, on-Hispanic White, non-Hispanic Black, non-Hispanic Asian, and Hispanic/Latin. The code can be modified to add other groups and other years. Please feel free to contact me (leim@bu.edu) if you have any feedback.

The [Univeristy of Virginia](http://racialdotmap.demographics.coopercenter.org) used to produce racial dot maps using the 2010 Census, but they are no longer available. [CNN](https://www.cnn.com/interactive/2021/us/census-race-ethnicity-map/) also publishes interactive racial dot maps using the 2020 Census. My code can be used to replicate these racial dot maps.

## Example maps

![alt text](https://github.com/leima0521/racial_dot_density_maps/blob/8345805e348f8232561836461f4f4bbee1e757b3/out/Chicago-Naperville-Elgin,%20IL-IN-WI-2000.png)

![alt text](https://github.com/leima0521/racial_dot_density_maps/blob/8345805e348f8232561836461f4f4bbee1e757b3/out/Chicago-Naperville-Elgin,%20IL-IN-WI-2010.png)

![alt text](https://github.com/leima0521/racial_dot_density_maps/blob/8345805e348f8232561836461f4f4bbee1e757b3/out/Chicago-Naperville-Elgin,%20IL-IN-WI-2020.png)

## Code

### Download data

[download_race_data.R](code/download_race_data.R) downloads the Census data. It requires the [`tidycensus`](https://walker-data.com/tidycensus/) package to call for Census APIs to download the data. You need to get a Census API for your own machine. I also use packages `tidyverse` and `tidyr` to clean the data.

The function `download_race_data` downloads data using the Census API. I focus on four racial/ethnic groups: non-Hispanic White, non-Hispanic Black, non-Hispanic Asian, and Hispanic/Latino. You can modify `variable <- c(...)` if you want to add other racial/ethnic groups, such as Hawaiian/Pacific Islander. You can also use the `get_acs()` function if you want to download the American Community Survey data in other years.

To use the function, simply specify the smallest geographic level of data, state, county, and year. For example, if you want to download the racial/ethnic population of Chicago at the census tract level in 2010, run `download_race_data(state = "IL", county = "Cook", year = 2010)`. If you want to download the data for multiple cities, you can use the `pmap_df` function. My code shows an example of how to download the data for the 10 largest CBSAs in the US.

```
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
```

### Make dot maps

[make_maps.R](code/make_maps.R) produces dot maps. Each dot represents a unique racial/ethnic group and captures 150 people by default. It requires the [`tmap`](https://github.com/r-tmap/tmap) package to produce the maps. 

The `make_dot_density_map` function produces the map by specifying the CBSA, year, and the population size of each dot. The main part of the code is written in [Kyle Walker's book Analayzing US Census Data](https://walker-data.com/census-r/mapping-census-data-with-r.html).

For example, if you want to produce a racial dot example for Chicago in 2010, run `make_dot_density_map("Chicago-Naperville-Elgin, IL-IN-WI", 2010)`. The default size of each dot is 150. If you want to change the dot value, simply use other values for `dotsize = xxx`. Note that it takes a while to produce these maps (your machine is not broken). My code also provides an example of how to use `pmap` to produce multiple maps. 

You can change aesthetics by changing values in the function. For example, you can change the color palette using a different `col = brewer.pal(xxx)`. You can also add major roads, rivers, lakes, and other layers to the map using `tmap`. If you want to produce interactive maps, check out the [`Leaflet`](https://rstudio.github.io/leaflet/) package and [Kyle Walker's book chapter](https://rstudio.github.io/leaflet/). 

```
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
    mutate(group = factor(group, levels = c("Asian", "White", "Black","Hispanic")))

  background_tracts <- filter(df, variable == "White")
  m <- tm_shape(background_tracts, 
           projection = sf::st_crs(4326)) + 
    tm_polygons(col = "white", 
                border.col = "grey80",
                lwd = 0.5) + 
    tm_shape(dots) +
    tm_dots(col = "group", 
            alpha = transparency,
            palette = "Spectral",
            size = 0.001,
            legend.show = FALSE) +
    tm_add_legend('symbol', size = c(0.3, 0.3, 0.3, 0.3), 
                 labels = c("Asian", "White", "Black","Hispanic"),
                 col = brewer.pal(4, "Spectral")
                 ) +
    tm_layout(main.title = paste0(cbsa, ", ", t),
              main.title.position = c('left','top'),
              main.title.size = 0.6,
              frame = FALSE
              )
  tmap_save(m, paste0(cbsa, "-", t, ".png"), 
            width = 6, height = 4)
}
```



