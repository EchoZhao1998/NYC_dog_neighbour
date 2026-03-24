master_filtered    <- readRDS("data/cleaned/master_filtered.rds")
dogs_per_zip_year  <- readRDS("data/cleaned/dogs_per_zip_year.rds")
bites_clean        <- readRDS("data/cleaned/bites_clean.rds")
bites_per_year_borough <- readRDS("data/cleaned/bites_per_year_borough.rds")

cat("All loaded.\n")
cat("master_filtered rows:", nrow(master_filtered), "\n")
cat("bites_per_year_borough rows:", nrow(bites_per_year_borough), "\n")

library(tidycensus)
library(tidyverse)

# ACS 1-year estimates give us annual borough population
# Available for 2016, 2017, 2018, 2019, 2021, 2022, 2023
# (2020 ACS 1-year was not released due to COVID data quality issues)

nyc_counties_fips <- c("005", "047", "061", "081", "085")
# Bronx=005, Kings(Brooklyn)=047, New York(Manhattan)=061, 
# Queens=081, Richmond(Staten Island)=085

pop_years <- c(2016, 2017, 2018, 2019, 2021, 2022, 2023)

pop_raw <- map_dfr(pop_years, function(yr) {
  get_acs(
    geography  = "county",
    variables  = "B01003_001",   # total population
    state      = "NY",
    county     = nyc_counties_fips,
    year       = yr,
    survey     = "acs1"          # 1-year for annual estimates
  ) |>
    mutate(year = yr)
})

pop_clean <- pop_raw |>
  mutate(borough = case_when(
    str_detect(NAME, "Bronx")      ~ "Bronx",
    str_detect(NAME, "Kings")      ~ "Brooklyn",
    str_detect(NAME, "New York C") ~ "Manhattan",
    str_detect(NAME, "Queens")     ~ "Queens",
    str_detect(NAME, "Richmond")   ~ "Staten Island"
  )) |>
  select(borough, year, population = estimate)

cat("Population data:\n")
pop_clean |> arrange(borough, year) |> print(n = 40)

saveRDS(pop_clean, "data/cleaned/nyc_borough_population.rds")