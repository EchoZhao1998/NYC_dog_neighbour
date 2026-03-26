# ══════════════════════════════════════════════════════════════════════════════
# PHASE 2: Full Cleaning & Master Dataset Construction
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(lubridate)
library(janitor)

options(tigris_use_cache = TRUE)
census_api_key("d43934f52489c25471f4f6e0942464d34a9052a8",install = T, overwrite = T)

# ── Load raw data (adjust filenames with "clean_names"(from pkg(janitor)) 
# converts everything to lowercase with underscores) ───────────────────────────

dogs_raw  <- read_csv("data/Dog_Licensing.csv") |> clean_names()
bites_raw <- read_csv("data/Dog_Bite.csv") |> clean_names()
dog_runs_raw <- read_csv("data/Dog_Runs.csv") |> clean_names()

# ══════════════════════════════════════════════════════════════════════════════
# STEP 1: Build NYC zipcode reference list from ZCTA boundaries (Dataset E)
# ══════════════════════════════════════════════════════════════════════════════

zcta_all <- zctas(cb = TRUE, year = 2020) # my data period is around 2015 - 2023. so I choice "2020"

# Bound of an area. Mentioned on 5147 Week4 Applied Session 
nyc_bbox <- st_bbox(c(xmin = -74.26, ymin = 40.49,
                      xmax = -73.70, ymax = 40.92),
                    crs = st_crs(4326)) |> st_as_sfc() 
                   # st_crs = "spatial coodinate reference"
                   # While a "bound" (bbox) defines where the edges of your box are, 
                   # the CRS tells R how to interpret those numbers on the Earth's surface. 
                   # 4326 is the EPSG code for WGS 84, 
                   # the most common global system used by GPS and web maps. It tells R these coordinates are Longitude and Latitude in decimal degrees.



# filters the list of ZIP codes (zcta_all) to keep only the areas (polygons) 
# that physically overlap or touch NYC bounding box(nyc_bbox).
zcta_nyc <- zcta_all |>
  st_transform(4326) |>
  filter(st_intersects(geometry, nyc_bbox, sparse = FALSE)[,1])
# sparse = TRUE (Default): Returns a list like [[1]] 1, [[2]] empty. 
# This is memory-efficient but hard to use inside a filter() function.
# sparse = FALSE: Returns a logical matrix (a grid of TRUE or FALSE).
# The [,1] pulls that first column out as a simple vector of TRUE/FALSE.
# Since I'm comparing many ZIP codes against one bounding box, the matrix has many rows but only one column.

nyc_zip_list <- zcta_nyc$ZCTA5CE20
cat("NYC ZCTAs from tigris:", length(nyc_zip_list), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 2: Clean Dataset A — Dog Licensing
# ══════════════════════════════════════════════════════════════════════════════

dogs_clean <- dogs_raw |>
  # drop 100%-missing columns
  select(animalbirth, extract_year, animalname, animalgender,
         breedname, zipcode, licenseissueddate, licenseexpireddate) |>
  distinct() |>                                       # remove 39,702 duplicates
  filter(!is.na(zipcode), !is.na(animalgender)) |>    # drop tiny NA rows
  mutate(zipcode = str_pad(as.character(as.integer(zipcode)), 5, pad = "0")) |> 
  filter(zipcode %in% nyc_zip_list)                   # NYC only

# The Problem: When ZIP codes are stored as numbers (as.integer), Excel or R often drops the "leading zero." For example, a Brooklyn ZIP code like 01101 becomes just 1101.
# The Fix:
  # as.character() turns the number into text.
  # 5 tells R the total length must be 5.
  # pad = "0" tells R to add zeros to the left side until it hits that length
  # 7452 (4 digits) becomes "07452"

cat("\nDataset A — rows after cleaning:", nrow(dogs_clean), "\n")
cat("Years available:", sort(unique(dogs_clean$extract_year)), "\n")
cat("Unique zipcodes:", n_distinct(dogs_clean$zipcode), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 3: Clean Dataset B — Dog Bites
# ══════════════════════════════════════════════════════════════════════════════

bites_clean <- bites_raw |>
  filter(species == "DOG") |> # ensure sample are all dog, in case some "Cat" mess 
  filter(!is.na(borough), borough != "Other") |>
  mutate(
    year    = year(dateofbite),
    month   = month(dateofbite),
    zipcode = str_pad(zipcode, 5, pad = "0"),
    zipcode = if_else(zipcode %in% nyc_zip_list, zipcode, NA_character_)
  ) |>
  filter(year >= 2016, year <= 2023)

cat("\nDataset B — rows after cleaning:", nrow(bites_clean), "\n")
cat("Years:", sort(unique(bites_clean$year)), "\n")
cat("Remaining NA zips:", sum(is.na(bites_clean$zipcode)), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 4: Clean Dataset C — Dog Runs (parse WKT geometry)
# ══════════════════════════════════════════════════════════════════════════════

dog_runs_sf <- dog_runs_raw |>
  filter(!is.na(the_geom)) |>
  st_as_sf(wkt = "the_geom", crs = 4326) |>
  mutate(borough_full = case_when(
    borough == "M" ~ "Manhattan",
    borough == "B" ~ "Brooklyn",
    borough == "Q" ~ "Queens",
    borough == "X" ~ "Bronx",
    borough == "R" ~ "Staten Island",
    TRUE           ~ NA_character_
  ))

cat("\nDataset C — dog runs loaded:", nrow(dog_runs_sf), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 5: Load Dataset D — ACS Income (2022 vintage)
# ══════════════════════════════════════════════════════════════════════════════

income_raw <- get_acs(
  geography = "zcta",
  variables = "B19013_001",
  year      = 2022, # in order the year consistent with records in the datasets, use 2022 instead of 2025. Note: Even "2024" is invalid. 2025 can aquire data
  survey    = "acs5"
)

income_nyc <- income_raw |>
  filter(GEOID %in% nyc_zip_list) |>
  rename(zipcode = GEOID, median_income = estimate) |>
  select(zipcode, median_income, moe) |>
  filter(!is.na(median_income))                       # drop 13 NA-income ZCTAs

cat("\nDataset D — NYC income ZCTAs retained:", nrow(income_nyc), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 6: Aggregate all datasets to ZCTA level
# ══════════════════════════════════════════════════════════════════════════════

# --- A: total dogs per zip (all years combined) -------------------------------
dogs_per_zip <- dogs_clean |>
  count(zipcode, name = "total_dogs")

# --- A: dogs per zip per year (for Q1 temporal analysis) ---------------------
dogs_per_zip_year <- dogs_clean |>
  count(zipcode, extract_year, name = "dog_count")

# --- B: bites per zip (2016-2023, zip-matched only) --------------------------
bites_per_zip <- bites_clean |>
  filter(!is.na(zipcode)) |>
  count(zipcode, name = "total_bites")

# --- B: bites per year, borough level (for Q1 — full temporal range) ---------
bites_per_year_borough <- bites_clean |>
  count(year, borough, name = "bite_count")

# --- C: dog runs per zip (spatial join) --------------------------------------
dog_runs_zip <- st_join(
  dog_runs_sf  |> st_transform(st_crs(zcta_nyc)),
  zcta_nyc     |> select(ZCTA5CE20),
  join = st_within
) |>
  st_drop_geometry() |>
  count(ZCTA5CE20, name = "n_runs") |>
  rename(zipcode = ZCTA5CE20)

# ══════════════════════════════════════════════════════════════════════════════
# STEP 7: Build master spatial dataframe
# ══════════════════════════════════════════════════════════════════════════════

master_sf <- zcta_nyc |>
  rename(zipcode = ZCTA5CE20) |>
  left_join(dogs_per_zip,  by = "zipcode") |>
  left_join(bites_per_zip, by = "zipcode") |>
  left_join(dog_runs_zip,  by = "zipcode") |>
  left_join(income_nyc |> select(zipcode, median_income), by = "zipcode") |>
  mutate(
    total_dogs   = replace_na(total_dogs,  0),
    total_bites  = replace_na(total_bites, 0),
    n_runs       = replace_na(n_runs,      0),
    area_km2     = as.numeric(ALAND20) / 1e6,
    dog_density  = if_else(area_km2 > 0, total_dogs / area_km2, NA_real_),
    bite_rate    = if_else(total_dogs > 0, total_bites / total_dogs * 1000,
                           NA_real_),
    dogs_per_run = if_else(n_runs > 0, total_dogs / n_runs, NA_real_)
  )

# ══════════════════════════════════════════════════════════════════════════════
# STEP 8: Sanity checks on master
# ══════════════════════════════════════════════════════════════════════════════

cat("\n=== MASTER DATASET SUMMARY ===\n")
cat("Total ZCTAs:", nrow(master_sf), "\n")
cat("ZCTAs with dogs > 0:", sum(master_sf$total_dogs > 0), "\n")
cat("ZCTAs with bites > 0:", sum(master_sf$total_bites > 0), "\n")
cat("ZCTAs with runs > 0:", sum(master_sf$n_runs > 0), "\n")
cat("ZCTAs with income data:", sum(!is.na(master_sf$median_income)), "\n")

master_sf |> st_drop_geometry() |>
  select(dog_density, bite_rate, n_runs, dogs_per_run, median_income) |>
  summary()

# ══════════════════════════════════════════════════════════════════════════════
# STEP 9: Save outputs for Phase 3 (visualisation)
# ══════════════════════════════════════════════════════════════════════════════

saveRDS(master_sf,             "master_sf.rds")
saveRDS(dogs_per_zip_year,     "dogs_per_zip_year.rds")
saveRDS(bites_clean,           "bites_clean.rds")
saveRDS(bites_per_year_borough,"bites_per_year_borough.rds")
saveRDS(dogs_clean,            "dogs_clean.rds")

cat("\nAll files saved. Ready for Phase 3.\n")

# Run these one at a time

# 1. Zero-dog ZCTAs
zero_dogs <- master_sf |>
  st_drop_geometry() |>
  filter(total_dogs == 0) |>
  select(zipcode, total_dogs, total_bites, area_km2, median_income) |>
  arrange(desc(area_km2))

cat("Zero-dog ZCTAs:", nrow(zero_dogs), "\n")
head(zero_dogs, 20)

# 2. Bite rate outliers
bite_outliers <- master_sf |>
  st_drop_geometry() |>
  filter(!is.na(bite_rate)) |>
  arrange(desc(bite_rate)) |>
  select(zipcode, total_dogs, total_bites, bite_rate)

head(bite_outliers, 10)

# 3. Dog count distribution (non-zero ZCTAs only)
dog_counts <- master_sf |>
  st_drop_geometry() |>
  filter(total_dogs > 0) |>
  pull(total_dogs)

quantile(dog_counts, probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95))

# 4. Apply filter and check
master_filtered <- master_sf |>
  filter(total_dogs >= 50)

cat("ZCTAs retained:", nrow(master_filtered), "\n")
cat("ZCTAs with runs > 0:", sum(master_filtered$n_runs > 0), "\n")
cat("ZCTAs with income:", sum(!is.na(master_filtered$median_income)), "\n")

master_filtered |>
  st_drop_geometry() |>
  select(dog_density, bite_rate, n_runs, dogs_per_run, median_income) |>
  summary()

saveRDS(master_filtered, "master_filtered.rds")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 8c: Final checks + borough labelling
# ══════════════════════════════════════════════════════════════════════════════

# 1. Confirm no NJ zips snuck through the filter
cat("=== Any non-NYC zips remaining? ===\n")
master_filtered |>
  st_drop_geometry() |>
  filter(!str_detect(zipcode, "^(100|101|102|103|104|110|111|112|113|114)")) |>
  select(zipcode, total_dogs) |>
  head(10)

# ── Step 2 fix: borough labels via county boundaries ─────────────────────────

nyc_counties <- tigris::counties(state = "NY", cb = TRUE, year = 2020) |>
  st_transform(4326) |>
  filter(NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) |>
  mutate(borough = case_when(
    NAME == "New York"  ~ "Manhattan",
    NAME == "Kings"     ~ "Brooklyn",
    NAME == "Queens"    ~ "Queens",
    NAME == "Bronx"     ~ "Bronx",
    NAME == "Richmond"  ~ "Staten Island"
  )) |>
  select(borough, geometry)

# Use centroid of each ZCTA for the join (more reliable than polygon intersect)
zcta_centroids <- master_filtered |>
  st_centroid()

borough_join <- zcta_centroids |>
  st_join(nyc_counties, join = st_within) |>
  st_drop_geometry() |>
  select(zipcode, borough)

master_filtered <- master_filtered |>
  left_join(borough_join, by = "zipcode")

cat("=== Borough distribution of ZCTAs ===\n")
master_filtered |>
  st_drop_geometry() |>
  count(borough) |>
  arrange(desc(n))

cat("\n=== Median bite rate by borough ===\n")
master_filtered |>
  st_drop_geometry() |>
  filter(!is.na(bite_rate), !is.na(borough)) |>
  group_by(borough) |>
  summarise(
    median_bite_rate = round(median(bite_rate), 2),
    median_income    = round(median(median_income, na.rm = TRUE)),
    total_runs       = sum(n_runs),
    n_zctas          = n()
  ) |>
  arrange(desc(median_bite_rate))

saveRDS(master_filtered, "master_filtered.rds")
cat("\nSaved. Ready for Phase 3.\n")

# Remove any existing borough column first, then redo the join
master_filtered <- master_filtered |>
  select(-any_of("borough"))   # drop if it exists, no error if it doesn't

zcta_centroids <- master_filtered |>
  st_centroid()

borough_join <- zcta_centroids |>
  st_join(nyc_counties, join = st_within) |>
  st_drop_geometry() |>
  select(zipcode, borough)

master_filtered <- master_filtered |>
  left_join(borough_join, by = "zipcode")

cat("=== Borough distribution ===\n")
master_filtered |>
  st_drop_geometry() |>
  count(borough) |>
  arrange(desc(n))

cat("\n=== Median bite rate by borough ===\n")
master_filtered |>
  st_drop_geometry() |>
  filter(!is.na(bite_rate), !is.na(borough)) |>
  group_by(borough) |>
  summarise(
    median_bite_rate = round(median(bite_rate), 2),
    median_income    = round(median(median_income, na.rm = TRUE)),
    total_runs       = sum(n_runs),
    n_zctas          = n()
  ) |>
  arrange(desc(median_bite_rate))

saveRDS(master_filtered, "master_filtered.rds")
cat("\nSaved. Ready for Phase 3.\n")