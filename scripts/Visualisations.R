# ══════════════════════════════════════════════════════════════════════════════
# Phase 3 — Q1 Visualisations
# ══════════════════════════════════════════════════════════════════════════════

# Crucial reference: 
# 1) https://r-graph-gallery.com/ggplot2-package.html
# 2) https://learning.monash.edu/mod/book/view.php?id=5329014&chapterid=933784
# 3) https://ggplot2-book.org/scales-guides.html
# 4) https://onepager.togaware.com/Labels_with_Comma.html
# 5) https://ggplot2.tidyverse.org/reference/ggsf.html
# 6) https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

library(tidyverse)
library(sf)
library(patchwork)
library(viridis)
library(ggrepel) # Ask Google AImode for help on adjusting legend, it told me this

# ── Load cleaned data ─────────────────────────────────────────────────────────
master_filtered        <- readRDS("data/cleaned/master_filtered.rds")
dogs_clean             <- readRDS("data/cleaned/dogs_clean.rds")
bites_per_year_borough <- readRDS("data/cleaned/bites_per_year_borough.rds")
pop_clean              <- readRDS("data/cleaned/nyc_borough_population.rds")

# ── Prepare ownership rate data ───────────────────────────────────────────────

# Aggregate licensed dogs by borough and year
dogs_borough_year <- dogs_clean |>
  filter(zipcode %in% master_filtered$zipcode) |>
  left_join(
    master_filtered |> st_drop_geometry() |> select(zipcode, borough),
    by = "zipcode"
  ) |>
  filter(!is.na(borough)) |>
  count(borough, extract_year, name = "licensed_dogs") |>
  rename(year = extract_year)

# Join with population to compute ownership rate
ownership_rate <- dogs_borough_year |>
  left_join(pop_clean, by = c("borough", "year")) |>
  mutate(
    dogs_per_1000 = licensed_dogs / population * 1000
  )

cat("Ownership rate table:\n")
ownership_rate |> arrange(borough, year) |> print(n = 30)

# Borough colour palette — consistent across all plots
# color hex number randomly pick from: colorbrewer2.org (resource: 5147 W4 workshop slides.)

borough_colours <- c(
  "Manhattan"     = "#3182bd",
  "Brooklyn"      = "#c51b8a",
  "Queens"        = "#feb24c",
  "Bronx"         = "#31a354",
  "Staten Island" = "#e6550d"
)

# ── Borough reference map ─────────────────────────────────────────────────────

nyc_boroughs_map <- master_filtered |>
  filter(!is.na(borough)) |>
  group_by(borough) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

# Compute centroids for borough labels
borough_centroids <- nyc_boroughs_map |>
  st_centroid() |>
  # calculates the exact geometric center of each borough's shape. 
  # It turns the big borough polygons into single points.
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    # The geometry column in a spatial dataset is complex. 
    # These lines extract the raw Longitude (X) and Latitude (Y) numbers into simple columns (lon and lat) 
    # so ggplot can read them easily.
    
    # Manual nudges for legibility
    lat = case_when(
      borough == "Staten Island" ~ lat + 0.01,
      borough == "Bronx"         ~ lat + 0.01,
      TRUE                       ~ lat
    )
  )

p_borough_ref <- ggplot() +
  geom_sf(data   = nyc_boroughs_map,
          aes(fill = borough),
          colour = "#FFFFFF", linewidth = 0.6) +
  geom_text(data = borough_centroids,
            aes(x = lon, y = lat, label = borough),
            size     = 3.5,
            fontface = "bold",
            colour   = "#1F2933") + # my website theme color, welcome to visit: https://echozhao1998.github.io/
  scale_fill_manual(values = borough_colours, guide = "none") +
  labs(
    title   = "New York City — five boroughs",
    caption = "Reference map; borough boundaries derived from US Census TIGER/Line ZCTAs"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title  = element_text(face = "bold", size = 12),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p_borough_ref)
ggsave("plots/plot0_borough_reference.png", p_borough_ref,
       width = 6, height = 5, dpi = 300, bg = "white")
cat("Borough reference map saved.\n")


# ── Plot 1A: Ownership rate bar chart ─────────────────────────────────────────

# Add a gap label for the missing years
gap_label <- tibble(
  x = 2020, y = max(ownership_rate$dogs_per_1000, na.rm = TRUE) * 0.5,
  label = "No licence\ndata 2019–21"
)

p1a <- ggplot(ownership_rate,
              aes(x = factor(year), y = dogs_per_1000, fill = borough)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  annotate("rect", xmin = 3.4, xmax = 3.6,          # narrow gap between 2018 and 2022
           ymin = -Inf, ymax = Inf,
           fill = "#d9d9d9", alpha = 0.5) +
  annotate("text", x = 3.5,
           y = max(ownership_rate$dogs_per_1000) * 0.85,
           label = "2019-21\nno data",
           size = 2.8, colour = "#525252", hjust = 0.5, lineheight = 0.9) +
  scale_fill_manual(values = borough_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Licensed dogs per 1,000 residents by borough",
    subtitle = "Annual licence extracts; 2019-2021 not published by NYC Open Data",
    x        = "Year",
    y        = "Licensed dogs per 1,000 residents",
    fill     = "Borough",
    caption  = "Sources: NYC Dog Licensing Dataset"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, colour = "#737373"),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank()
  )

# ── Plot 1B: Bite counts line chart ───────────────────────────────────────────

# Add COVID annotation year
covid_year <- 2020

p1b <- ggplot(bites_per_year_borough,
              aes(x = year, y = bite_count,
                  colour = borough, group = borough)) +
  annotate("rect", xmin = 2019.5, xmax = 2020.5,
           ymin = -Inf, ymax = Inf,
           fill = "#ffffcc", alpha = 0.7) +
  annotate("text", x = 2020,
           y = max(bites_per_year_borough$bite_count) * 0.92,
           label = "COVID-19\nDrop",
           size = 2.8, colour = "#525252", hjust = 0.5, lineheight = 0.9) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = borough_colours) +
  scale_x_continuous(breaks = 2016:2023) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Reported dog bite incidents by borough",
    subtitle = "All years 2016-2023; Dog_Bites Dataset covers full period continuously",
    x        = "Year",
    y        = "Number of reported bites",
    colour   = "Borough",
    caption  = "Source: DOHMH Dog Bite Data"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, colour = "#737373"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

# ── Plot 1C: Choropleth — dog density 2022 ────────────────────────────────────

# Use 2022 as the most recent year with large licensing count
dogs_2022 <- dogs_clean |>
  filter(extract_year == 2022) |>
  count(zipcode, name = "dogs_2022")

map_2022 <- master_filtered |>
  left_join(dogs_2022, by = "zipcode") |>
  mutate(
    dogs_2022   = replace_na(dogs_2022, 0),
    density_2022 = dogs_2022 / (as.numeric(ALAND20) / 1e6)
  )

p1c <- ggplot(map_2022) +
  geom_sf(aes(fill = density_2022), colour = "white", linewidth = 0.15) +
  scale_fill_viridis_c(
    option    = "plasma", # color platte choose subjectively. recoure: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
    name      = "Dogs per km\u00B2", # Directly ask Google. search "how to show ^2 in R"
    trans     = "sqrt",           # sqrt transform reduces extreme skew
    na.value  = "#d9d9d9",
    labels    = scales::comma
  ) +
  labs(
    title    = "Dog ownership density across NYC zipcodes (2022)",
    subtitle = "Square-root scale; brighter (yellow) = higher dog concentration",
    caption  = "Sources: NYC Dog Licensing Dataset; US Census TIGER/Line ZCTAs"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "#737373"),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )

# ── Preview each plot individually ────────────────────────────────────────────
print(p1a)
print(p1b)
print(p1c)

# ── Save plots ────────────────────────────────────────────────────────────────
ggsave("plots/plot1a_ownership_rate.png", p1a,
       width = 8, height = 5, dpi = 300, bg = "white")
ggsave("plots/plot1b_bite_counts.png",    p1b,
       width = 8, height = 5, dpi = 300, bg = "white")
ggsave("plots/plot1c_density_map.png",    p1c,
       width = 7, height = 6, dpi = 300, bg = "white")

cat("Q1 plots saved.\n")


# Check gap index inputs
cat("=== Dog density range ===\n")
summary(master_filtered$dog_density)

cat("\n=== n_runs distribution ===\n")
master_filtered |> st_drop_geometry() |>
  count(n_runs) |> arrange(n_runs)

cat("\n=== dogs_per_run for ZCTAs with runs ===\n")
master_filtered |>
  st_drop_geometry() |>
  filter(n_runs > 0) |>
  group_by(borough) |>
  summarise(
    median_dogs_per_run = median(dogs_per_run, na.rm = TRUE),
    total_runs = sum(n_runs),
    n_zctas_with_runs = n()
  ) |>
  arrange(desc(median_dogs_per_run))



# ══════════════════════════════════════════════════════════════════════════════
# Phase 3 — Q2 Visualisations
# ══════════════════════════════════════════════════════════════════════════════

# ── Compute gap index ─────────────────────────────────────────────────────────

master_filtered <- master_filtered |>
  mutate(
    # Normalise both variables 0-1 across all ZCTAs
    density_scaled = (dog_density - min(dog_density, na.rm = TRUE)) /
      (max(dog_density, na.rm = TRUE) - min(dog_density, na.rm = TRUE)),
    runs_scaled    = (n_runs - min(n_runs)) /
      (max(n_runs) - min(n_runs)),
    # Gap index: high density + low run access = high gap
    gap_index = density_scaled * (1 - runs_scaled),
    # Access category for Plot 2A
    run_access     = case_when(
      n_runs == 0 ~ "No runs",
      n_runs == 1 ~ "1 run",
      n_runs >= 2 ~ "2+ runs"
    ) |> factor(levels = c("No runs", "1 run", "2+ runs"))
  )

# Borough colour palette (reuse from Q1)
borough_colours <- c(
  "Manhattan"     = "#3182bd",
  "Brooklyn"      = "#c51b8a",
  "Queens"        = "#feb24c",
  "Bronx"         = "#31a354",
  "Staten Island" = "#e6550d"
)

# ── Plot 2A: Choropleth — dog density + run locations overlaid ────────────────

# Compute run centroids for overlay dots
dog_runs_centroids <- dog_runs_sf |>
  st_transform(st_crs(master_filtered)) |>
  st_centroid()

p2a <- ggplot() +
  geom_sf(data  = master_filtered,
          aes(fill = dog_density),
          colour = "white", linewidth = 0.15) +
  geom_sf(data   = dog_runs_centroids,
          colour = "#1a9850",
          size   = 1.2,
          alpha  = 0.9) +
  scale_fill_viridis_c(
    option   = "plasma",  # same theme as Q1 map
    name     = "Dogs per km\u00B2",
    trans    = "sqrt",
    na.value = "#d9d9d9",
    labels   = scales::comma # It automatically adds commas to large numbers on your Y-axis (e.g., changing 1000 to 1,000).
  ) +
  labs(
    title    = "Dog ownership density and off-leash run locations across NYC",
    subtitle = "Green dots = dog run locations; brighter (yellow) = higher dog density",
    caption  = "Sources: NYC Dog Licensing Dataset; NYC Parks Dog Runs; US Census TIGER/Line"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, colour = "#737373"),
    legend.position  = "right",
    plot.margin      = margin(10, 10, 10, 10)
  )

# ── Plot 2B: Bar chart — dogs per run by borough ──────────────────────────────

# Borough-level summary: total dogs / total runs (only where runs exist)
borough_gap <- master_filtered |>
  st_drop_geometry() |>
  filter(!is.na(borough)) |>
  group_by(borough) |>
  summarise(
    total_dogs = sum(total_dogs),
    total_runs = sum(n_runs),
    pct_zctas_no_run = mean(n_runs == 0) * 100
  )|>
  # n_runs == 0: This creates a list of TRUE (if there are no runs) or FALSE (if there is at least one run) for every ZIP code in that borough.
  # mean(...): In R, TRUE counts as 1 and FALSE counts as 0. When you take the mean of these, you get the proportion (e.g., 0.40 means 40% of the ZIP codes have no runs).
  # * 100: This converts the proportion into a clean percentage (e.g., 0.40 becomes 40.0).
  mutate(
    dogs_per_run = if_else(total_runs > 0,
                           total_dogs / total_runs,
                           NA_real_) # Note: with plain NA, R might show error: "Can't combine double and logical."
                                     # By using NA_real_, explicitly telling R: "Put a missing value here, but make sure it's the decimal-friendly version of NA."
  ) |>
  arrange(desc(dogs_per_run))

p2b <- ggplot(borough_gap |> filter(!is.na(dogs_per_run)),
              aes(x = reorder(borough, dogs_per_run),
                  y = dogs_per_run,
                  fill = borough)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::comma(round(dogs_per_run))),
            hjust  = -0.15,
            size   = 3.5,
            colour = "#525252") +
  coord_flip() + # horizontal bars
  scale_fill_manual(values = borough_colours, guide = "none") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18)), 
    # expension: controls the "breathing room" between your data and the plot axes. 
    # By default, ggplot adds a 5% buffer so points don't touch the edges.
    # 0 (The first number): This is the bottom padding. Setting it to 0 ensures bars sit right on the X-axis line.
    # 0.18 (The second number): This adds 18% extra space at the top. 
    labels = scales::comma
  ) +
  labs(
    title    = "Licensed dogs per off-leash run by borough",
    subtitle = "Higher = more dogs competing for each run space",
    x        = NULL,
    y        = "Licensed dogs per run",
    caption  = "Sources: NYC Dog Licensing Dataset; NYC Parks Dog Runs"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, colour = "#737373"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

# ── Plot 2C: Scatter(jitter) — dog density vs n_runs, coloured by borough ─────────────

# Annotate worst-gap ZCTAs (high density, zero runs)
top_gap <- master_filtered |>
  st_drop_geometry() |>
  filter(!is.na(borough)) |>
  arrange(desc(gap_index)) |>
  slice_head(n = 5)

p2c <- ggplot(
  master_filtered |> st_drop_geometry() |> filter(!is.na(borough)),
  aes(x = dog_density, y = n_runs, colour = borough)
) +
  coord_cartesian(clip = "off") + 
  # Highlight the "high density, no runs" danger zone. Prompt: "How to add a banner highlight the zone with high density but no runs location."
  annotate("rect",
           xmin = median(master_filtered$dog_density, na.rm = TRUE),
           xmax = Inf, ymin = -0.4, ymax = 0.4,
           fill = "#ffffcc", alpha = 0.5) +
  annotate("text",
           x     = max(master_filtered$dog_density, na.rm = TRUE),
           y     = 0.8, # Move up (out of yellow zone)
           label = "High density, no runs zones",
           size  = 3, colour = "#525252", fontface = "bold.italic", hjust = 1) +
  # New Annotation: An arrow pointing up into the yellow box
  annotate("curve",
           x = max(master_filtered$dog_density, na.rm = TRUE) * 0.9, y = 0.7, # Start point
           xend = max(master_filtered$dog_density, na.rm = TRUE) * 0.95, yend = 0.5, # End point
           arrow = arrow(length = unit(0.2, "cm")), 
           curvature = -0.5, colour = "#800026") +
  # Jitter Y only — keeps x coordinates exact on sqrt scale
  geom_point(
    size     = 1.5,
    alpha    = 0.65,
    position = position_jitter(width = 0, height = 0.2, seed = 42)
    # seed = 42 fixes the random positions — same every render
  ) +
  # Labels for top 5
  geom_text_repel(
    data        = top_gap,
    aes(label   = paste0(zipcode, "\n(", round(dog_density), "/km\u00b2)")),
    size        = 2.5,          # Slightly larger for readability
    fontface    = "bold",
    lineheight  = 0.85,
    box.padding = 0.5,          # Space around the text box
    point.padding = 0.3,        # Space between point and label
    min.segment.length = 0,     # Always show the connecting line
    segment.color = "#252525",   # Color of the leader line
    segment.size = 0.2,         # Thin lines to keep it clean
    direction   = "both",       # Allow labels to move up/down/left/right
    force       = 2             # Strength of the "push" between labels
  ) +
  scale_colour_manual(values = borough_colours) +
  scale_x_continuous(
    labels = scales::comma,
    trans  = "sqrt"
  ) +
  scale_y_continuous(
    breaks = 0:4,
    limits = c(-0.5, 4.8)
  ) +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  labs(
    title    = "Dog ownership density vs. number of off-leash runs per ZCTA",
    subtitle = "Top 5 highest gap-index zipcodes labelled with density; vertical jitter applied to show overlapping points; x-axis square-root scaled",
    x        = "Dog density (dogs per km\u00b2, sqrt scale)",
    y        = "Number of off-leash runs",
    colour   = "Borough",
    caption  = "Sources: NYC Dog Licensing Dataset; NYC Parks Dog Runs"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, colour = "#737373"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank() # Refer: https://r-charts.com/ggplot2/grid/. My aim is remove too many grid lines, so it looks more "clean". Then Google AI taught me search "r grid"
  )

# ── Plot 2D: Gap index choropleth ─────────────────────────────────────────────

p2d <- ggplot(master_filtered) +
  geom_sf(aes(fill = gap_index),
          colour = "white", linewidth = 0.15) +
  scale_fill_viridis_c(
    option   = "inferno",
    name     = "Gap index",
    na.value = "#d9d9d9"
  ) +
  labs(
    title    = "Infrastructure gap index across NYC zipcodes",
    subtitle = "High score = high dog density with few or no off-leash spaces",
    caption  = "Gap index = normalised dog density * (1 - normalised run access)"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 9, colour = "#737373"),
    legend.position = "right",
    plot.margin     = margin(10, 10, 10, 10)
  )

# ── Print and save all ────────────────────────────────────────────────────────

print(p2a)
print(p2b)
print(p2c)
print(p2d)

ggsave("plots/plot2a_density_runs_overlay.png", p2a,
       width = 7, height = 6, dpi = 300, bg = "white")
ggsave("plots/plot2b_dogs_per_run_borough.png", p2b,
       width = 7, height = 4, dpi = 300, bg = "white")
ggsave("plots/plot2c_density_vs_runs_scatter.png", p2c,
       width = 8, height = 5, dpi = 300, bg = "white")
ggsave("plots/plot2d_gap_index_map.png", p2d,
       width = 7, height = 6, dpi = 300, bg = "white")

cat("Q2 plots saved.\n")


# Check all saved plots exist
plots_expected <- c(
  "plots/plot1a_ownership_rate.png",
  "plots/plot1b_bite_counts.png", 
  "plots/plot1c_density_map.png",
  "plots/plot2a_density_runs_overlay.png",
  "plots/plot2b_dogs_per_run_borough.png",
  "plots/plot2c_density_vs_runs_scatter.png",
  "plots/plot2d_gap_index_map.png"
)

cat("=== Plot files check ===\n")
for (f in plots_expected) {
  exists <- file.exists(f)
  size   <- if (exists) paste0(round(file.size(f)/1024), " KB") else "MISSING"
  cat(sprintf("%-45s %s\n", f, size))
}