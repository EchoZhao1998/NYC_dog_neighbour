# ══════════════════════════════════════════════════════════════════════════════
# Phase 3 — Q3 Visualisations
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(sf)
library(ggrepel)
library(spdep)
library(viridis)
library(scales)

# ── Prepare Q3 data ───────────────────────────────────────────────────────────

q3_data <- master_filtered |>
  st_drop_geometry() |>
  filter(!is.na(bite_rate), !is.na(median_income), !is.na(borough)) |>
  mutate(
    income_quartile = ntile(median_income, 4),
    income_label    = case_when(
      income_quartile == 1 ~ "Q1: Low income",
      income_quartile == 2 ~ "Q2: Lower-middle",
      income_quartile == 3 ~ "Q3: Upper-middle",
      income_quartile == 4 ~ "Q4: High income"
    ) |> factor(levels = c("Q1: Low income", "Q2: Lower-middle",
                           "Q3: Upper-middle", "Q4: High income")),
    run_access = case_when(
      n_runs == 0 ~ "No runs",
      n_runs == 1 ~ "1 run",
      n_runs >= 2 ~ "2+ runs"
    ) |> factor(levels = c("No runs", "1 run", "2+ runs"))
  )

borough_colours <- c(
  "Manhattan"     = "#3182bd",
  "Brooklyn"      = "#c51b8a",
  "Queens"        = "#feb24c",
  "Bronx"         = "#31a354",
  "Staten Island" = "#e6550d"
)

# ── Figure 8: Bite rate choropleth ────────────────────────────────────────────

p3a <- ggplot(master_filtered |> filter(!is.na(bite_rate))) +
  geom_sf(aes(fill = bite_rate),
          colour = "white", linewidth = 0.15) +
  scale_fill_viridis_c(
    option   = "rocket",
    direction = -1, # 1 - from dark to light. -1 converse
    name = "Bites per\n1,000 dogs",
    na.value = "#d9d9d9",
    trans = "sqrt"
  ) +
  labs(
    title = "Dog bite rate across NYC zipcodes",
    subtitle = "Bites per 1,000 licensed dogs (2022 denominator); sqrt scale",
    caption = "Sources: DOHMH Dog Bite Data; NYC Dog Licensing Dataset"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "#737373"),
    legend.position = "right"
  )

ggsave("plots/plot3a_bite_rate_map.png", p3a,
       width = 7, height = 6, dpi = 300, bg = "white")

# ── Figure 9: Dog density vs bite rate + Spearman annotation ─────────────────

# Spearman test for annotation
sp_density <- cor.test(q3_data$dog_density, q3_data$bite_rate,
                       method = "spearman")
sp_income  <- cor.test(q3_data$median_income, q3_data$bite_rate,
                       method = "spearman")

rho_label <- paste0("Spearman \u03c1 = ",
                    round(sp_density$estimate, 3),
                    ", p < 0.001")

p3b <- ggplot(q3_data,
              aes(x = dog_density, y = bite_rate, colour = borough)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE,
              colour = "#525252", linewidth = 0.8,
              fill = "#d9d9d9", alpha = 0.4) +
  annotate("text",
           x     = max(q3_data$dog_density, na.rm = TRUE) * 0.95,
           y     = max(q3_data$bite_rate,   na.rm = TRUE) * 0.95,
           label = rho_label,
           size  = 3.2, hjust = 1, colour = "#525252", fontface = "italic") +
  scale_colour_manual(values = borough_colours) +
  scale_x_continuous(labels = comma, trans = "sqrt") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Dog ownership density vs. bite rate per ZCTA",
    subtitle = paste0("Spearman \u03c1 = ",
                      round(sp_density$estimate, 3),
                      " indicates moderate negative relationship"),
    x        = "Dog density (dogs per km\u00b2, sqrt scale)",
    y        = "Bite rate (per 1,000 dogs)",
    colour   = "Borough",
    caption  = "Sources: DOHMH Dog Bite Data; NYC Dog Licensing Dataset"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, colour = "#737373"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("plots/plot3b_density_vs_biterate.png", p3b,
       width = 8, height = 5, dpi = 300, bg = "white")

# ── Figure 10: Bite rate by run access category ───────────────────────────────

p3c <- ggplot(q3_data,
              aes(x = run_access, y = bite_rate, fill = run_access)) +
  geom_violin(alpha = 0.6, trim = TRUE) + # reference: https://www.atlassian.com/data/charts/violin-plot-complete-guide
  geom_boxplot(width = 0.15, outlier.size = 1,
               fill = "white", alpha = 0.8) +
  scale_fill_manual(
    values = c("No runs" = "#d7191c",
               "1 run"   = "#fdae61",
               "2+ runs" = "#1a9641"),
    guide  = "none"
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Bite rate by off-leash run access category",
    subtitle = "Violin + boxplot; each point is one ZCTA",
    x        = "Off-leash run access",
    y        = "Bite rate (per 1,000 dogs)",
    caption  = "Sources: DOHMH Dog Bite Data; NYC Dog Licensing Dataset; NYC Parks Dog Runs"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, colour = "#737373"),
    panel.grid.minor = element_blank()
  )

ggsave("plots/plot3c_runs_vs_biterate.png",    p3c,
       width = 7, height = 5, dpi = 300, bg = "white")

# ── Kruskal-Wallis + pairwise Wilcoxon ───────────────────────────────────────

kruskal_result <- kruskal.test(
  bite_rate ~ run_access,
  data = q3_data
)
cat("=== Kruskal-Wallis test ===\n")
print(kruskal_result)

pairwise_result <- pairwise.wilcox.test(
  q3_data$bite_rate,
  q3_data$run_access,
  p.adjust.method = "bonferroni"
)
cat("\n=== Pairwise Wilcoxon (Bonferroni corrected) ===\n")
print(pairwise_result)

# Descriptive statistics per group — for report table
cat("\n=== Median bite rate by run access ===\n")
q3_data |>
  group_by(run_access) |>
  summarise(
    n          = n(),
    median     = round(median(bite_rate, na.rm = TRUE), 1),
    q25        = round(quantile(bite_rate, 0.25, na.rm = TRUE), 1),
    q75        = round(quantile(bite_rate, 0.75, na.rm = TRUE), 1),
    max        = round(max(bite_rate, na.rm = TRUE), 1)
  )


# ── Figure 11: Faceted scatter — density vs bite rate by income quartile ──────

p3d <- ggplot(q3_data,
              aes(x = dog_density, y = bite_rate,
                  colour = median_income)) +
  geom_point(size = 2, alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE,
              colour = "#525252", linewidth = 0.7) +
  facet_wrap(~income_label, nrow = 2) +
  scale_colour_viridis_c(
    option = "plasma",
    name   = "Median\nincome ($)",
    labels = comma
  ) +
  scale_x_continuous(labels = comma, trans = "sqrt") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Dog density vs. bite rate by neighbourhood income quartile",
    subtitle = "Each panel = one income group; colour intensity = income level within panel",
    x        = "Dog density (dogs per km\u00b2, sqrt scale)",
    y        = "Bite rate (per 1,000 dogs)",
    caption  = "Sources: DOHMH Dog Bite Data; NYC Dog Licensing Dataset; ACS 2022"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, colour = "#737373"),
    legend.position  = "right",
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 10)
  )


ggsave("plots/plot3d_income_facet.png",        p3d,
       width = 9, height = 7, dpi = 300, bg = "white")

cat("Q3 plots saved.\n")