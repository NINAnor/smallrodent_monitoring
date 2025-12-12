#### BARPLOT WITH PROPORTION OF MALFUNCTIONING CAMERAS
library(tidyverse)

setwd("C:/Users/albert.chacon/OneDrive - NINA/Documents/Rodent exploratory analyses 2025")

data <- read_tsv("data/merged_number_of_observations_all_sites_names.txt")    # for tab-delimited

# Ensure UTF-8 encoding for Norwegian letters (å, ø, æ)
Sys.setlocale("LC_CTYPE", "nb_NO.UTF-8")

# ---- Calculate proportion of bad_quality images (weighted by v_abundance) ----
bad_quality_summary <- data %>%
  group_by(sn_locality, t_year_month, v_class_id) %>%
  summarise(total_abundance = sum(v_abundance, na.rm = TRUE), .groups = "drop") %>%
  group_by(sn_locality, t_year_month) %>%
  summarise(
    total_obs = sum(total_abundance, na.rm = TRUE),
    bad_quality = sum(total_abundance[v_class_id == "bad_quality"], na.rm = TRUE),
    prop_bad = bad_quality / total_obs * 100,
    .groups = "drop"
  )

# ---- Order factors for consistent plotting ----
bad_quality_summary <- bad_quality_summary %>%
  mutate(
    sn_locality = factor(sn_locality, levels = unique(sn_locality)),
    # Standardize month format (replace underscores or dots with hyphens)
    t_year_month_clean = str_replace_all(t_year_month, "[_.]", "-"),
    # Convert to real date (YYYY-MM-01)
    t_year_month_clean = if_else(
      str_detect(t_year_month_clean, "^\\d{4}-\\d{1,2}$"),
      paste0(t_year_month_clean, "-01"),
      t_year_month_clean
    ),
    t_year_month = factor(
      t_year_month,
      levels = unique(t_year_month[order(as.Date(t_year_month_clean, format = "%Y-%m-%d"))])
    )
  ) %>%
  select(-t_year_month_clean)

# ---- BARPLOT ----
p_bar <- ggplot(bad_quality_summary, aes(x = t_year_month, y = prop_bad, fill = prop_bad)) +
  geom_col(color = "black", width = 0.8) +
  geom_text(
    aes(label = sprintf("%.1f", prop_bad)),
    vjust = -0.4,
    size = 3.5
  ) +
  scale_fill_gradientn(
    name = "Andel dårlige bilder (%)",
    colours = c("#b9f6ca", "#fff9c4", "#ffe0b2", "#ffccbc"),
    values = scales::rescale(c(0, 10, 30, 50, 100)),
    limits = c(0, 100),
    na.value = "grey90"
  ) +
  labs(
    title = "Prosentandel dårlige bilder per region og måned",
    x = "År–måned",
    y = "Andel dårlige bilder (%)"
  ) +
  facet_wrap(~ sn_locality, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14, base_family = "DejaVu Sans") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

# ---- Display the plot ----
print(p_bar)

# ---- Save to 'plot' folder ----
if (!dir.exists("plot")) dir.create("plot")

file_name_bar <- "plot/prosentandel_darlige_bilder_per_region_og_maned_barplot.png"

ggsave(
  filename = file_name_bar,
  plot = p_bar,
  width = 10,
  height = 8,
  dpi = 300,
  type = "cairo-png"
)

message("Barplot saved as: ", file_name_bar)
