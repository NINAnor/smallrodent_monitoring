# ==== LOAD LIBRARIES ====
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tidyr)
library(readr)
library(lubridate)

setwd("C:/Users/albert.chacon/OneDrive - NINA/Documents/Replicates analysis")

# ==== INPUT / SETTINGS ====
df <- read_delim("Data/number_of_observations_per_month_snohetta_2024.txt", delim = "\t")
site_name <- "Snøhetta"

# Ensure UTF-8 encoding for Norwegian letters (å, ø, æ)
Sys.setlocale("LC_CTYPE", "nb_NO.UTF-8")

# Theme default
theme_set(theme_minimal(base_family = "sans"))

# ==== DEFINE TAXA ====
taxa <- c("lem_lem" = "Lemen" , "cricetidae" ="Mus" )

# ==== CREATE FOLDERS FOR OUTPUT ====
plot_dir <- file.path("plots", site_name)
table_dir <- file.path("tables", site_name)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# ==== EXTRACT SECTION–HABITAT INFO ====
df <- df %>%
  mutate(section_hab = str_extract(sn_site, "(l|m)_(he|hu|sn)"))

section_hab_list <- unique(df$section_hab)
cat("Found section–habitat combinations:", paste(section_hab_list, collapse = ", "), "\n")

# ==== MAIN ANALYSIS (build monthly totals for 6-cam and all 4-cam combinations) ====
site_summary_list <- list()
all_monthly_4 <- list()
all_monthly_6 <- list()

for (target_group in section_hab_list) {
  cat("\nProcessing", target_group, "...\n")
  
  df_sub <- df %>% filter(section_hab == target_group)
  cam_ids <- unique(df_sub$sn_site)
  
  if (length(cam_ids) < 6) {
    cat("Skipping", target_group, "- fewer than 6 cameras.\n")
    next
  }
  
  # --- 6-camera dataset ---
  monthly_6 <- df_sub %>%
    filter(v_class_id %in% names(taxa)) %>%
    group_by(t_year_month, v_class_id) %>%
    summarise(v_abundance = sum(v_abundance, na.rm = TRUE), .groups = "drop") %>%
    mutate(setup = "6-camera", section_hab = target_group)
  
  all_monthly_6[[target_group]] <- monthly_6
  
  # --- 4-camera combinations ---
  combs <- combn(cam_ids, 4, simplify = FALSE)
  monthly_list <- list()
  
  for (i in seq_along(combs)) {
    cams <- combs[[i]]
    sub4 <- df_sub %>% filter(sn_site %in% cams)
    
    monthly_list[[i]] <- sub4 %>%
      filter(v_class_id %in% names(taxa)) %>%
      group_by(t_year_month, v_class_id) %>%
      summarise(v_abundance = sum(v_abundance, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        combination_id = i,
        kept_cameras = paste(cams, collapse = ";"),
        section_hab = target_group
      )
  }
  
  monthly_4 <- bind_rows(monthly_list)
  all_monthly_4[[target_group]] <- monthly_4
  
  # optional: store summary
  range_4 <- monthly_4 %>%
    group_by(t_year_month, v_class_id, section_hab) %>%
    summarise(
      min_v_abundance = min(v_abundance, na.rm = TRUE),
      max_v_abundance = max(v_abundance, na.rm = TRUE),
      .groups = "drop"
    )
  
  combined <- full_join(monthly_6, range_4,
                        by = c("t_year_month", "v_class_id", "section_hab"))
  site_summary_list[[target_group]] <- combined
}

# ==== COMBINE ALL DATA ACROSS SECTIONS ====
all_monthly_4 <- bind_rows(all_monthly_4)
all_monthly_6 <- bind_rows(all_monthly_6)

# ==== SUM OVER SECTION–HAB COMBINATIONS ====
monthly_6_all <- all_monthly_6 %>%
  group_by(t_year_month, v_class_id) %>%
  summarise(v_abundance = sum(v_abundance, na.rm = TRUE), .groups = "drop")

monthly_4_all <- all_monthly_4 %>%
  group_by(t_year_month, v_class_id, combination_id) %>%
  summarise(v_abundance = sum(v_abundance, na.rm = TRUE), .groups = "drop")

# ==== MEAN 4-CAMERA VALUES (ACROSS COMBINATIONS) ====
monthly_4_mean_all <- monthly_4_all %>%
  group_by(t_year_month, v_class_id) %>%
  summarise(mean_v_abundance = mean(v_abundance, na.rm = TRUE), .groups = "drop")

# ==== LABELS ====
monthly_6_all <- monthly_6_all %>% mutate(taxon_label = recode(v_class_id, !!!taxa))
monthly_4_all <- monthly_4_all %>% mutate(taxon_label = recode(v_class_id, !!!taxa))
monthly_4_mean_all <- monthly_4_mean_all %>% mutate(taxon_label = recode(v_class_id, !!!taxa))

# ==== CREATE PER-CAMERA COLUMNS ====
monthly_4_all <- monthly_4_all %>%
  mutate(v_abundance_per_camera = v_abundance / 4,
         combination_id = as.character(combination_id))

monthly_4_mean_all <- monthly_4_mean_all %>%
  mutate(mean_v_abundance_per_camera = mean_v_abundance / 4)

monthly_6_all <- monthly_6_all %>%
  mutate(v_abundance_per_camera = v_abundance / 6)

# ==== BUILD RANGE (min/max) ACROSS 15 COMBINATIONS (per camera) ====
range_4_percam <- monthly_4_all %>%
  group_by(t_year_month, v_class_id, taxon_label) %>%
  summarise(
    min_percam = min(v_abundance_per_camera, na.rm = TRUE),
    max_percam = max(v_abundance_per_camera, na.rm = TRUE),
    .groups = "drop"
  )

# ==== COMBINE VALUES INTO A TABLE (for saving/inspection) ====
combined_summary_percam <- range_4_percam %>%
  left_join(
    monthly_4_mean_all %>% select(t_year_month, v_class_id, mean_v_abundance_per_camera),
    by = c("t_year_month", "v_class_id")
  ) %>%
  left_join(
    monthly_6_all %>% select(t_year_month, v_class_id, v_abundance_per_camera),
    by = c("t_year_month", "v_class_id")
  ) %>%
  rename(
    mean4_percam = mean_v_abundance_per_camera,
    sixcam_percam = v_abundance_per_camera
  ) %>%
  select(t_year_month, v_class_id, taxon_label, min_percam, max_percam, mean4_percam, sixcam_percam) %>%
  arrange(v_class_id, t_year_month)

# Save to CSV for inspection
write_csv(combined_summary_percam, file.path(table_dir, "range_mean6cam_percamera_summary.csv"))
cat("Saved range/mean/6cam per-camera summary to:", file.path(table_dir, "range_mean6cam_percamera_summary.csv"), "\n")

# ==== PLOTTING: GREEN LINES for each 4-camera combination, plus red & dotted-blue lines on top ====

# Prepare month ordering (same logic as before)
parse_month <- function(x) {
  x2 <- gsub("_", "-", x)
  x2 <- gsub("\\.", "-", x2)
  xdate <- suppressWarnings(ymd(paste0(x2, "-01")))
  if (all(is.na(xdate))) return(NULL)
  tibble(orig = x, date = xdate)
}

m4 <- unique(monthly_4_all$t_year_month)
m6 <- unique(monthly_6_all$t_year_month)
m_all <- sort(unique(c(m4, m6)))

parsed <- parse_month(m_all)
if (!is.null(parsed)) {
  ordered_months <- parsed %>% arrange(date) %>% pull(orig)
} else {
  ordered_months <- sort(m_all)
}

monthly_4_all <- monthly_4_all %>% mutate(t_year_month = factor(t_year_month, levels = ordered_months))
monthly_6_all <- monthly_6_all %>% mutate(t_year_month = factor(t_year_month, levels = ordered_months))
monthly_4_mean_all <- monthly_4_mean_all %>% mutate(t_year_month = factor(t_year_month, levels = ordered_months))

# Defensive: remove NA months if any
monthly_4_all <- filter(monthly_4_all, !is.na(t_year_month))
monthly_4_mean_all <- filter(monthly_4_mean_all, !is.na(t_year_month))
monthly_6_all <- filter(monthly_6_all, !is.na(t_year_month))

# Plot: green lines for each combination (per camera), dotted blue mean, solid red 6cam
p_lines <- ggplot() +
  # 15 green lines (one per combination), using per-camera values
  geom_line(
    data = monthly_4_all,
    aes(x = t_year_month, y = v_abundance_per_camera, group = interaction(combination_id, taxon_label)),
    color = "forestgreen", alpha = 0.5, size = 0.7
  ) +
  # Solid red line = 6-camera per camera
  geom_line(
    data = monthly_6_all,
    aes(x = t_year_month, y = v_abundance_per_camera, group = taxon_label),
    color = "red3", size = 1.6
  ) +
  # Dotted blue line = mean of 15 four-camera combinations per camera (plotted on top)
  geom_line(
    data = monthly_4_mean_all,
    aes(x = t_year_month, y = mean_v_abundance_per_camera, group = taxon_label),
    color = "blue3", size = 1.1, linetype = "dotted"
  ) +
  facet_wrap(~taxon_label, scales = "free_y") +
  labs(
    title = paste("Observasjoner per kamera – 4-kamera vs. 6-kamera —", site_name),
    subtitle = "Grønne linjer: 15 firekamerakombinasjoner. Blå stiplet linje: snitt for 4-kamera. Rød linje: 6-kamera.",
    x = "Måned",
    y = "Observasjoner per måned per kamera"
  ) +
  theme(
    text = element_text(family = "DejaVu Sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    strip.text = element_text(size = 13, face = "bold")
  )

print(p_lines)

# Save plot
ggsave(
  filename = file.path(plot_dir, paste0("lines4cam_vs_6cam", site_name, ".png")),
  plot = p_lines,
  width = 11, height = 6.5, dpi = 300, type = "cairo-png"
)

cat("Saved plot to:", file.path(plot_dir, paste0("4cam_vs_6cam", site_name, ".png")), "\n")
cat("Summary table saved to:", file.path(table_dir, "range_mean6cam_percamera_summary.csv"), "\n")
