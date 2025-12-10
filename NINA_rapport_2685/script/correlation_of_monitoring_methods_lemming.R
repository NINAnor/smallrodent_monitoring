library(dplyr)
library(ggplot2)
library(patchwork)

# set WD
setwd("C:/Users/eivind.kleiven/OneDrive - NINA/GitProjects/Rodent_obs_snap_live")

Sys.setlocale("LC_ALL", "Norwegian")

# --- 1 FORMAT OBSERVATION DATA (no scaling yet) --------------------
#import obs data
dat_obs <- read.csv("data/Rodent_data_finnmark.csv", sep=";")

# Summarize data: proportion of SettSmagnager == 1 per year and Rapporteringsniva
dat_summary <- dat_obs %>%
  filter(!Rapporteringsniva=="Inner") %>%
  mutate(Rapporteringsniva = recode(Rapporteringsniva,
                                    "West/Coast" = "Vest",
                                    "East" = "Øst"))%>%
  group_by(Aar, Rapporteringsniva) %>%
  summarise(
    n = n(),
    n_smag = sum(SettSmagnager == 1, na.rm = TRUE),
    prop_smag = n_smag / n,
    .groups = "drop"
  )

dat_obs_formatted <- dat_summary %>%
  mutate(
    method = "observation",
    value = prop_smag    # raw proportion
  ) %>%
  select(Aar, Rapporteringsniva, value, method)

# --- 3 FORMAT SNAP TRAP DATA --------------------------------------
# impport storkalafangst
dat_snap <- read.table("data/storskala.txt")

dat_snap_formatted <- dat_snap %>%
  filter(!V2=="year")%>%
  filter(V1 == "fall") %>%
  mutate(
    year = as.numeric(V2),
    plot = V3,
    sum_snap = as.numeric(V4)) %>%   #only lemming
  group_by(year) %>%
  summarise(
    value = mean(sum_snap, na.rm = TRUE),  # mean per plot
    .groups = "drop"
  ) %>%
  mutate(
    Aar = year,
    Rapporteringsniva = "Øst",
    method = "snaptrap"
  ) %>%
  select(Aar, Rapporteringsniva, value, method)

dat_snap_formatted

# --- 3 FORMAT SNAP TRAP DATA Joatka --------------------------------------
# impport storkalafangst

species <- c("lem_lem")

dat_snap_jotka <- read.table("data/Jotka_1986_2023.txt", header = FALSE)

dat_snap_jotka <- dat_snap_jotka[-1, ]

dat_snap_jotka <- dat_snap_jotka %>%
  rename(
    t_year = V1,
    t_season = V2,
    sn_site = V3,
    sn_section = V4,
    sc_type_of_sites_ecological = V5,
    v_species = V6,
    v_abundance = V7,
    trapnights = V8,
    index = V9
  ) %>%
  filter(t_season == "fall") %>%
  filter(v_species %in% c("myo_ruf", "myo_rut", "mic_oec", "mic_agr", "lem_lem")) %>%
  mutate(
    t_year = as.integer(t_year),
    t_season = tolower(t_season),
    index = as.numeric(index)
  ) %>%
  group_by(t_year, v_species) %>%
  summarise(mean_index = mean(index, na.rm = TRUE)) %>%
  ungroup()

dat_snap_jotka_lem <- dat_snap_jotka %>%
  filter(v_species == "lem_lem")

dat_snap_jotka_tot <- dat_snap_jotka %>%
  group_by(t_year) %>%
  summarise(tot = sum(mean_index, na.rm = TRUE)) %>%
  ungroup()

dat_snap_jotka_formatted <- dat_snap_jotka_tot %>%
  mutate(
    Aar = t_year,
    Rapporteringsniva = "Vest",  # Adjust if Jotka belongs to another region
    method = "snaptrap",
    value = tot
  ) %>%
  select(Aar, Rapporteringsniva, value, method)

# --- 4 COMBINE ALL DATA -------------------------------------------
dat_all <- bind_rows(dat_obs_formatted, dat_snap_formatted,dat_snap_jotka_formatted)

# --- 5 Scale observation data --------------------------------------
dat_all_scaled <- dat_all %>%
  group_by(Rapporteringsniva) %>%
  mutate(
    # find max for cmr and snaptrap (may be missing in some regions)
    max_snap = ifelse(any(method == "snaptrap"), max(value[method == "snaptrap"], na.rm = TRUE), NA),
    
    # choose which max to use for scaling observations
    max_ref = case_when(
      !is.na(max_snap) ~ max_snap, # otherwise use snaptrap
      TRUE ~ NA_real_              # fallback
    ),
    
    # scale observations relative to their own max
    value_scaled = if_else(
      method == "observation",
      value * max_ref / max(value[method == "observation"], na.rm = TRUE),
      value
    )
  ) %>%
  ungroup()

# --- 6 DEFINE COLOR PALETTE --------------------------------------
method_colors <- c(
  "observation" = "#E69F00",  # orange
  #"cmr"         = "#56B4E9",  # blue
  "snaptrap"    = "#009E73"   # green
)


# --- 8 Make plot function ---
plot_region <- function(region_name) {
  ggplot(
    dat_all_scaled %>% filter(Rapporteringsniva == region_name),
    aes(x = Aar, y = value_scaled, color = method)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = method_colors) +
    scale_linetype_manual(values = c("observation" = "dashed", "snaptrap" = "solid")) +
    scale_x_continuous(limits = c(2009, 2025), breaks = seq(2009, 2025, 2)) +
    labs(
      title = region_name,
      x = "",
      y = "Antall fanget ",
      color = "Metode",
      linetype = "Metode"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 14)
    )
}

# --- 9 MAKE PLOTS -----------------------------------------------
plot_east  <- plot_region("Øst")
plot_west  <- plot_region("Vest")

library(cowplot)

# Combine with the shared legend
combined_plot <- (
  plot_east + theme(legend.position = "none")
) / 
#(
#  plot_inner + theme(legend.position = "none")
#) / 
(
  plot_west + theme(legend.position = "bottom")
)

combined_plot

ggsave("comparison_methods_lem.png", plot = combined_plot, width = 8, height = 6, dpi = 300)
