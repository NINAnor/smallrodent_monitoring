library(ggplot2)
library(tidyverse)
library(lubridate)

setwd("C:/Users/albert.chacon/OneDrive - NINA/Documents/Rodent exploratory analyses 2025")

data <- read_tsv("data/merged_number_of_observations_all_sites.txt")    # for tab-delimited
head(data)
str(data)

# Ensure UTF-8 encoding for Norwegian letters (å, ø, æ)
Sys.setlocale("LC_CTYPE", "nb_NO.UTF-8")

# Keep only the taxa of interest and remove bad_quality/empty rows
taxa <- c("cricetidae", "lem_lem")

data_clean <- data %>%
  filter(v_class_id %in% taxa)

# ---- Define place name ----
place_name <- "alle steder"  # <-- Change this for each site

# ---- Create 'plot' folder if it doesn't exist ----
if (!dir.exists("plot")) {
  dir.create("plot")
}

# ---- TAXON NAME LOOKUP (NORWEGIAN) ----
taxon_labels <- c(
  "lem_lem" = "Lemen",
  "cricetidae" = "Mus"
)


# ---- Monthly abundance at site scale ----
site_data <- data_clean %>%
  filter(sn_locality == "All") %>%
  group_by(t_year, t_month, v_class_id) %>%
  summarise(monthly_abundance = sum(v_abundance), .groups = "drop") %>%
  mutate(
    date = as.Date(paste0(t_year, "-", t_month, "-01")),
    taxon_label = recode(v_class_id, !!!taxon_labels)   # <-- APPLY NORWEGIAN NAMES
  )

p1 <- ggplot(site_data, aes(x = date, y = monthly_abundance, color = taxon_label)) +
  geom_line(aes(group = taxon_label), size = 1.5) +
  geom_point(size = 3) +
  labs(
    title = paste("Månedsobservasjoner av lemen og mus i", place_name),
    x = "Dato", y = "Antall observasjoner", color = "Takson"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

# Save the first plot
ggsave(
  filename = paste0("plot/", place_name, "_site_monthly_abundance.png"),
  plot = p1,
  width = 10, height = 6, dpi = 300
)



# ---- Monthly abundance per habitat and section ----

# Create lookup tables for the translations
habitat_labels <- c(
  heath = "hei",
  hummock_mire = "tuemark",
  snowbed = "snøleie"
)

section_labels <- c(
  low_alpine_zone = "lavalpin sone",
  middle_alpine_zone = "mellomalpin sone"
)

monthly_abundance <- data_clean %>%
  filter(sn_locality == "All") %>%
  group_by(sc_section, sc_type_of_sites_ecological, t_year, t_month, v_class_id) %>%
  summarise(monthly_abundance = sum(v_abundance), .groups = "drop") %>%
  mutate(
    date = as.Date(paste0(t_year, "-", t_month, "-01")),
    taxon_label = recode(v_class_id, !!!taxon_labels)   # <-- APPLY NORWEGIAN NAMES
  )


p2 <- ggplot(monthly_abundance, aes(x = date, y = monthly_abundance, color = taxon_label)) +
  geom_line(aes(group = taxon_label), size = 1.2) +
  geom_point(size = 2.5) +
  facet_grid(
    sc_section ~ sc_type_of_sites_ecological,
    scales = "free_y",
    labeller = labeller(
      sc_type_of_sites_ecological = habitat_labels,
      sc_section = section_labels
    )
  ) +
  labs(
    title = paste("Observasjoner per habitat og seksjon i", place_name),
    x = "Dato", y = "Antall observasjoner", color = "Takson"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    strip.background = element_blank(),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20)
  )

# Save the second plot
ggsave(
  filename = paste0("plot/", place_name, "_habitat_section_abundance.png"),
  plot = p2,
  width = 12, height = 8, dpi = 300, type = "cairo-png"
)
