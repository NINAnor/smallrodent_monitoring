library(tidyverse)

setwd("C:/Users/albert.chacon/OneDrive - NINA/Documents/Rodent exploratory analyses 2025")

data <- read_tsv("data/merged_number_of_observations_all_sites.txt")

head(data)
str(data)

Sys.setlocale("LC_CTYPE", "nb_NO.UTF-8")

# Keep only the taxa of interest
taxa <- c("cricetidae", "lem_lem")

data_clean <- data %>%
  filter(v_class_id %in% taxa)

##############
## vinter vs sommer habitatbruk
library(tidyverse)

# Filter and combine section + habitat
data_habitat <- data_clean %>%
  filter(sn_locality == "All") %>%
  mutate(section_habitat = paste(sc_section, sc_type_of_sites_ecological, sep = " - "))

# Define seasons
winter_months <- c(10:12, 1:5)
summer_months <- 6:9

data_habitat <- data_habitat %>%
  mutate(season = case_when(
    t_month %in% winter_months ~ "Vinter (Okt–Mai)",
    t_month %in% summer_months ~ "Sommer (Jun–Sep)"
  ))

# Summarize abundance per taxon, habitat, and season
habitat_season <- data_habitat %>%
  group_by(v_class_id, season, section_habitat) %>%
  summarise(total_abundance = sum(v_abundance), .groups = "drop") %>%
  group_by(v_class_id, season) %>%
  mutate(perc = total_abundance / sum(total_abundance) * 100)

# -------------------------
# TRANSLATE TAXON NAMES HERE
# -------------------------
taxon_labels <- c(
  "lem_lem" = "Lemen",
  "cricetidae" = "Mus"
)

habitat_season <- habitat_season %>%
  mutate(taxon_label = recode(v_class_id, !!!taxon_labels))

# Translate habitat labels
habitat_season <- habitat_season %>%
  mutate(section_habitat = case_when(
    section_habitat == "low_alpine_zone - heath" ~ "lavalpin sone - hei",
    section_habitat == "low_alpine_zone - hummock_mire" ~ "lavalpin sone - tuemark",
    section_habitat == "low_alpine_zone - snowbed" ~ "lavalpin sone - snøleie",
    section_habitat == "middle_alpine_zone - hummock_mire" ~ "mellomalpin sone - tuemark",
    section_habitat == "middle_alpine_zone - snowbed" ~ "mellomalpin sone - snøleie",
    TRUE ~ section_habitat
  ))

habitat_season$section_habitat <- factor(
  habitat_season$section_habitat,
  levels = c(
    "lavalpin sone - hei",
    "lavalpin sone - tuemark",
    "lavalpin sone - snøleie",
    "mellomalpin sone - tuemark",
    "mellomalpin sone - snøleie"
  )
)

# Compute sample sizes (n)
sample_sizes <- habitat_season %>%
  group_by(v_class_id, season) %>%
  summarise(n = sum(total_abundance), .groups = "drop") %>%
  mutate(taxon_label = recode(v_class_id, !!!taxon_labels))  # translate names


# ---- Stacked barplot ----
p <- ggplot(habitat_season, aes(x = taxon_label, y = perc, fill = section_habitat)) +
  geom_bar(stat = "identity") +
  geom_text(
    data = sample_sizes,
    mapping = aes(x = taxon_label, y = 105, label = paste0("n = ", n)),
    size = 5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  facet_wrap(~season) +
  labs(
    title = "Sesongbasert habitatbruk etter takson",
    x = "Takson",
    y = "Prosentandel av totale observasjoner",
    fill = "Seksjon - habitat"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)
  ) +
  coord_cartesian(ylim = c(0, 110))

print(p)

# Save to file
if (!dir.exists("plot")) dir.create("plot")

title_text <- ggplot_build(p)$plot$labels$title
file_name <- paste0(
  "plot/",
  gsub("[^[:alnum:]_]+", "_", tolower(title_text)),
  ".png"
)

ggsave(
  filename = file_name,
  plot = p,
  width = 10,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

message("Plot saved as: ", file_name)
