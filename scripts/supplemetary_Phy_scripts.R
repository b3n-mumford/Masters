# ==========================================================
# Plot: element bars by species, shaded facet background by class
# Inputs:
#   - tray_element_means.csv  (tray/specimen-level means; uses mean_at)
#   - species_phylogeny.csv   (species -> class, feeding_guild)
# Output:
#   - plot object p
#   - optional saved PNG
# ==========================================================
setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(readr)
  library(ggnewscale)  # new_scale_fill()
})

# ---- 1) Paths (edit if needed) ----
tray_means_path <- "Data/processed/tray_element_means.csv"
phylogeny_path  <- "Data/metadata/species_phylogeny.csv"

# ---- 2) Read data ----
tray_element_means <- readr::read_csv(tray_means_path, show_col_types = FALSE) %>%
  janitor::clean_names()

species_phylogeny <- readr::read_csv(phylogeny_path, show_col_types = FALSE) %>%
  janitor::clean_names()

# ---- 3) Minimal checks + cleanup ----
req_tray <- c("species", "tray_id", "element_symbol", "mean_at")
req_phy  <- c("species", "class", "feeding_guild_simple")

miss_tray <- setdiff(req_tray, names(tray_element_means))
miss_phy  <- setdiff(req_phy, names(species_phylogeny))

if (length(miss_tray) > 0) stop("Missing in tray_element_means: ", paste(miss_tray, collapse = ", "))
if (length(miss_phy)  > 0) stop("Missing in species_phylogeny: ",  paste(miss_phy,  collapse = ", "))

# Drop any accidental index column like "unnamed_0"
tray_element_means <- tray_element_means %>%
  select(-matches("^unnamed"))

# Join class + feeding guild
plot_df <- tray_element_means %>%
  left_join(species_phylogeny %>% select(species, class, feeding_guild, feeding_guild_simple), by = "species")

if (any(is.na(plot_df$class)) || any(is.na(plot_df$feeding_guild))) {
  missing_species <- plot_df %>%
    filter(is.na(class) | is.na(feeding_guild)) %>%
    distinct(species) %>%
    pull(species)
  stop("Missing class/feeding_guild for species: ", paste(missing_species, collapse = ", "))
}

# ---- 4) Optional: add a human-readable element name column (so your old aes(fill=element) works) ----
element_lookup <- tribble(
  ~element_symbol, ~element,
  "Al", "Aluminum",
  "Mg", "Magnesium",
  "Si", "Silicon",
  "S",  "Sulfur",
  "Rh", "Rhodium",
  "Ra", "Radium",
  "Th", "Thorium",
  "C",  "Carbon",
  "O",  "Oxygen",
  "Na", "Sodium",
  "Cl", "Chlorine",
  "Ca", "Calcium"
)

plot_df <- plot_df %>%
  left_join(element_lookup, by = "element_symbol") %>%
  mutate(element = coalesce(element, element_symbol))

# ---- 5) Filter to match your previous object: no Ca/C/O/Na/Cl ----
exclude_symbols <- c("Ca", "C", "O", "Na", "Cl")

madreporite_ossicle_means_noCaCONaCl <- plot_df %>%
  filter(!element_symbol %in% exclude_symbols)

# Facet background data
shade_df <- madreporite_ossicle_means_noCaCONaCl %>%
  distinct(species, class)
species_levels <- c(
  "Strongylocentrotus droebachiensis",
  "Strongylocentrotus purpuratus",
  "Mesocentrotus franciscanus",
  "Dendraster excentricus",
  "Ophiopholis aculeata",
  "Florometra serratissima",
  "Pycnopodia helianthoides",
  "Leptasterias hexactis",
  "Pisaster ochraceus",
  "Patiria miniata",
  "Henricia leviuscula",
  "Mediaster aequalis",
  "Stylasterias forreri",
  "Orthasterias koehleri",
  "Evasterias troschelii",
  "Dermasterias imbricata",
  "Chiridota albatrossii",
  "Parastichopus californicus",
  "Cucumaria miniata",
  "Cucumaria pallida",
  "Cucumaria piperata",
  "Psolus chitinoides",
  "Pentamera spp"
)

shade_df <- shade_df %>%
  mutate(species = factor(species, levels = species_levels))

madreporite_ossicle_means_noCaCONaCl <- madreporite_ossicle_means_noCaCONaCl %>%
  mutate(species = factor(species, levels = species_levels))

# Optional: order x-axis elements
element_order <- c("Al","Si","S","Mg","Rh","Ra","Th")
present_order <- element_order[element_order %in% madreporite_ossicle_means_noCaCONaCl$element_symbol]
if (length(present_order) > 1) {
  madreporite_ossicle_means_noCaCONaCl <- madreporite_ossicle_means_noCaCONaCl %>%
    mutate(element_symbol = factor(element_symbol, levels = present_order))
}

# ---- 6) Plot (matches your example structure) ----
p <- ggplot(madreporite_ossicle_means_noCaCONaCl,
            aes(x = element_symbol, y = mean_at * 100)) +
  geom_rect(
    data = shade_df,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = class),
    inherit.aes = FALSE,
    alpha = 0.15
  ) +
  scale_fill_brewer(palette = "Dark2", name = "Class") +
  ggnewscale::new_scale_fill() +
  geom_col(aes(fill = element), alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ species, nrow = 6, ncol = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Element", y = "Atomic percent (%)")

print(p)
#################



shade_df <- plot_df %>%
  distinct(species, class)

plot_df <- plot_df %>%
  mutate(species = factor(species, levels = species_levels))

p <- ggplot(plot_df,
            aes(x = element_symbol, y = mean_at * 100)) +
  geom_rect(
    data = shade_df,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = class),
    inherit.aes = FALSE,
    alpha = 0.15
  ) +
  scale_fill_brewer(palette = "Dark2", name = "Class") +
  ggnewscale::new_scale_fill() +
  geom_col(aes(fill = element), alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ species, nrow = 6, ncol = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Element", y = "Atomic percent (%)")

print(p)






write.csv(plot_df, "Data/processed/tray_elements_phylogeny.csv")
