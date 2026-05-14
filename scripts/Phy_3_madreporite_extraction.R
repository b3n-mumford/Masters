#first analysis of madreporite ossicles 
library(dplyr)
setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")


eds_with_species <- read.csv("Data/processed/species_elements.csv")

#filter dataframe to only include species that have had madreporite analysed 
madreporite_df <- eds_with_species %>%
  filter(
    ossicle == "Madreporite"
  )

#identify how many species there are in new filtered dataframe
unique_species <- unique(madreporite_df$species)



####need to remove two rows in particular from dataframe (Mediaster aequalis)
# rows 158 and 159
madreporite_df <- madreporite_df %>%
  slice(-c(158, 159))

#the only one is Dendraster excentricus with two madreporite tray species 
# want to drop the rows J5 
madreporite_df <- madreporite_df %>%
  filter(
    tray_id != "R5"
  )


#answer should be 23
write_csv(madreporite_df, 
          "Data/processed/madreporite_df.csv")




############### OK UP UNTIL THIS POINT #######

##############    #############  #############
##############    #############  #############
##############    #############  #############

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(readr)
})

# ---- 1) Read + standardize column names ----
madreporite_df <- read_csv("Data/processed/madreporite_df.csv", show_col_types = FALSE) %>%
  clean_names()

# Expecting columns like:
# session, tray_id, spot, element_symbol, element_name,
# atomic_concentration_percentage, weight_concentration_percentage,
# source_file, species, ossicle, common

required <- c(
  "tray_id","spot","element_symbol","atomic_concentration_percentage","species"
)
missing <- setdiff(required, names(madreporite_df))
if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))

# ---- 2) Define scan_id robustly ----
# Prefer source_file (unique per scan). Fall back to tray_id + spot when source_file missing.
madreporite_df <- madreporite_df %>%
  mutate(
    scan_id = dplyr::coalesce(
      source_file,
      paste0("unknown__", tray_id, "__spot", spot)
    )
  )

# ---- 3) Basic integrity checks ----
# (a) One row per scan_id x element_symbol (should be true)
dups <- madreporite_df %>%
  count(scan_id, element_symbol) %>%
  filter(n > 1)

if (nrow(dups) > 0) {
  stop("Duplicate element entries within a scan detected. Example rows:\n",
       paste0(capture.output(print(head(dups, 10))), collapse = "\n"))
}

# (b) Check closure within each scan (atomic concentrations should sum ~ 1)
scan_totals <- madreporite_df %>%
  group_by(scan_id) %>%
  summarise(total_at = sum(atomic_concentration_percentage, na.rm = TRUE), .groups = "drop")

# If this fails badly, fix upstream (but tiny floating error is fine)
if (any(abs(scan_totals$total_at - 1) > 1e-3, na.rm = TRUE)) {
  warning("Some scans do not sum ~1 in atomic concentration. Inspect scan_totals.")
}

# ---- 4) Create a complete scan x element table (fill missing elements as 0) ----
elements <- sort(unique(madreporite_df$element_symbol))

scan_meta <- madreporite_df %>%
  distinct(scan_id, tray_id, species, ossicle, session, common)

scan_long <- madreporite_df %>%
  select(
    scan_id,
    element_symbol,
    at = atomic_concentration_percentage,
    wt = weight_concentration_percentage
  )

scan_complete <- scan_meta %>%
  tidyr::crossing(element_symbol = elements) %>%
  left_join(scan_long, by = c("scan_id", "element_symbol")) %>%
  mutate(
    at = coalesce(at, 0),
    # wt may be missing in some rows; absent element => 0, but truly missing wt for present element stays NA
    # (If you want strict 0-fill for missing wt too, change coalesce(wt, 0))
    wt = if_else(is.na(wt), 0, wt)
  )

# ---- 5) Compute tray-level means (replicate scans within the same specimen) ----
tray_element_means <- scan_complete %>%
  group_by(species, tray_id, element_symbol) %>%
  summarise(
    mean_at = mean(at),
    mean_wt = mean(wt),
    n_scans = n_distinct(scan_id),
    .groups = "drop"
  )

# Optional diagnostic: tray totals (should sum to ~1 across elements for mean_at)
tray_totals <- tray_element_means %>%
  group_by(species, tray_id) %>%
  summarise(total_mean_at = sum(mean_at), .groups = "drop")

# # ---- 6) Compute species-level means (average trays within species) ----
# species_counts <- tray_element_means %>%
#   distinct(species, tray_id, n_scans) %>%
#   group_by(species) %>%
#   summarise(
#     n_trays = n_distinct(tray_id),
#     n_scans_total = sum(n_scans),
#     .groups = "drop"
#   )
# 
# species_element_means <- tray_element_means %>%
#   group_by(species, element_symbol) %>%
#   summarise(
#     mean_at = mean(mean_at),
#     sd_at_between_trays = sd(mean_at),
#     mean_wt = mean(mean_wt),
#     sd_wt_between_trays = sd(mean_wt),
#     .groups = "drop"
#   ) %>%
#   left_join(species_counts, by = "species") %>%
#   arrange(species, element_symbol)




#############
# 
# Keep tray-level means as your main “analysis-ready” table.
# If you want a “species-level” file for convenience in plotting, you can just rename:
species_element_means <- tray_element_means %>%
  select(species, element_symbol, mean_at, n_scans) 


# ---- 7) Save outputs ----
write.csv(tray_element_means, "Data/processed/tray_element_means.csv")
write.csv(species_element_means, "Data/processed/species_element_means.csv")
write.csv(scan_totals, "Data/processed/diagnostic_scan_totals.csv")
write.csv(tray_totals, "Data/processed/diagnostic_tray_totals.csv")

# Objects now available:
# - tray_element_means
# - species_element_means





##############    #############  #############
##############    #############  #############
##############    #############  #############
##############    #############  #############
##############    #############  #############
##############    #############  #############











#rename a few columns and streamline this dataframe
madreporite_df <- madreporite_df %>%
  select(
    session,
    species,
    ossicle,
    tray_id,
    spot,
    Element.name,
    Atomic.concentration.percentage,
    Weight.concentration.percentage
  )

#difference in columns have been dropped between eds_with_species and madreporite_df
# colnames(eds_with_species)
# colnames(madreporite_df)
# difference <- setdiff(colnames(eds_with_species), colnames(madreporite_df))
# view(difference)



# tidy up column names
madreporite_df <- madreporite_df %>%
  rename(
    element = Element.name,
    at_percent = Atomic.concentration.percentage,
    wt_percent = Weight.concentration.percentage
  )



#  average if there is more than one spot analysis for a tray_id
#keeping both atomic and weight concentration percentage
madreporite_ossicle_means <- madreporite_df %>%
  group_by(
    species,
    tray_id,
    element
  ) %>%
  summarise(
    mean_wt = mean(wt_percent, na.rm = TRUE), mean_at = mean(at_percent, na.rm = TRUE),
    n_scans = n(),
    .groups = "drop"
  )





#identify if there were multiple tray_id and madreporite scan for any species
madreporite_tray_counts <- madreporite_ossicle_means %>%
  group_by(species) %>%
  summarise(
    n_trays = n_distinct(tray_id),
    .groups = "drop"
  )





#export the data
write_csv(
  madreporite_ossicle_means,
  "Data/processed/madreporite_ossicle_element_means.csv"
)


#convert into wide format for future analysis too 
madreporite_wide <- madreporite_ossicle_means %>%
  pivot_wider(
    names_from  = element,
    values_from = mean_wt
  )

#export the data
write_csv(madreporite_wide, 
          "Data/processed/madreporite_ossicle_element_means_wide.csv")


