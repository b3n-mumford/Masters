library(readr)
library(dplyr)
library(stringr)
setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")

eds_quant_df <- read.csv("Data/processed/EDS_quantification_all_sessions.csv")

##### inlcude extra JEOL scans here ###########
jeol_long <- read.csv("Data/processed/extra_JEOL_SEM_scans_long.csv")

#divide atomic concentration percentage by 100 to match scale of other EDS quant data
jeol_long <- jeol_long %>%
  mutate(
    `Atomic.concentration.percentage` = `Atomic.concentration.percentage` / 100
  )
#add jeol long onto bottom of eds_quant_Df
eds_quant_df <- bind_rows(
  eds_quant_df,
  jeol_long
)

#######
 psolus_long <- read.csv("Data/processed/extra_psolus_SEM_scans_long.csv")

psolus_long <- psolus_long %>%
  mutate(
    `Atomic.concentration.percentage` = `Atomic.concentration.percentage` / 100
  )

eds_quant_df <- bind_rows(
  eds_quant_df,
  psolus_long
)

#######

#anyhting that i want ot include as my principal phykogentic analysis has been labelled as a madreporite in the ossicle column,
# hplothuroids dont have but written so are included 

## relational database to get species taken to SEM from tray ID
tray_lookup <- read.csv(
  "Data/Metadata/tray_species_lookup.csv"
)

#tidying up tray_lookup by filtering those with tray ID and species info
tray_lookup_summary <- tray_lookup %>%
  group_by(tray_id) %>%
  summarize(
    Species_Count = n(),
    Species_List = paste(species, collapse = ", ")
  )

# creating new dataframe that combines EDS quant data with species info
eds_with_species <- eds_quant_df %>%
  left_join(tray_lookup, by = "tray_id")

#filtering to see which tray IDs do not have species info
eds_with_species %>%
  filter(is.na(species)) %>%
  distinct(tray_id)
##result is no tray IDs without species info
#apart from T1 and T2 - which were the feeding tank experiments




#### tidy dataframe by sorting tray id by number following letter

eds_with_species %>%
  mutate(
    tray_row = str_extract(tray_id, "^[A-Za-z]+"),
    tray_col = as.integer(str_extract(tray_id, "\\d+"))
  ) %>%
  arrange(tray_row, tray_col)
#(this code doesnt change the dataframe, and is just for viewing - likely uneeded)




#export final dataframe with species info added to EDS quant data
write_csv(
  eds_with_species,
  "Data/processed/species_elements.csv"
)

