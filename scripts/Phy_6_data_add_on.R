library(dplyr)
library(tidyr)
library(stringr)
setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis/Data/raw")

new_raw <- read.csv("extra_JEOL_SEM_scans.csv")
new_long <- new_raw %>%
  pivot_longer(
    cols = C:Fe,
    names_to = "Element symbol",
    values_to = "Atomic concentration percentage"
  ) %>%
  filter(!is.na(`Atomic concentration percentage`))


new_long <- new_long %>%
  mutate(
    spot = str_extract(`Spectrum.Label`, "\\d+") |> as.integer()
  )

element_lookup <- tibble::tribble(
  ~`Element symbol`, ~`Atomic number`, ~`Element name`,
  "C", 6, "Carbon",
  "O", 8, "Oxygen",
  "Na", 11, "Sodium",
  "Mg", 12, "Magnesium",
  "Al", 13, "Aluminum",
  "Si", 14, "Silicon",
  "P", 15, "Phosphorus",
  "S", 16, "Sulfur",
  "Cl", 17, "Chlorine",
  "K", 19, "Potassium",
  "Ca", 20, "Calcium",
  "Fe", 26, "Iron"
)


new_long <- new_long %>%
  left_join(element_lookup, by = "Element symbol")




###up to here is good ####



new_long <- new_long %>%
  mutate(
    `Energy level` = NA_real_,
    session = NA,
    tray_id = NA,
    source_file = NA
  )



new_long <- new_long %>%
  select(
    session,
    tray_id,
    spot,
    `Atomic number`,
    `Element symbol`,
    `Element name`,
    `Atomic concentration percentage`,
    `Energy level`,
    source_file,
    Project.Path,
  )



new_long <- new_long %>%
  mutate(
    tray_id = str_split(`Project.Path`, "/", simplify = TRUE)[,2]
  )

#drop project.path
new_long <- new_long %>%
  select(-Project.Path)



#save csv
setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")
write.csv(new_long, "Data/processed/extra_JEOL_SEM_scans_long.csv", row.names = FALSE)







#####want to run above again for Psolus chitinoides
setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis/Data/raw")

p_chitnoides <- read.csv("psolus_chitinoides_JEOL.csv")
new_psolus <- p_chitnoides %>%
  pivot_longer(
    cols = C:Fe,
    names_to = "Element symbol",
    values_to = "Atomic concentration percentage"
  ) %>%
  filter(!is.na(`Atomic concentration percentage`))


new_psolus <- new_psolus %>%
  mutate(
    spot = str_extract(`Spectrum.Label`, "\\d+") |> as.integer()
  )

element_lookup <- tibble::tribble(
  ~`Element symbol`, ~`Atomic number`, ~`Element name`,
  "C", 6, "Carbon",
  "O", 8, "Oxygen",
  "Na", 11, "Sodium",
  "Mg", 12, "Magnesium",
  "Al", 13, "Aluminum",
  "Si", 14, "Silicon",
  "P", 15, "Phosphorus",
  "S", 16, "Sulfur",
  "Cl", 17, "Chlorine",
  "K", 19, "Potassium",
  "Ca", 20, "Calcium",
  "Fe", 26, "Iron"
)


new_psolus <- new_psolus %>%
  left_join(element_lookup, by = "Element symbol")


new_psolus <- new_psolus %>%
  mutate(
    `Energy level` = NA_real_,
    session = NA,
    tray_id = NA,
    source_file = NA
  )



new_psolus <- new_psolus %>%
  select(
    session,
    tray_id,
    spot,
    `Atomic number`,
    `Element symbol`,
    `Element name`,
    `Atomic concentration percentage`,
    `Energy level`,
    source_file,
    Project.Path,
  )



new_psolus <- new_psolus %>%
  mutate(
    tray_id = "F13"
  )

#drop project.path
new_psolus <- new_psolus %>%
  select(-Project.Path)


setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")
write.csv(new_psolus, "Data/processed/extra_psolus_SEM_scans_long.csv", row.names = FALSE)










