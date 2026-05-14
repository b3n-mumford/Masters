#-----------------------------
# Load packages
#-----------------------------
library(tidyverse)

#Double check that worknig directory is set 
setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")
#-----------------------------
# Set root directory for extracting my data
#-----------------------------
sem_root <- "Data/raw/SEM/acorn"

#-----------------------------
# Find all quantification.csv files
#-----------------------------
quant_files <- list.files(
  path = sem_root,
  pattern = "^quantification\\.csv$",
  recursive = TRUE,
  full.names = TRUE, 
  ignore.case = TRUE
)

#-----------------------------
# Function to parse tray_id, spot, and session from folder path
#-----------------------------
parse_file_info <- function(file_path) {
  
  # Parent folder of quantification.csv
  folder_name <- basename(dirname(file_path))
  
  # Extract tray_id: starts with letter(s) + number
  tray_id <- str_extract(folder_name, "^[A-Z]+[0-9]+")
  
  # Extract spot number after 'analysis_'
  spot <- str_extract(folder_name, "(?<=analysis_)\\d+")
  
  # Extract session from path: e.g., Session_1
  session <- str_extract(file_path, "Session_[0-9]+")
  
  tibble(
    session = session,
    tray_id = tray_id,
    spot = as.integer(spot)
  )
}

#-----------------------------
# Read and combine all CSVs
#-----------------------------
acorn_quant_df <- map_dfr(quant_files, function(file) {
  
  # Parse metadata
  meta <- parse_file_info(file)
  
  # Read CSV
  quant <- read_csv(file, show_col_types = FALSE)
  
  # Combine
  bind_cols(meta, quant) %>%
    mutate(source_file = file)
})




#-----------------------------
# Inspect the combined dataframe
#-----------------------------
glimpse(acorn_quant_df)

#-----------------------------
# Save tidy dataset
#-----------------------------
write_csv(
  acorn_quant_df,
  "Data/processed/acorn_quantification_all_sessions.csv"
)
#-----------------------------

#check to see how many specimens i have 
# Count the number of unique tray_id
num_tray_ids <- acorn_quant_df %>% 
  distinct(tray_id) %>% 
  nrow()
# list the unique tray IDS
unique(acorn_quant_df$tray_id)
sort(unique(acorn_quant_df$tray_id))





jeol_quant <- read.csv("Data/raw/SEM/Acorn/j_quantification.csv")
#insert column listing species (B canosus)
jeol_quant <- jeol_quant %>%
  mutate(species = "B_canosus")

#pivot longer
jeol_quant_long <- jeol_quant %>%
  pivot_longer(cols = C:Mo, names_to = "Element Symbol", values_to = "Atomic  percentage")

#average atomic percentage for each element symbol keep species column too
jeol_quant_avg <- jeol_quant_long %>%
  group_by(species, `Element Symbol`) %>%
  summarise(avg_atomic_percentage = mean(`Atomic  percentage`, na.rm = TRUE))


#rename column names 
jeol_quant_avg <- jeol_quant_avg %>%
  rename(
    `Element symbol` = `Element symbol`,
    `Atomic concentration percentage` = avg_atomic_percentage
  )


#add element name column related on element symbol column
jeol_quant_avg <- jeol_quant_avg %>%
  mutate(Element.name = case_when(
    `Element symbol` == "C" ~ "Carbon",
    `Element symbol` == "O" ~ "Oxygen",
    `Element symbol` == "Na" ~ "Sodium",
    `Element symbol` == "Mg" ~ "Magnesium",
    `Element symbol` == "Al" ~ "Aluminium",
    `Element symbol` == "Si" ~ "Silicon",
    `Element symbol` == "P" ~ "Phosphorus",
    `Element symbol` == "S" ~ "Sulfur",
    `Element symbol` == "Cl" ~ "Chlorine",
    `Element symbol` == "K" ~ "Potassium",
    `Element symbol` == "Ca" ~ "Calcium",
    `Element symbol` == "Ti" ~ "Titanium",
    `Element symbol` == "Cr" ~ "Chromium",
    `Element symbol` == "Mn" ~ "Manganese",
    `Element symbol` == "Fe" ~ "Iron",
    `Element symbol` == "Ni" ~ "Nickel",
    `Element symbol` == "Cu" ~ "Copper",
    `Element symbol` == "Zn" ~ "Zinc",
    `Element symbol` == "Mo" ~ "Molybdenum" 
  ))

#rename element.name
jeol_quant_avg <- jeol_quant_avg %>%
  rename(
    `Element name` = Element.name
  )




###extract a species column 

library(dplyr)
library(stringr)

acorn_quant_df_species <- acorn_quant_df %>%
  mutate(
    species = str_extract(
      `source_file`,
      "(?<=Data/raw/SEM/acorn/export/)[A-Za-z]+_[a-z]+"
    )
  )


#remove first 15 rows
acorn_quant_df_species <- acorn_quant_df_species %>%
  slice(-1:-15)

#add on jeol_quant_avg to acorn_quant_df_species by element name and species
acorn_quant_df_species <- bind_rows(
  acorn_quant_df_species,
  jeol_quant_avg
)



#change species column to full species names 
acorn_quant_df_species <- acorn_quant_df_species %>%
  mutate(
    species = case_when(
      species == "B_occidentalis" ~ "Balanoglossus occidentalis",
      species == "B_canosus"     ~ "Balanoglossus carnosus",
      species == "G_berkeleyi"    ~ "Glossobalanus berkeleyi",
      species == "P_graveolens"  ~ "Ptychodera graveolens",
      species == "S_kowaleskii"  ~ "Saccoglossus kowalevskii",
      species == "S_pusillus"    ~ "Saccoglossus pusillus",
      TRUE ~ species
    )
  )



#for rows of balonoglossus carnosus, change atomic conentration to same scale to fit other (divei by 100)
acorn_quant_df_species <- acorn_quant_df_species %>%
  mutate(
    `Atomic concentration percentage` = if_else(
      species == "Balanoglossus carnosus",
      `Atomic concentration percentage` / 100,
      `Atomic concentration percentage`
    )
  )

#now have full data frame 
write_csv(
  acorn_quant_df_species,
  "Data/processed/acorn_species_elements.csv"
)





#PLotting 

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")


acorn_elements <- read.csv("Data/processed/acorn_species_elements.csv")




# reorder the facetting of species

acorn_elements$species <- factor(
  acorn_elements$species,
  levels = c(
    "Balanoglossus occidentalis",
    "Balanoglossus carnosus",
    "Glossobalanus berkeleyi",
    "Ptychodera graveolens",
   "Saccoglossus kowalevskii",
     "Saccoglossus pusillus"))


#remove first 4 columns from df
acorn_elements<- acorn_elements %>%
  select(-session, -tray_id, -spot, -source_file,-Energy.level, -Atomic.number) 

#make average of elements if more than one data point exist for a species_with_
acorn_elements <- acorn_elements %>%
  group_by(species, Element.symbol, Element.name) %>%
  summarise(
    Atomic.concentration.percentage = mean(Atomic.concentration.percentage, na.rm = TRUE),
    .groups = "drop"
  )

#multiply atomic concentration percentage by 100 to get weight percent
acorn_elements <- acorn_elements %>%
  mutate(Atomic.concentration.percentage = Atomic.concentration.percentage * 100
  )

#initial plot 
ggplot(acorn_elements,
     aes(x = Element.symbol, y = Atomic.concentration.percentage, fill = Element.name)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~ species, scales = "free_y") +
  labs(
    x = "Element",
    y = "Weight percent (%)",
    fill = "Species"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )



#now remove CaCONaCl
acorn_elements_noCaCONaCl <- acorn_elements %>%
  filter(
    !Element.name %in% c("Calcium", "Carbon", "Oxygen", "Sodium", "Chlorine")
  )


#plot again 
ggplot(acorn_elements_noCaCONaCl,
       aes(x = Element.symbol, y = Atomic.concentration.percentage, fill = Element.name)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~ species, scales = "free_y") +
  labs(
    x = "Element",
    y = "Weight percent (%)",
    fill = "Species"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )



#into dataframe add a. new column called family that contains two factors Harrimaniidae and Ptychoderidae 
#assign the folling species into the folllwing as follows
#Harrimaniidae: Saccoglossus kowalevskii, Saccoglossus pusillus
#Ptychoderidae: Ptychodera graveolens, Balanoglossus occidentalis, Balanoglossus carnosus, Glossobalanus berkeleyi

acorn_elements_noCaCONaCl <- acorn_elements_noCaCONaCl %>%
  mutate(
    family = case_when(
      species %in% c("Saccoglossus kowalevskii", "Saccoglossus pusillus") ~ "Harrimaniidae",
      species %in% c("Ptychodera graveolens", "Balanoglossus occidentalis", "Balanoglossus carnosus", "Glossobalanus berkeleyi") ~ "Ptychoderidae",
      TRUE ~ NA_character_
    )
  )


#now shade the background of the facet for family ptychoderidae in light blue and family harrimaniidae in light pink


acorn_shade<- acorn_elements_noCaCONaCl %>%
  distinct(species, family)

acorn_shade$species <- factor(
  acorn_shade$species,
  levels = c(
    "Balanoglossus occidentalis",
    "Balanoglossus carnosus",
    "Glossobalanus berkeleyi",
    "Ptychodera graveolens",
    "Saccoglossus kowalevskii",
    "Saccoglossus pusillus"
  )
)


#remove gold
acorn_elements_noCaCONaCl <- acorn_elements_noCaCONaCl %>%
  filter(
    Element.name != "Gold"
  )


#only want to plot Mg and Si 
acorn_elements_noCaCONaCl <- acorn_elements_noCaCONaCl %>%
  filter(
    Element.name %in% c("Magnesium", "Silicon")
  )
    
ggplot(acorn_elements_noCaCONaCl,
       aes(x = Element.symbol, y = Atomic.concentration.percentage)) +
  geom_rect(data = acorn_shade,
            aes(xmin = -Inf, xmax = Inf,
                ymin = -Inf, ymax = Inf,
                fill = family),
            inherit.aes = FALSE,
            alpha = 0.15) +
  scale_fill_brewer(palette = "Accent", name = "Family") +
  new_scale_fill() +
  geom_col(aes(fill = Element.symbol), alpha = 0.8, show.legend = FALSE)  +
  facet_wrap(~ species, nrow = 1, ncol = 6) +
  theme_bw()+
  labs(
    x = "Element",
    y = "Atmoic percent (%)") 



