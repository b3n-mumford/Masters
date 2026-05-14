library(tidyverse)
library(purrr)
library(dplyr)
setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")

# read in Raman txt data files
raman_files <- list.files(
  path = "Data/raw/Raman",
  pattern = "\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)
# open raman files into one large dataset with three columns; 
# ID - taken from the file name and then the two common columns already present as headers in each file, Wave and Intensity
raman_data <- map_dfr(raman_files, function(file) {
  # Extract ID from file name
  id <- tools::file_path_sans_ext(basename(file))
  
  # Read the txt file
  data <- read_delim(file, delim = "\t", col_names = TRUE, show_col_types = FALSE)
  
  # Add ID column
  data <- data %>%
    mutate(ID = id)
  
  return(data)
})

# Rename columns for clarity
raman_data <- raman_data %>%
  rename(
    intensity = ...2,
    wave_number = "#Wave"
  )

#drop intensity column
raman_data <- raman_data %>%
  select(
    ID,
    wave_number, 
    intensity
  )



write.csv(raman_data, "Data/processed/biomechanical_raman_data.csv", row.names = FALSE)




