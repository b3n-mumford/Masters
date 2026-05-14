#-----------------------------
# Load packages
#-----------------------------
library(tidyverse)

#Double check that worknig directory is set 
setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")
#-----------------------------
# Set root directory for extracting my data
#-----------------------------
sem_root <- "Data/raw/SEM"

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
eds_quant_df <- map_dfr(quant_files, function(file) {
  
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
glimpse(eds_quant_df)

#-----------------------------
# Save tidy dataset
#-----------------------------
write_csv(
  eds_quant_df,
  "Data/processed/EDS_quantification_all_sessions.csv"
)
#-----------------------------

#check to see how many specimens i have 
# Count the number of unique tray_id
num_tray_ids <- eds_quant_df %>% 
  distinct(tray_id) %>% 
  nrow()
# list the unique tray IDS
unique(eds_quant_df$tray_id)
sort(unique(eds_quant_df$tray_id))




##### END OF SCRIPT