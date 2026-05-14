library(tidyverse)

setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis/Data/raw/SEM")

#extractin Pisaster feeding data in files Tank1 and Tank2
# want to read the quantification.csv files inside each specimen (labelled as T1_PX) within export of 
#each of these folders 

tank_dirs <- c("Tank1", "Tank2")

quant_files <- list.files(
  path = tank_dirs,
  pattern = "quantification\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)


quant_df <- tibble(file_path = quant_files) %>%
  mutate(
    specimen_id = basename(dirname(file_path)),
    tank = str_extract(specimen_id, "^T[12]")
  )


pisaster_data <- quant_df %>%
  mutate(data = map(file_path, read_csv)) %>%
  unnest(data)


#only want to keep specimens that are labelled as "a" replicates, 
#which are the ones that were used for SEM and Raman analysis, 
#and thus have the most complete data for the analyses I want to do.
pisaster_madreporite <- pisaster_data %>%
  filter(str_detect(specimen_id, "T[1-2]_P[1-9]+_a"))



#remove those that are numbered after finishing in _a after T2_PX_a, as these are the ones that were not used for SEM and Raman analysis, and thus do not have the most complete data for the analyses I want to do.
pisaster_madreporite <- pisaster_madreporite %>%
  filter(!str_detect(specimen_id, "T[1-2]_P[1-9]+_a[1-9]"))




#now remove the part after TX_PX to get the specimen 
#name that matches the one used for SEM and Raman analysis
pisaster_madreporite <- pisaster_madreporite %>%
  mutate(
    specimen = str_extract(specimen_id, "T[1-2]_P\\d+")
  )



dup_check <- pisaster_madreporite %>%
  count(specimen) %>%
  filter(n > 1)


####want to swap out _a for T2_P6 with T2_P6_b 

#exatract T2_P6-b from pisaster_Data
t2_p6_b <- pisaster_data %>%
  filter(str_detect(specimen_id, "T2_P6_b"))



#replace the rows in pisaster_madreporite that have specimen T2_P6 with the rows from t2_p6_b
pisaster_madreporite <- pisaster_madreporite %>%
  filter(specimen != "T2_P6") %>%
  bind_rows(t2_p6_b %>%
              mutate(
                specimen = "T2_P6"
              ))


#  #T2_P6 contains nickel 
# #remove this particular element symbol from this specimen
# 
# #rename column Element name to element_name
# pisaster_madreporite <- pisaster_madreporite %>%
#   rename(
#     element_name = `Element name`
#   )
# 
# pisaster_madreporite <- pisaster_madreporite %>%
#   filter(!(specimen == "T2_P6" & element_name == "Nickel"))
# 


#separate and rename tank and treatment info from specimen ID
pisaster_madreporite <- pisaster_madreporite %>%
  mutate(
    tank = str_extract(specimen, "T[12]"),
    treatment = case_when(
      tank == "T1" ~ "normal",
      tank == "T2" ~ "crushed"
    )
  )

#number of data points for normal treatment 
normal_count_n <- pisaster_madreporite %>%
  filter(treatment == "normal") %>%
  nrow()
normal_count_c <- pisaster_madreporite %>%
  filter(treatment == "crushed") %>%
  nrow()



setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")
#export the data
write_csv(
  pisaster_madreporite,
  "Data/processed/pisaster_madreporite.csv" )








