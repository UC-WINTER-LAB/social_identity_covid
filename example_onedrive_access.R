library(tidyverse)
library(Microsoft365R)

# Connect to your onedrive on the university network
my_drive <- get_business_onedrive()

# List data shared with you
my_drive$list_shared_files()

# Now get the names of each file and filter out the ones that match given string
my_drive$list_shared_files() %>%
  names() %>%
  grep("Data", ., value=TRUE) # Change Data to whatever you want to filter

# Load a dataframe from CSV
df <- my_drive$load_dataframe("Data/COVID19 Lockdown Surveys/covid_lvl1_followup.csv")
