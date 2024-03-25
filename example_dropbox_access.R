library(tidyverse)
library(rdrop2)

# Authenticate with dropbox
drop_auth(new_user = TRUE)

# View files filtered by stem (easier to save to object then access path names)
drop_dir("winter data/covid19 lockdown surveys")

# Load a single file
my_df <- drop_read_csv("winter data/rwa mturk experiment 2022/condition_codes.csv")


# Get multiple files
files <- rdrop2::drop_dir("winter data/covid19 lockdown surveys/momentary_level_one")$path_display

# Pull each file from dropbox and bind it into a single df
raw_data <- data.frame()

for (file in 1:length(files)) {
  print(files[file])
  
  raw_data <- bind_rows(
    raw_data,
    drop_read_csv(files[file])
  )
}
