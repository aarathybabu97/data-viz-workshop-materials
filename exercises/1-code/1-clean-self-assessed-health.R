###############################################################
# Project: Data cleaning
# Purpose: Clean National Health Survey Data
# Inputs: exercises/2-raw-data/national-health-survey-2022-table-2.xlsx
# Outputs: exercises/3-clean-data/vis1.Rda
# Author: Aarathy Babu
###############################################################

##################
# Load libraries #
##################

library(readxl)
library(dplyr)
library(tidyr)

#################
# Read the data #
#################

# Spreadsheet url
myURL <- "https://www.abs.gov.au/statistics/health/health-conditions-and-risks/national-health-survey/2022/NHSDC02.xlsx"

# Download the file locally
download.file(url = myURL, mode = "wb",
              destfile = 'exercises/2-raw-data/national-health-survey-2022-table-2.xlsx')

# Read in the raw data
df1Raw <- read_excel(
  path = 'exercises/2-raw-data/national-health-survey-2022-table-2.xlsx',
  sheet = 'Table 2.3_Proportions',
  skip = 5,
  .name_repair = ~ make.names(.x, unique = TRUE)
)

##################
# Clean the data #
##################

# Define health status categories
healthStatus <- c("Poor", "Fair", "Good", "Very good", "Excellent")

# Filter to these rows
df1Clean <- df1Raw |>
  filter(X %in% healthStatus)

dim(df1Clean)

# Filter to these rows
df1Clean <- df1Raw |>
  filter(X %in% healthStatus) |>
  select(c(X, ends_with(".1"), -'Australia.1'))|>
  # Append this line to the previous code using |>
  pivot_longer(
    cols = ends_with(".1"),
    names_to = 'state',
    values_to = 'percent') |>
  rename(status=X) |> # Append this code to the previous code using |>
  mutate(
    state = factor(
      case_when (
        state == 'NSW.1' ~ 'New South Wales',
        state == 'Vic..1' ~ 'Victoria',
        state == 'Qld.1' ~ 'Queensland',
        state == 'SA.1' ~ 'South Australia',
        state == 'WA.1' ~ 'Western Australia',
        state == 'Tas..1' ~ 'Tasmania',
        state == 'NT.1' ~ 'Northern Territory',
        state == 'ACT.1' ~ 'Australian Capital Territory'
      )))|> # Append this code to the previous code using |>
  mutate( status = factor(status, levels = healthStatus)) |>
  # Append this code to the previous code using |>
  arrange(state, status)


is.factor(df1Clean$state)
levels(df1Clean$state)

###################
# Save the charts #
###################

# Save the data in .Rda format
save(df1Clean, file=here::here('exercises/3-clean-data/vis1.Rda'))

# Save the data in .csv format
write.csv(df1Clean,
          file=here::here('exercises/3-clean-data/vis1.csv'),
          row.names = FALSE)

