###############################################################
# Project:  National Health Survey 2022 Analysis
# Purpose:  Read and clean self-assessed health data by state
# Inputs:   TABLE 2, sheet: Table 2.3_Proportions
# Outputs:  solutions/3-clean-data/vis1.csv
#           solutions/3-clean-data/vis1.Rda
# Author:   Mark Hanly
###############################################################

##################
# Load libraries #
##################

library(dplyr)
library(openxlsx)
library(tidyr)

# Spreadsheet url
url <- "https://www.abs.gov.au/statistics/health/health-conditions-and-risks/national-health-survey/2022/NHSDC01.xlsx"

# Download the file locally
download.file(url, mode="wb", 'solutions/2-raw-data/national-health-survey-2022-table-1.xlsx')

# Read in the raw data
df2Raw <- read_excel(
  path = 'solutions/2-raw-data/national-health-survey-2022-table-1.xlsx',
  sheet = 'Table 1.3_Proportions',
  skip = 5,
  .name_repair = ~ make.names(.x, unique = TRUE)
)

# Clean the raw data
df2Clean <- df2Raw |>
  slice(12:23) |>
  select(c(X, ends_with(".1"))) |>
  pivot_longer(
    cols = ends_with(".1"),
    names_to = 'period',
    values_to = 'percent',
    values_transform = as.numeric) |>
  rename(condition = X) |>
  mutate(
    date = as.Date(
      ifelse(nchar(period)==7,
             paste(as.numeric(substr(period, 2, 5)), '07', '01', sep='-'),
             paste(as.numeric(substr(period, 2, 5)) + 1, '01', '01', sep='-'))
    )) |>
  mutate(
    condition = case_when(
      condition=="Arthritis(d)" ~ "Arthtitis",
      condition=="Asthma" ~ "Asthma",
      condition=="Back problems (dorsopathies)(e)" ~ "Back problems",
      condition=="Cancer (malignant neoplasms)" ~ "Cancer",
      condition=="Chronic obstructive pulmonary disease (COPD)(f)" ~ "COPD",
      condition=="Diabetes mellitus(g)" ~ "Diabetes",
      condition=="Hayfever and allergic rhinitis" ~ "Allergies",
      condition=="Heart, stroke and vascular disease(h)"  ~ "Heart, stroke & vascular",
      condition=="Hypertension(i)" ~ "Hypertension",
      condition=="Kidney disease(j)" ~ "Kidney disease",
      condition=="Mental and behavioural conditions(k)(l)(m)" ~ "Mental health",
      condition=="Osteoporosis(n)" ~ "Osteoporosis"
    )) |>
  select(-period)

# Save the data in .Rda format
save(df2Clean, file=here::here('solutions/3-clean-data/vis2.Rda'))

# Save the data in .csv format
write.csv(df2Clean,
          file=here::here('solutions/3-clean-data/vis2.csv'),
          row.names = FALSE)

### # JAGO # ###
