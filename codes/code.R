
# Packages et databases ---------------------------------------------------
Packages <- c('tidyverse','readr','rlang','questionr','corrplot','ggradar','dplyr','writexl')
lapply(Packages, library, character.only = TRUE)


df <- read_csv("cleaned_data/cleaned_data.csv")

source('codes/Functions.R')

View(df)


# 1. Look at Patient information ------------------------------------------

# Import csv will automaticaly convert 0,1 factor to integer, so we have to reconvert 
# them
df <- df %>%
  mutate(across(where(~all(unique(.[!is.na(.)]) %in% c("0","1"))), as.factor))


df %>%
  select(where(is.double) & - PatientID) %>%
  summary() %>%
  {data.frame(unclass(.), check.names = FALSE)}


graph('CholesterolTriglycerides')

# 1.1 Description of Patients' information regarding gender ---------------


# Almost the same number of male and female. Much more caucasian than other Etchnicity,
# higher proportion of Middle class (driven by more male), and High School Bachelor's level.
# For all these variables, gender's distribution among class is the same (about 50-50)
graphiques(Ethnicity)


# 2. Lifestyle Factors ----------------------------------------------------


