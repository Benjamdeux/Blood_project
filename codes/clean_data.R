
# Packages and data ------------------------------------------------------------

C_packages <- c('tidyverse', 'readr')
lapply(C_packages, library, character.only = TRUE)

df <- read_csv("raw_data/diabetes_data.csv")

# Modification de quatres variables que l'on recode en character
# Le genre
# L'ethnicité
# Le statut socioéconomiques
# Le niveau d'éducation

step_1<- df %>%
  mutate(Gender = fct_recode(as.character(Gender), "Male" = '0', "Female" = '1')) %>%
  mutate(
    Ethnicity = fct_recode(
      as.character(Ethnicity),
      'Caucasian' = '0',
      'African American' = '1',
      'Asian' = '2',
      'Other' = '3'
    )
  ) %>%
  mutate(SocioeconomicStatus = fct_recode(
    as.character(SocioeconomicStatus),
    'Low' = '0',
    'Middle' = '1',
    'High' = '2'
  )) %>%
  mutate(
    EducationLevel = fct_recode(
      as.character(EducationLevel),
      'None' = '0',
      'High School' = '1',
      "Bachelor's" = '2',
      'Higher' = '3'
    )
  )

step_2<- step_1 %>%
  mutate(across(where(~all(unique(.[!is.na(.)]) %in% c("0","1"))), as.factor))




write.csv(step_2, file = 'cleaned_data/cleaned_data.csv', row.names = FALSE)
