# Load libraries ----
library(tidyverse)
library(readxl)
library(here)


# Import data ----
questions <- read_xlsx(path = here("data", "data-dictionary.xlsx"), sheet = "questions")

responses_pre_processed <- read_csv(file = here("data", "survey-responses-pre-processed.csv"),)


# Recode ASAPbioCrowd ----
responses_pre_processed %>% count(ASAPbioCrowd)

response_recoded1 <- responses_pre_processed %>%
  mutate (ASAPbioCrowd = case_when(str_detect(ASAPbioCrowd, "never participated") ~ "Never participated",
                                   str_detect(ASAPbioCrowd, "active participant") ~ "Actively participate",
                                   str_detect(ASAPbioCrowd, "used to be involved") ~ "Used to participate"))


# Recode demographics data ----
## explore responses
sort(unique(responses_pre_processed$Nationality))
sort(unique(responses_pre_processed$JobTitle))

## import df for recoding 
country_recode <- read_xlsx(path = here("data", "data-dictionary.xlsx"), sheet = "country", skip = 4) %>%
  select(Country, Continent, GNvsGS) %>%
  filter(!is.na(GNvsGS))

job_recode <- read_xlsx(path = here("data", "data-dictionary.xlsx"), sheet = "job-title") %>%
  filter(!is.na(JobTitle.Recoded))


## recode nationality, career location, and job title
response_recoded <- response_recoded1 %>%
  left_join(country_recode %>% rename(Nationality = Country, 
                                      Nationality.Continent = Continent, 
                                      Nationality.GN.GS = GNvsGS), 
            by = "Nationality") %>%
  left_join(country_recode %>% rename(CareerLocation = Country, 
                                      CareerLocation.Continent = Continent, 
                                      CareerLocation.GN.GS = GNvsGS), 
            by = "CareerLocation") %>%
  mutate(Nationality.Continent = if_else(Nationality == "I'd prefer not to answer", "I'd prefer not to answer", Nationality.Continent),
         Nationality.GN.GS = if_else(Nationality == "I'd prefer not to answer", "I'd prefer not to answer", Nationality.GN.GS),
         CareerLocation.Continent = if_else(CareerLocation == "I'd prefer not to answer", "I'd prefer not to answer", CareerLocation.Continent),
         CareerLocation.GN.GS = if_else(CareerLocation == "I'd prefer not to answer", "I'd prefer not to answer", CareerLocation.GN.GS)) %>%
  left_join(job_recode, by = "JobTitle")



# Export data set ----
write_csv(response_recoded, 
          file = here("data", "survey-responses-recoded.csv"),
          na = "")

