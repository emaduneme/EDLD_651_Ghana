
## What effects do messages about
# climate change have on Millennials and Generation Z
# and their desire to have children?


library(tidyverse)
library(psych)
library(skimr)
library(readr)
library(janitor)
library(rio)
library(here)
library(ggthemes)
library(readxl)
library(DT)



here("Data")
list.files(here("Data"))
## Loading data
css_ghana <- read_excel(here("Data", "Climate_Synthetic.xlsx"))


## Tidying Data: Pivot_longer gender into one variable and creating a value that will eventually be removed.
css_ghana <- css_ghana %>% 
  pivot_longer(c("Male", "Female"), 
               names_to = "Gender",
               values_to = "value") %>% 
  select(1:2,Gender, everything())
view(css_ghana)

## I had to deselect some unwanted variables separately for them to work. 
css_ghana <- css_ghana %>% 
  select(-(starts_with("1.")), -value)


## Filtering for only those that completed the survey
css_ghana <- css_ghana %>% 
  filter(Status == "Completed") 



#Renaming variables
css_ghana <- css_ghana %>% 
  rename(q8_cc_priority = "8. How should addressing issues of climate change be prioritized?") %>% 
  filter(!is.na(q8_cc_priority))

css_ghana <- css_ghana %>%
  rename(age_generation =  "Which age generation below applies to you",
    q_10_cc_message_effec_providing_info  = 
      "10. Think about messages you’ve seen in the media about climate change. How effective were they in terms of providing information?",
    q_17_cc_infuence_child_desire = 
      "17. How much of an influence has climate change had on your desire to have children?",
    q7_cc_concerned = "7. How concerned are you about climate change?") 


names(css_ghana)[8] = "q_16_not_parent_chid_desire"

# Recoding Variables

css_ghana <- css_ghana %>% 
  mutate(age_gen_recoded = 
           case_when(age_generation ==  "Other" ~ "Others",
                     age_generation == "1997-2004" ~ "Gen_Z",
                     TRUE ~ "Millennials")) 
  
css_ghana <- css_ghana %>% 
  select(1:3, age_gen_recoded, everything())


view(css_ghana)

names(css_ghana)


  ## Recoding To numeric
  css_ghana$q7_cc_concerned <- recode(css_ghana$q7_cc_concerned, "Not at all" = 1, "Not very concerned" = 2 , "Slightly concerned" = 3, "Moderately concerned" = 4, "Very concerned" = 5,     
         "Extremely concerned" = 6)
  

## the row names were not properly written so I had to recode it  
css_ghana <- css_ghana %>% 
  mutate(q8_cc_priority = case_when(
    q8_cc_priority == "Low priority" ~"1",
    q8_cc_priority == "Moderate priority" ~ "3",
    q8_cc_priority == "Major priority" ~ "4",
    q8_cc_priority == "Top priority" ~ "5",
    q8_cc_priority == "Minor priority" ~ "2",
    q8_cc_priority == " Minor priority" ~ "2",
  )) 

# Recoding from character to numeric
css_ghana <-css_ghana %>% 
  filter(!is.na(q_10_cc_message_effec_providing_info),
         !is.na( q_17_cc_infuence_child_desire),
         !is.na(q_16_not_parent_chid_desire))
         
                
sort(unique(css_ghana$q_10_cc_message_effec_providing_info))
css_ghana <- css_ghana %>% 
mutate(q8_cc_priority = as.numeric(q8_cc_priority)) 
 
css_ghana$q_16_not_parent_chid_desire <- recode(css_ghana$q_16_not_parent_chid_desire,
  "Extremely high desire" = 7, "High desire" = 6,  "Minor desire" = 5, 
  "Moderate desire" = 4, "No desire or unsure" = 3,  "Somewhat dissatisfied" = 2,
  "Very low desire" = 1)  

css_ghana$q_17_cc_infuence_child_desire <- recode(css_ghana$q_17_cc_infuence_child_desire,
"Barely any influence" = 6, "Major influence" = 5,  "Minor influence" = 4,     "Moderate influence" = 3,
 "No influence at all" = 2, "Top influence"  = 1)


css_ghana$q_10_cc_message_effec_providing_info <- recode(css_ghana$q_10_cc_message_effec_providing_info,
"Extremely effective" = 6,               "Moderately effective"  = 5,                 
 "Not effective at all or Not applicable" = 4, "Slightly effective"  = 3,                 
 "Very effective" = 2 ,                    "Very low effect" = 1)
 
 


css_ghana$Gender <- as.factor(css_ghana$Gender)

css_ghana$age_gen_recoded <- as.factor(css_ghana$age_gen_recoded)
unique(css_ghana$age_gen_recoded)

css_ghana %>% 
 group_by(Gender,age_gen_recoded) %>% 
  summarise(mean_priority = mean(q8_cc_priority, na.rm = TRUE))

css_ghana %>% 
  group_by(age_gen_recoded, q_17_cc_infuence_child_desire) %>% 
  summarise(mean_child_desire = (mean(q_17_cc_infuence_child_desire)),
            mean_climate_message = mean(q_10_cc_message_effec_providing_info)) %>% 
  ggplot(aes( mean_climate_message, age_gen_recoded, fill = age_gen_recoded)) +
  geom_col(size = 1) +
  facet_wrap(~q_17_cc_infuence_child_desire, ncol = 1)



export(css_ghana, here("Data", "Climate_Synthetic_clean.sav"))


  view(css_ghana)
   
  
  ## mutate(q8_cc_priority = recode(q8_cc_priority,
                              ##   "Old category" = 1,
                              ## "other old category" = 2)
        ## mutate(q8_cc_priority = recode(q8_cc_priority,
                                 ##       "Moderate priority" = 1,
                                  ##      "Major priority" = 2)
   