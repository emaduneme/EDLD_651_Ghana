

library(dplyr)
library(tidyverse)
library(psych)
library(skimr)
library(readr)
library(janitor)
library(rio)
library(here)
library(ggthemes)
library(readxl)


Climate_Change_Survey <- read_excel(here("Data", "Climate Change Survey.xlsx"), setclass = "tbl_df")

names(Climate_Change_Survey)
css_ghana <- Climate_Change_Survey %>% 
  select(1, 4, 10:13, 18:19, 21, 26:27)
export(css_ghana, here("Data", "Climate_Synthetic.xlsx"))




## Recoding Gender

css_ghana <- css_ghana %>% 
pivot_longer(c("Male", "Female"), names_to = "Gender") %>% 
  select(1:2,Gender, !value, everything())
  view(css_ghana)
  
  css_ghana <- css_ghana %>% 
  select(-(starts_with("1.")))
  
  
##Renaming Variable. 
css_ghana <- Climate_Change_Survey %>% 
select(-(starts_with("1."))) 
  mutate(Gender = 
           case_when(
             Male == "x" ~ "male",
             Female == "x" ~ "female",
             Male == "x" & Female == "x" ~ "both"
           )) %>% 
rename(q8_cc_priority = "8. How should addressing issues of climate change be prioritized?" ) %>% 
  view()
 


## Recode 
css_ghana <- css_ghana %>% 
 filter(!is.na(q8_cc_priority)) %>% 
  mutate(q8_cc_priority = case_when(
                              q8_cc_priority == "Low priority" ~"1",
                              q8_cc_priority == "Moderate priority" ~ "3",
                              q8_cc_priority == "Major priority" ~ "4",
                              q8_cc_priority == "Top priority" ~ "5",
                              q8_cc_priority == "Minor priority" ~ "2",
                              q8_cc_priority == " Minor priority" ~ "2"
                              
         ))
names(css_ghana)

## changing columns 

# colnames(css_ghana)[12:15] <- c("age_generation", "region", "education",
#                                "work_status") 


css_ghana <- css_ghana %>% 
rename(consent = 
         "STATEMENT OF CONSENT\r\nI will like to take part in this survey",
       age_generation =  "Which age generation below applies to you",
       region = "3. Region of stay",
       level_of_education =
         "4. What is the highest level of education you have completed?",
       occupation_status = "5.Occupational status; Are you presently working?")

css_ghana <- css_ghana %>% 
  select(-c("Male", "Female"))


css_ghana <- css_ghana %>% 
rename(q7_cc_concerned = "7. How concerned are you about climate change?",
       income_range = 
         "6. Income range: In which of these broad categories would your monthly household income fall roughly?",
     q_9_cc_cc_addressed_region =  "9. Currently, how much is the issue of climate change being addressed in the region where you reside?")
  

#rearrange columns  
css_ghana <- css_ghana %>% 
  select(1:9, "Gender", everything())


#Convert climate change prioirty to numeric
css_ghana <-  css_ghana %>% 
 mutate(q8_cc_priority = as.numeric(q8_cc_priority))
 class(css_ghana$q8_cc_priority)


unique(css_ghana$age_generation)
 
## Recode age_generation
css_ghana <- css_ghana %>% 
 mutate(age_gen_recoded = 
          case_when(age_generation ==  "Other" ~ "Others",
                    age_generation == "1997-2004" ~ "Gen_Z",
                    TRUE ~ "Millennials")) 

css_ghana <- css_ghana %>% 
  filter(age_gen_recoded != "Others")


 

## Rename variables 
css_ghana <- css_ghana %>% 
   rename(
     q_10_cc_message_effec_providing_info  = 
          "10. Think about messages you’ve seen in the media about climate change. How effective were they in terms of providing information?",
     q_12_cc_message_effec_inspir_change = 
       "12. Think about messages you’ve seen in the media about climate change. How effective were they in terms of inspiring change?",
     q_13_cc_message_effec_feasib_recomdatn = 
       "13. Think about messages you’ve seen about ways to help slow the effects of climate change. How feasible were the recommendations?",
     q_14_cc_message_negative_emotions =
       "14. Think about messages you’ve seen in the media about climate change. On what level did you experience negative emotions?",
     q_15_cc_message_hope =
     "15. Think about messages you’ve seen about climate change. On what level did you experience hope?",
     q_17_cc_infuence_child_desire = 
       "17. How much of an influence has climate change had on your desire to have children?",
    q_18_impact_cc_desire_child =
      "\r\n18. If issues of climate change were addressed more effectively, how much would it impact your desire to have children?",
    q_19_open_for_interviews =
      "19. Would you be comfortable participating in a one-on-one interview to elaborate on your answers above?")
 

names(css_ghana)
## renaming variables
names(css_ghana)[24] = "q_16_not_parent_chid_desire"
   
unique(css_ghana$q7_cc_concerned)

## To numeric
 recode(css_ghana$q7_cc_concerned, "Not at all" = 1, "Not very concerned" = 2 , "Slightly concerned" = 3, "Moderately concerned" = 4, "Very concerned" = 5,     
        "Extremely concerned" = 6)
 
 

  view(css_ghana)
names(css_ghana) 
css_ghana <- css_ghana %>% 
  select(1:10,age_gen_recoded, everything())


data_science_class_final <- css_ghana %>% 
  select(age_gen_recoded, Gender, q7_cc_concerned, q8_cc_priority, 
         q_10_cc_message_effec_providing_info, q_14_cc_message_negative_emotions,
         25:27)
data_science_class_final


here()
export(data_science_class_final, here("Data", "Synthetic_ghana_data.sav"))

## 18, 19, 21, 26, 27
