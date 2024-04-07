library(tidyverse)
library(rdrop2)
library(sjmisc)
library(psych)
library(lavaan)
library(Hmisc)
library(ggpubr)
library(stats)
library(rstatix)

##############Set up
drop_auth(new_user = TRUE)
drop_acc()


######Loading lvl3 leadership file############
sil_df <- drop_read_csv("winter data/covid19 lockdown surveys/covid_lvl3_identity_leadership.csv") %>%
  
  #Removing Redundant Columns
  select(-StartDate, 
         -EndDate, 
         -ResponseId,
         -Status, 
         -IPAddress, 
         -Progress, 
         -RecordedDate,
         -RecipientLastName, 
         -Duration..in.seconds.,
         -RecipientFirstName, 
         -RecipientEmail,
         -ExternalReference, 
         -LocationLatitude, 
         -LocationLongitude, 
         -DistributionChannel,
         -UserLanguage,
         -Q47, #Consent question
         -Q61) #Empty question


######Filtering###### -- IF RELEVANT

#Filtering by 'finished'
#sil_df <- sil_df %>%
  #filter(Finished == "TRUE") 


test_ili <- sil_df[c(2, 4, 40:89)]

###################Recoding####################

test_ili <- test_ili %>%
  
  #Recode political affiliation
  mutate_at(c(3:7), ~as.numeric(recode(., "Totally Oppose" = 1, "Strongly Oppose" = 2,
                                       "Oppose" = 3, "Somewhat oppose" = 4, "Weakly oppose" = 5,
                                       "Neutral/Neither oppose nor support" = 6, "weakly support" = 7,
                                       "Somewhat support" = 8, "Support" = 9, "Strongly Support" = 10,
                                       "Totally Support" = 11))) %>%
  
  #Recode ingroup affiliation
  mutate_at(c(8:23), ~as.numeric(recode(., "Strongly disagree" = 1, "Disagree" = 2, 
                                        "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                                        "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))) %>%
  
  
  #Recode ili
  mutate_at(c(31:45), ~as.numeric(recode(., "Strongly disagree" = 1, "Disagree" = 2, 
                                       "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))) %>%
  
  #Recode fear of covid
  mutate_at(c(46:52), ~as.numeric(recode(., "Strongly disagree" = 1, "Disagree" = 2, 
                                       "Somewhat disagree" = 3, "Neutral" = 4,
                                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7)))


####################Creating Sex and Political Party Variables###############

#First filtering the dataset so only those with >4 response are used = 1710 obs
test_ili <- test_ili %>%
  filter(Political.party.sup_1 > 6 | Political.party.sup_2 > 6)
  
#Creating the Political Party variable and sex variable
test_ili <- test_ili %>%
  mutate(Political_Party = case_when(Political.party.sup_1 > Political.party.sup_2 ~ 'National',
                                     Political.party.sup_2 > Political.party.sup_1 ~ 'Labour')) %>%
  
  mutate(Sex = case_when(Gender == 'Male' ~ 'Male',
                         Gender == 'Female' ~ 'Female'))



  
