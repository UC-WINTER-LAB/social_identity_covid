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


#Creates new dataset with age, gender, ethnicity, political party, political 
#orientation, conservatism, and ILI, ingroup affiliation
test_ili <- sil_df[c(2, 4, 6, 13:30, 39:60, 68:82)]
test_ili[test_ili == ""] <- NA
test_ili <- na.omit(test_ili)

#remove label rows
test_ili <- test_ili[-c(1, 2), ]

###################Recoding####################

test_ili <- test_ili %>%
  
  #Recode conservatism
  mutate_at(c(4:9), ~as.numeric(recode(., "Strongly Disagree" = 1, "Disagree" = 2, 
                                       "Somewhat disagree" = 3, "Neutral" = 4, 
                                       "Somewhat agree" = 5, "Agree" = 6, 
                                       "Strongly Agree" = 7))) %>%
  
  #Recode political orientation (bigger the number, the more conservative)
  mutate_at(c(22), ~as.numeric(recode(., "Very Liberal" = 1, "Liberal" = 2, 
                                     "Somewhat liberal" = 3, "Moderate" = 4, 
                                     "Somewhat conservative" = 5, "Conservative" = 6,
                                     "Very Conservative" = 7))) %>%
  
  #Recode political affiliation
  mutate_at(c(23:27), ~as.numeric(recode(., "Totally Oppose" = 1, "Strongly Oppose" = 2,
                                       "Oppose" = 3, "Somewhat oppose" = 4, "Weakly oppose" = 5,
                                       "Neutral/ Neither oppose nor support" = 6, "weakly support" = 7,
                                       "Somewhat support" = 8, "Support" = 9, "Strongly Support" = 10,
                                       "Totally Support" = 11))) %>%
  
  #Recode ingroup affiliation AND ili AND traditionalism and authoritarianism
  mutate_at(c(10:21, 28:58), ~as.numeric(recode(., "Strongly disagree" = 1, "Disagree" = 2, 
                                        "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
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

###########################CFAS######################################

########Test ILI CFA

leadership.Model <- '
  prototypicality =~ ID.leadership_1 + ID.leadership_2 +  ID.leadership_3 + ID.leadership_4

  advancement =~ ID.leadership_5 + ID.leadership_6 + ID.leadership_7 + ID.leadership_8

  entrepreneurship =~ ID.leadership_9 + ID.leadership_10 + ID.leadership_11 + ID.leadership_12

  impresarioship =~ ID.leadership_13 + ID.leadership_14 + ID.leadership_15 
  
  ili =~ 1*prototypicality + 1*advancement + 1*entrepreneurship + 1*impresarioship
'

leadership_fit <- cfa(leadership.Model, data = test_ili, ordered = TRUE)
summary(leadership_fit, fit.measures = TRUE) #Good RMSEA, good CFI


##Yeet predicted ili scores into ye olde test dataframe
idx <- lavInspect(leadership_fit, "case.idx")
ili_scores <- lavPredict(leadership_fit)
## loop over factors
for (fs in colnames(ili_scores)) {
  test_ili[idx, fs] <- ili_scores[ , fs]
}


#####################ACT CFA###############
authority_cols <- select(test_ili, starts_with("ACT.")) %>%
  colnames()

authority_cfa <- cfa(
  paste(
    paste("conservatism =~", paste0(authority_cols[grepl(".Conservatism", authority_cols)][c(2, 1, 3:6)], collapse = " + ")),
    paste("traditionalism =~", paste0(authority_cols[grepl(".Traditionalism", authority_cols)][c(2, 1, 3:6)], collapse = " + ")),
    paste("authoritarianism =~", paste0(authority_cols[grepl(".Authoritarianism", authority_cols)][c(2, 1, 3:6)], collapse = " + ")),
    "act =~ 1*conservatism + 1*traditionalism + 1*authoritarianism",
    sep=" \n "
  ),
  data = test_ili, ordered = TRUE
)
summary(authority_cfa, fit.measures = TRUE)

##Yeet predicted ACT latent values into dataframe
idx <- lavInspect(authority_cfa, "case.idx")
act_scores <- lavPredict(authority_cfa)
##loop over factors
for (fs in colnames(act_scores)) {
  test_ili[idx, fs] <- act_scores[ , fs]
}



#################Test Ingroup Affilitation CFA######################

ingroup.Model <- '
  commitment =~ Ingroup.identity_1 + Ingroup.identity_5 + Ingroup.identity_9 +
                Ingroup.identity_13
  
  superiority =~ Ingroup.identity_2 + Ingroup.identity_6 + Ingroup.identity_10 +
                 Ingroup.identity_14
  
  importance =~ Ingroup.identity_3 + Ingroup.identity_7 + Ingroup.identity_11 + 
                Ingroup.identity_15
  
  deference =~ Ingroup.identity_4 + Ingroup.identity_8 + Ingroup.identity_12 +
               Ingroup.identity_16
               
  ingroup =~ 1*commitment + 1*superiority + 1*importance + 1*deference
'

ingroup_fit <- cfa(ingroup.Model, data = test_ili, ordered = TRUE)
summary(ingroup_fit, fit.measures = TRUE) #both rmsea and cfi are bad


#Join predicted values into the dataset
idx <- lavInspect(ingroup_fit, "case.idx")
ingroup_scores <- lavPredict(ingroup_fit)
## loop over factors
for (fs in colnames(ingroup_scores)) {
  test_ili[idx, fs] <- ingroup_scores[ , fs]
}

#######################Clear environment, make nice dataframe#################
analysis_df <- test_ili[c(1, 3, 22, 59:74)]

rm(list = setdiff(ls(), "analysis_df"))

#Making sure that the variables are set nicely (and correctly)
analysis_df$Political_Party <- as.factor(analysis_df$Political_Party)
analysis_df$Sex <- as.factor(analysis_df$Sex)
analysis_df$Age <- as.numeric(analysis_df$Age)
