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

#Creates new dataset with age, gender, political party, political orientation, conservatism,
#and ILI, ingroup affiliation, fear of covid, and perceived vulnerability to disease
test_ili <- sil_df[c(2, 4, 13:18, 39:60, 68:104)]

###################Recoding####################

test_ili <- test_ili %>%
  
  #Recode conservatism
  mutate_at(c(3:8), ~as.numeric(recode(., "Strongly Disagree" = 1, "Disagree" = 2, 
                                       "Somewhat disagree" = 3, "Neutral" = 4, 
                                       "Somewhat agree" = 5, "Agree" = 6, 
                                       "Strongly Agree" = 7))) %>%
  
  #Recode political orientation (bigger the number, the more conservative)
  mutate_at(c(9), ~as.numeric(recode(., "Very Liberal" = 1, "Liberal" = 2, 
                                     "Somewhat liberal" = 3, "Moderate" = 4, 
                                     "Somewhat conservative" = 5, "Conservative" = 6,
                                     "Very Conservative" = 7))) %>%
  
  #Recode political affiliation
  mutate_at(c(10:14), ~as.numeric(recode(., "Totally Oppose" = 1, "Strongly Oppose" = 2,
                                       "Oppose" = 3, "Somewhat oppose" = 4, "Weakly oppose" = 5,
                                       "Neutral/Neither oppose nor support" = 6, "weakly support" = 7,
                                       "Somewhat support" = 8, "Support" = 9, "Strongly Support" = 10,
                                       "Totally Support" = 11))) %>%
  
  #Recode ingroup affiliation
  mutate_at(c(15:30), ~as.numeric(recode(., "Strongly disagree" = 1, "Disagree" = 2, 
                                        "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                                        "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))) %>%
  
  
  #Recode ili
  mutate_at(c(31:45), ~as.numeric(recode(., "Strongly disagree" = 1, "Disagree" = 2, 
                                       "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))) %>%
  
  #Recode fear of covid and perceived vulnerability
  mutate_at(c(46:67), ~as.numeric(recode(., "Strongly disagree" = 1, "Disagree" = 2, 
                                       "Somewhat disagree" = 3, "Neutral" = 4,
                                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7))) 
  
#Recode perceived vulnerability
test_ili$Perceived.Vuln_3R <- 8 - test_ili$Perceived.Vuln_3
test_ili$Perceived.Vuln_5R <- 8 - test_ili$Perceived.Vuln_5
test_ili$Perceived.Vuln_11R <- 8 - test_ili$Perceived.Vuln_11
test_ili$Perceived.Vuln_12R <- 8 - test_ili$Perceived.Vuln_12
test_ili$Perceived.Vuln_13R <- 8 - test_ili$Perceived.Vuln_13
test_ili$Perceived.Vuln_14R <- 8 - test_ili$Perceived.Vuln_14

#Recode conservatism
test_ili$ACT.Conservatism_1R <- 8 - test_ili$ACT.Conservatism_1
test_ili$ACT.Conservatism_3R <- 8 - test_ili$ACT.Conservatism_3
test_ili$ACT.Conservatism_6R <- 8 - test_ili$ACT.Conservatism_6


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


#####################Conservatism CFA###############
conservatism.Model <- '
  conservatism =~ ACT.Conservatism_1R + ACT.Conservatism_2 + ACT.Conservatism_3R +
                  ACT.Conservatism_4 + ACT.Conservatism_5 + ACT.Conservatism_6R
'

con_fit <- cfa(conservatism.Model, data = test_ili, ordered = TRUE)
summary(con_fit, fit.measures = TRUE) #not great fit measures

##Yeet predicted conservatism latent valyes into dataframe
idx <- lavInspect(con_fit, "case.idx")
con_scores <- lavPredict(con_fit)
##loop over factors
for (fs in colnames(con_scores)) {
  test_ili[idx, fs] <- con_scores[ , fs]
}


############Test Fear of Covid CFA#############
fear.Model <- '
  fear =~ Fear.of.COVID_1 + Fear.of.COVID_2 + Fear.of.COVID_3 + Fear.of.COVID_4 +
          Fear.of.COVID_5 + Fear.of.COVID_6 + Fear.of.COVID_7
'

fear_fit <- cfa(fear.Model, data = test_ili, ordered = TRUE)
summary(fear_fit, fit.measures = TRUE) #Shit RMSEA, good CFI


##Yeet predicted fear of covid scores into ye olde test dataframe
idx <- lavInspect(fear_fit, "case.idx")
fear_scores <- lavPredict(fear_fit)
## loop over factors
for (fs in colnames(fear_scores)) {
  test_ili[idx, fs] <- fear_scores[ , fs]
}



############Perceived Vulnerability CFA#############
vulnerability.Model <- '
  vulnerability =~ Perceived.Vuln_1 + Perceived.Vuln_2 + Perceived.Vuln_3R + 
                   Perceived.Vuln_4 + Perceived.Vuln_5R + Perceived.Vuln_6 + 
                   Perceived.Vuln_7 + Perceived.Vuln_8 + Perceived.Vuln_9 + 
                   Perceived.Vuln_10 + Perceived.Vuln_11R + Perceived.Vuln_12R + 
                   Perceived.Vuln_13R + Perceived.Vuln_14R + Perceived.Vuln_15
'

vulnerability_fit <- cfa(vulnerability.Model, data = test_ili, ordered = TRUE)
summary(vulnerability_fit, fit.measures = TRUE) #Shit RMSEA, Shit CFI


##Yeet predicted perceived vulnerability scores into ye olde test dataframe
idx <- lavInspect(vulnerability_fit, "case.idx")
vulnerability_scores <- lavPredict(vulnerability_fit)
## loop over factors
for (fs in colnames(vulnerability_scores)) {
  test_ili[idx, fs] <- vulnerability_scores[ , fs]
}

#################Test Ingroup Affilitation CFA######################

ingroup.Model <- '
  ingroup =~ Ingroup.identity_1 + Ingroup.identity_2 + Ingroup.identity_3 + 
             Ingroup.identity_4 + Ingroup.identity_5 + Ingroup.identity_6 + 
             Ingroup.identity_7 + Ingroup.identity_8 + Ingroup.identity_9 + 
             Ingroup.identity_10 + Ingroup.identity_11 + Ingroup.identity_12 + 
             Ingroup.identity_13 + Ingroup.identity_14 + Ingroup.identity_15 + 
             Ingroup.identity_16
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
analysis_df <- test_ili[c(1, 9,  77:87)]

rm(list = setdiff(ls(), "analysis_df"))

#Making sure that the variables are set nicely (and correctly)
analysis_df$Political_Party <- as.factor(analysis_df$Political_Party)
analysis_df$Sex <- as.factor(analysis_df$Sex)
analysis_df$Age <- as.numeric(analysis_df$Age)
