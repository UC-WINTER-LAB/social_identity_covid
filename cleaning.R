library(tidyverse)
library(rdrop2)
library(sjmisc)
library(psych)
library(lavaan)
library(Hmisc)
library(ggpubr)

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


test_ili <- sil_df[c(68:89)]

###################Test Recoding####################

###Identity Leadership Inventory
test_ili <- test_ili %>%
  mutate_at(c(1:15), ~as.numeric(recode(., "Strongly disagree" = 1, "Disagree" = 2, 
                                       "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7)))

test_ili <- test_ili %>%
  mutate_at(c(16:22), ~as.numeric(recode(., "Strongly disagree" = 1, "Disagree" = 2, 
                                       "Somewhat disagree" = 3, "Neutral" = 4,
                                       "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7)))


########Test ILI CFA

leadership.Model <- '
  prototypicality =~ ID.leadership_1 + ID.leadership_2 +  ID.leadership_3 + ID.leadership_4

  advancement =~ ID.leadership_5 + ID.leadership_6 + ID.leadership_7 + ID.leadership_8

  entrepreneurship =~ ID.leadership_9 + ID.leadership_10 + ID.leadership_11 + ID.leadership_12

  impresarioship =~ ID.leadership_13 + ID.leadership_14 + ID.leadership_15 
'

leadership_fit <- cfa(leadership.Model, data = test_ili)
summary(leadership_fit, fit.measures = TRUE) #Good RMSEA, good CFI


##Yeet predicted ili scores into ye olde test dataframe
idx <- lavInspect(leadership_fit, "case.idx")
ili_scores <- lavPredict(leadership_fit)
## loop over factors
for (fs in colnames(ili_scores)) {
  test_ili[idx, fs] <- ili_scores[ , fs]
}


############Test Fear of Covid CFA#############
fear.Model <- '
  fear =~ Fear.of.COVID_1 + Fear.of.COVID_2 + Fear.of.COVID_3 + Fear.of.COVID_4 +
          Fear.of.COVID_5 + Fear.of.COVID_6 + Fear.of.COVID_7
'

fear_fit <- cfa(fear.Model, data = test_ili)
summary(fear_fit, fit.measures = TRUE) #Shit RMSEA, ehh CFI


##Yeet predicted fear of covid scores into ye olde test dataframe
idx <- lavInspect(fear_fit, "case.idx")
fear_scores <- lavPredict(fear_fit)
## loop over factors
for (fs in colnames(fear_scores)) {
  test_ili[idx, fs] <- fear_scores[ , fs]
}


###################Test Correlations on Predicted Values ################


df_Cor <- test_ili %>%
  select(prototypicality, advancement, entrepreneurship, impresarioship,
         fear)

rcorr(as.matrix(df_Cor))


#############Test Correlations via Normal Score Calculations################
#########Calculating ili score
test_ili$ILI_Score <- rowMeans(test_ili[,c(1:15)], na.rm = TRUE)

#Calculating fear of covid score
test_ili$FOC_Score <- rowSums(test_ili[,c(16:22)], na.rm = TRUE)



#Correlation -- sign, but weak correlation, also skewed to the right
ggscatter(test_ili, x = "ILI_Score", y = "FOC_Score", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Identity Leadership", 
          ylab = "Fear of Covid")
