source("cleaning.R")

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



###################Test Political Affiliation CFA######################

politics.Model <- '
  politics =~ Political.party.sup_1 + Political.party.sup_2 + Political.party.sup_3 + 
              Political.party.sup_4 + Political.party.sup_5
'

politics_fit <- cfa(politics.Model, data = test_ili)
summary(politics_fit, fit.measures = TRUE) #both rmsea and cfi are bad


#Join predicted values into the dataset
idx <- lavInspect(politics_fit, "case.idx")
politics_scores <- lavPredict(politics_fit)
## loop over factors
for (fs in colnames(politics_scores)) {
  test_ili[idx, fs] <- politics_scores[ , fs]
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

ingroup_fit <- cfa(ingroup.Model, data = test_ili)
summary(ingroup_fit, fit.measures = TRUE) #both rmsea and cfi are bad


#Join predicted values into the dataset
idx <- lavInspect(ingroup_fit, "case.idx")
ingroup_scores <- lavPredict(ingroup_fit)
## loop over factors
for (fs in colnames(ingroup_scores)) {
  test_ili[idx, fs] <- ingroup_scores[ , fs]
}

###################Test Correlations on Predicted Values ################


df_Cor <- test_ili %>%
  select(prototypicality, advancement, entrepreneurship, impresarioship,
         fear, politics, ingroup)

rcorr(as.matrix(df_Cor))


###################Regressions#################################

#Regression on political affiliation and ingroup affiliation - SIGN
paia_model <- lm(ingroup ~ politics, data = test_ili)
summary(paia_model)

#Regression on political affiliation and prototypicality - SIGN
papro_model <- lm(prototypicality ~ politics, data = test_ili)
summary(papro_model)

#Regression on political affiliation and advancement - SIGN
paad_model <- lm(advancement ~ politics, data = test_ili)
summary(paad_model)

#Regression on political affiliation and entrepreneurship - SIGN
paen_model <- lm(entrepreneurship  ~ politics, data = test_ili)
summary(paen_model)

#Regression on political affiliation and impresarioship - SIGN
paim_model <- lm(impresarioship ~ politics, data = test_ili)
summary(paim_model)


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