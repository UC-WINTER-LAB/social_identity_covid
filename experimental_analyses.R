source("cleaning.R")


##National vs Labour in in-group identity -- ANOVA
ingroup.anova <- aov(ingroup ~ Political_Party, data = analysis_df)
summary(ingroup.anova) #Significant differences in ingroup affiliation

###############Regression on political party and ili on ingroup###########
part_ili_model <- lm(ingroup ~ ili + Political_Party, data = analysis_df)
summary(part_ili_model) #Significant -- test for mediation?

###############Regression on political party, fear, vulnerability on ingroup###########
part_uncert_model <- lm(ingroup ~ fear + vulnerability + Political_Party, data = analysis_df)
summary(part_uncert_model) #vulnerability non-significant, fear -- yes


######################EXPLORATORY MEDIATION ANALYSES###############
##Complete ILI on the relationship between political party and ingroup identity
ili.model <- '
              #mediator
              ili ~ a * Political_Party
              ingroup ~ b * ili
              
              #direct effect
              ingroup ~ c * Political_Party
              
              #indirect effect
              ab := a*b
              
              #total effect
              total := c + (a*b)

'
fit <- sem(model = ili.model, data = analysis_df, se = "bootstrap", bootstrap = 1000)
summary(fit, fit.measures = TRUE) #partial mediation


#####Prototypicality on the relationship between political party and ingroup identity
proto.model <- '
              #mediator
              prototypicality ~ a * Political_Party
              ingroup ~ b * prototypicality
              
              #direct effect
              ingroup ~ c * Political_Party
              
              #indirect effect
              ab := a*b
              
              #total effect
              total := c + (a*b)

'
proto_fit <- sem(model = proto.model, data = analysis_df, se = "bootstrap", bootstrap = 1000)
summary(proto_fit, fit.measures = TRUE) #partial mediation (stronger than previous)


#####Fear of Covid on the relationship between ILI and ingroup identity
fear.model <- '
              #mediator
              fear ~ a * ili
              ingroup ~ b * fear
              
              #direct effect
              ingroup ~ c * ili
              
              #indirect effect
              ab := a*b
              
              #total effect
              total := c + (a*b)

'
fear_fit <- sem(model = fear.model, data = analysis_df, se = "bootstrap", bootstrap = 1000)
summary(fear_fit, fit.measures = TRUE) #no mediation

#####Vulnerabiliry on the relationship between ILI and ingroup identity
vuln.model <- '
              #mediator
              vulnerability ~ a * ili
              ingroup ~ b * vulnerability
              
              #direct effect
              ingroup ~ c * ili
              
              #indirect effect
              ab := a*b
              
              #total effect
              total := c + (a*b)

'
vuln_fit <- sem(model = vuln.model, data = analysis_df, se = "bootstrap", bootstrap = 1000)
summary(vuln_fit, fit.measures = TRUE) #no mediation


#########################OLD ANALYSES BELOW#########################

###############Regression on political party, sex, age, on ingroup###########
ingroup_model <- lm(ingroup ~ Age + Sex + Political_Party, data = analysis_df)
summary(ingroup_model)


#########New Regressions on political party, ili, and subfactors##################
#Overall ILI
ili_model <- lm(ili ~ Age + Sex + Political_Party, data = analysis_df) 
summary(ili_model) #SIGNIFICANT

#Prototypicality
proto_model <- lm(prototypicality ~ Age + Sex + Political_Party, data = analysis_df) 
summary(proto_model) #SIGNIFICANT

#Advancement
adv_model <- lm(advancement ~ Age + Sex + Political_Party, data = analysis_df) 
summary(adv_model) #SIGNIFICANT

#Entrepreneurship
entre_model <- lm(entrepreneurship ~ Age + Sex + Political_Party, data = analysis_df) 
summary(entre_model) #SIGNIFICANT

#Impresarioship
impres_model <- lm(impresarioship ~ Age + Sex + Political_Party, data = analysis_df) 
summary(impres_model) #SIGNIFICANT



###################Old Regressions#################################

#Regression on original political affiliation and ingroup affiliation - SIGN
paia_model <- lm(ingroup ~ politics, data = analysis_df)
summary(paia_model)

#Regression on original political affiliation and prototypicality - SIGN
papro_model <- lm(prototypicality ~ politics, data = analysis_df)
summary(papro_model)

#Regression on original political affiliation and advancement - SIGN
paad_model <- lm(advancement ~ politics, data = analysis_df)
summary(paad_model)

#Regression on original political affiliation and entrepreneurship - SIGN
paen_model <- lm(entrepreneurship  ~ politics, data = analysis_df)
summary(paen_model)

#Regression on original political affiliation and impresarioship - SIGN
paim_model <- lm(impresarioship ~ politics, data = analysis_df)
summary(paim_model)


###################Test Correlations on Predicted Values ################

#Note "politics" is original predicted variable, not categorical one
df_Cor <- analysis_df %>%
  select(prototypicality, advancement, entrepreneurship, impresarioship,
         fear, politics, ingroup)

rcorr(as.matrix(df_Cor))



#############Test Correlations via Normal Score Calculations################

#########Calculating ili score
analysis_df$ILI_Score <- rowMeans(test_ili[,c(1:15)], na.rm = TRUE)

#Calculating fear of covid score
analysis_df$FOC_Score <- rowSums(test_ili[,c(16:22)], na.rm = TRUE)



#Correlation -- sign, but weak correlation, also skewed to the right
ggscatter(analysis_df, x = "ILI_Score", y = "FOC_Score", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Identity Leadership", 
          ylab = "Fear of Covid")