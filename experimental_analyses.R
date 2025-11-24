source("cleaning.R")

##Demographics
frq(analysis_df$Sex)
frq(analysis_df$Ethnicity)

analysis_df %>%
  summarise(
    MeanAge = mean(Age, na.rm = TRUE), SDAge = sd(Age, na.rm = TRUE),
    MinAge = min(Age, na.rm = TRUE), MaxAge = max(Age, na.rm = TRUE)
    )


#####################MODEL TESTING############################

###########Model 1: political party/orientation on ingroup

#Regression on political party/orientation, sex, age, on ingroup
politic_model <- lm(ingroup ~ Political_Party + Political.Beliefs + Age + Sex, data = analysis_df)
summary(politic_model)


###########Model 2: impact of ili on political party on ingroup

##Multiple Mediation (all ili subfactors) on political party
party.model <- '
                #mediators
                advancement ~ a1 * Political_Party
                prototypicality ~ a2 * Political_Party
                entrepreneurship ~ a3 * Political_Party
                impresarioship ~ a4 * Political_Party
                
                ingroup ~ b1 * advancement
                ingroup ~ b2 * prototypicality
                ingroup ~ b3 * entrepreneurship
                ingroup ~ b4 * impresarioship
                
                #direct effect
                ingroup ~ c * Political_Party
                
                #indirect
                ind_adv := a1 * b1
                ind_proto := a2 * b2
                ind_entre := a3 * b3
                ind_imp := a4 * b4
                
                #total effect
                total := ind_adv + ind_proto + ind_entre + ind_imp + c

'
ili_party_fit <- sem(model = party.model, data = analysis_df, se = "bootstrap", 
                     bootstrap = 1000)
summary(ili_party_fit, fit.measures = TRUE)


#########Model 3: introducing authoritarianism

##Two Mediators (Proto and ACT) on political party & ingroup
act_party.model <- '
                   #mediators
                   prototypicality ~ a1 * Political_Party + Age + Sex
                   act ~ a2 * Political_Party + Age + Sex

                   ingroup ~ b1 * prototypicality + b2 * act

                   #direct effect
                   ingroup ~ c * Political_Party + Age + Sex

                   #indirect effect
                   ind_proto := a1 * b1
                   ind_act := a2 * a2

                   #total effect
                   total := ind_proto + ind_act + c
'
act_party_fit <- sem(model = act_party.model, data = analysis_df, se = "bootstrap", 
                     bootstrap = 1000)
summary(act_party_fit, fit.measures = TRUE)


##########Model 4: political party and orientation on ACT, proto, and ingroup

act_part_orie_model <- '
                       #mediators
                       prototypicality ~ a1 * Political_Party + a3 * Political.Beliefs + Age + Sex
                       act ~ a2 * Political_Party + a4 * Political.Beliefs + Age + Sex
    
                       ingroup ~ b1 * prototypicality + b2 * act

                       #direct effect
                       ingroup ~ c1 * Political_Party + c2 * Political.Beliefs + Age + Sex

                       #indirect effect
                       ind_proto_part := a1 * b1
                       ind_act_part := a2 * b2

                       ind_proto_orie := a3 * b1
                       ind_act_orie := a4 * b2

                       #total effect
                       total1 := ind_proto_part + ind_act_part + c1
                       total2 := ind_proto_orie + ind_act_orie + c2
'

act_part_orie_fit <- sem(model = act_part_orie_model, data = analysis_df, se = "bootstrap",
                         bootstrap = 1000)
summary(act_part_orie_fit, fit.measures = TRUE)
parameterEstimates(act_part_orie_fit, level = 0.95, boot.ci.type = "perc")

#########################OLD ANALYSES BELOW#########################

######################REGRESSIONS############################

#Regression on political party and ili on ingroup###########
part_ili_model <- lm(ingroup ~ ili + Political_Party + Age + Sex, data = analysis_df)
summary(part_ili_model) #Significant -- test for mediation?


#Regression on political party, fear, vulnerability on ingroup###########
part_uncert_model <- lm(ingroup ~ fear + vulnerability + Political_Party, data = analysis_df)
summary(part_uncert_model) #vulnerability non-significant, fear -- yes


#Regression on political orientation, ili, age, and sex on ingroup
orient_model <- lm(ingroup ~ Political.Beliefs + advancement + impresarioship + prototypicality +
                     entrepreneurship + Age + Sex, data = analysis_df)
summary(orient_model)


#Regression on ili, age, and sex on ingroup
orient2_model <- lm(ingroup ~ advancement + impresarioship + prototypicality +
                      entrepreneurship + Age + Sex, data = analysis_df)
summary(orient2_model)


#############TESTING FOR MODERATION#####################


#Moderation regression (ili on ingroup, party as moderator)
moder_model <- lm(ingroup ~ ili + Age + Sex + ili*Political_Party, data = analysis_df)
summary(moder_model)

ggplot(analysis_df, aes(ili, ingroup, color = factor(Political_Party))) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  scale_colour_manual(name = "Political Party", labels = c("Labour", "National"),
                      values = c("#FF6666", "#6666FF"))


############################MEDIATIONS###########################

##Multiple Mediation (all ili subfactors) on political orientation
orient_ili.model <- '
                #mediators
                advancement ~ a1 * Political.Beliefs
                prototypicality ~ a2 * Political.Beliefs
                entrepreneurship ~ a3 * Political.Beliefs
                impresarioship ~ a4 * Political.Beliefs
                
                ingroup ~ b1 * advancement
                ingroup ~ b2 * prototypicality
                ingroup ~ b3 * entrepreneurship
                ingroup ~ b4 * impresarioship
                
                #direct effect
                ingroup ~ c * Political.Beliefs
                
                #indirect
                ind_adv := a1 * b1
                ind_proto := a2 * b2
                ind_entre := a3 * b3
                ind_imp := a4 * b4
                
                #total effect
                total := ind_adv + ind_proto + ind_entre + ind_imp + c

'
orient_ili_fit <- sem(model = orient_ili.model, data = analysis_df, se = "bootstrap", 
                      bootstrap = 1000)
summary(orient_ili_fit, fit.measures = TRUE)



##Multiple Mediation (all ili subfactors) on political party and orientation
full.model <- '
                #mediators
                advancement ~ a1 * Political_Party + a5 * Political.Beliefs + Age + Sex
                prototypicality ~ a2 * Political_Party + a6 * Political.Beliefs + Age + Sex
                entrepreneurship ~ a3 * Political_Party + a7 * Political.Beliefs + Age + Sex
                impresarioship ~ a4 * Political_Party + a8 * Political.Beliefs + Age + Sex
                
                ingroup ~ b1 * advancement + b2 * prototypicality + b3 * entrepreneurship + b4 * impresarioship
                
                #direct effect
                ingroup ~ c1 * Political_Party + c2 * Political.Beliefs + Age + Sex
                
                #indirect
                ind_adv_part := a1 * b1
                ind_proto_part := a2 * b2
                ind_entre_part := a3 * b3
                ind_imp_part := a4 * b4
                
                ind_adv_orie := a5 * b1
                ind_proto_orie := a6 * b2
                ind_entre_orie := a7 * b3
                ind_imp_orie := a8 * b4
                
                #total effect
                total1 := ind_adv_part + ind_proto_part + ind_entre_part + ind_imp_part + c1
                total2 := ind_adv_orie + ind_proto_orie + ind_entre_orie + ind_imp_orie + c2

'
full_fit <- sem(model = full.model, data = analysis_df, se = "bootstrap", 
                bootstrap = 1000)
summary(full_fit, fit.measures = TRUE)
parameterEstimates(full_fit, level = 0.95, boot.ci.type = "perc")


##Two Mediators (Proto and Conservatism) on political party & ingroup
con_party.model <- '
                   #mediators
                   prototypicality ~ a1 * Political_Party + Age + Sex
                   conservatism ~ a2 * Political_Party + Age + Sex

                   ingroup ~ b1 * prototypicality + b2 * conservatism

                   #direct effect
                   ingroup ~ c * Political_Party + Age + Sex

                   #indirect effect
                   ind_proto := a1 * b1
                   ind_conserv := a2 * a2

                   #total effect
                   total := ind_proto + ind_conserv + c
'
con_party_fit <- sem(model = con_party.model, data = analysis_df, se = "bootstrap", 
                     bootstrap = 1000)
summary(con_party_fit, fit.measures = TRUE)


##Two Mediators (Proto and Conservatism) on political orientation & ingroup
con_orientation.model <- '
                   #mediators
                   prototypicality ~ a1 * Political.Beliefs + Age + Sex
                   conservatism ~ a2 * Political.Beliefs + Age + Sex

                   ingroup ~ b1 * prototypicality + b2 * conservatism

                   #direct effect
                   ingroup ~ c * Political.Beliefs + Age + Sex

                   #indirect effect
                   ind_proto := a1 * b1
                   ind_conserv := a2 * a2

                   #total effect
                   total := ind_proto + ind_conserv + c
'
con_orientation_fit <- sem(model = con_orientation.model, data = analysis_df, se = "bootstrap", 
                           bootstrap = 1000)
summary(con_orientation_fit, fit.measures = TRUE)





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
party_fit <- sem(model = ili.model, data = analysis_df, se = "bootstrap", bootstrap = 1000)
summary(fit, fit.measures = TRUE) #partial mediation


#Complete ILI on the relationship between political orientation and ingroup
ili_orient.model <- '
              #mediator
              ili ~ a * Political.Beliefs
              ingroup ~ b * ili
              
              #direct effect
              ingroup ~ c * Political.Beliefs
              
              #indirect effect
              ab := a*b
              
              #total effect
              total := c + (a*b)

'
orientILI_fit <- sem(model = ili_orient.model, data = analysis_df, se = "bootstrap", bootstrap = 1000)
summary(orientILI_fit, fit.measures = TRUE) #partial mediation


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


########################ANOVAS#####################################


##National vs Labour in in-group identity -- ANOVA
ingroup.anova <- aov(ingroup ~ Political_Party, data = analysis_df)
summary(ingroup.anova) #Significant differences in ingroup affiliation

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