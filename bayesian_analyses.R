source("cleaning.R")

#Cleaning code------------------------------------------------------------------
analysis_df <- analysis_df %>%
  janitor::clean_names()

colnames(analysis_df)

#Model 1 Pol_orint -> ingroup---------------------------------------------------
priors_1 <- c(
  prior(normal(0, 0.5), class = "b") 
)

m1 <- brm(
  ingroup ~ political_party + age + sex,
  data = analysis_df,
  family = gaussian,
  prior = priors_1,
  chains = 4, cores = 4,
)

m1
bayestestR::rope(m1)