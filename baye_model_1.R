source("cleaning.R")

#Model 1 Pol_orint -> ingroup---------------------------------------------------
priors_1 <- c(
  prior(normal(0, 1), class = "b") 
)

m1 <- brm(
  ingroup ~ political_party + age + sex,
  data = analysis_df,
  family = gaussian,
  prior = priors_1,
  chains = 4, cores = 4,
)

m1
bayestestR::rope(
  m1,
  range = c(-0.05, 0.05)
)
