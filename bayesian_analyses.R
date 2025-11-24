library(brms)
library(cmdstanr)

analysis_df <- analysis_df %>%
  janitor::clean_names()

colnames(analysis_df)

priors <- c(
  prior(normal(0, 0.5), class = "b") 
)

m1 <- brm(
  ingroup ~ political_beliefs + age + sex,
  data = analysis_df,
  family = gaussian,
  prior = priors,
  chains = 4, cores = 4,
  backend = 'cmdstan'
)


m1