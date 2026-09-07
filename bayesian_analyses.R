source("cleaning.R")

#Cleaning code------------------------------------------------------------------
analysis_df <- analysis_df %>%
  janitor::clean_names()

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
bayestestR::rope(m1)

#Model 2 SEM--------------------------------------------------------------------
ili.model <- '
              #a paths
              advancement ~ a1 * Political_Party
              prototypicality ~ a2 * Political_Party
              entrepreneurship ~ a3 * Political_Party
              impresarioship ~ a4 * Political_Party
              #b paths  
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

m_adv <- bf(advancement ~ Political_Party + Age + Sex)
m_proto <- bf(prototypicality ~ Political_Party + Age + Sex)
m_entre <- bf(entrepreneurship ~ Political_Party + Age + Sex)
m_imp <- bf(impresarioship ~ Political_Party + Age + Sex)

y_model <- bf(ingroup ~ advancement + prototypicality + entrepreneurship 
              + impresarioship + Political_Party + Age + Sex)

m2_naive_nz <- brm(
  m_adv + m_proto + m_entre + m_imp + y_model + set_rescor(FALSE),
  data = analysis_df,
  family = gaussian,
  chains = 4, cores = 4
)

priors_ili_nz <- c(
  prior(normal(0, 1), class = "b", resp = "advancement"),
  prior(normal(0, 1), class = "b", resp = "prototypicality"),
  prior(normal(0, 1), class = "b", resp = "entrepreneurship"),
  prior(normal(0, 1), class = "b", resp = "impresarioship"),
  prior(normal(0, 1), class = "b", resp = "ingroup")
)

m2_informed_nz <- brm(
  m_adv + m_proto + m_entre + m_imp + y_model + set_rescor(FALSE),
  prior = priors_ili_nz,
  data = analysis_df,
  family = gaussian,
  sample_prior = "yes",
  chains = 4, cores = 4
)

summary(m2_informed_nz)
bayestestR::rope(m2_informed_nz)

# Model 2 - indirect / total effects
post <- as_draws_df(m2_informed_nz) %>%
  mutate(
    ind_adv = b_advancement_Political.Beliefs * b_ingroup_advancement,
    ind_proto = b_prototypicality_Political.Beliefs * b_ingroup_prototypicality,
    ind_entre = b_entrepreneurship_Political.Beliefs * b_ingroup_entrepreneurship,
    ind_imp = b_impresarioship_Political.Beliefs * b_ingroup_impresarioship,
    direct = b_ingroup_Political.Beliefs,
    total = direct + ind_adv + ind_proto + ind_entre + ind_imp
  )

post %>% 
  summarise(across(everything(), function(x){mean(x > 0)})) %>%
  select(starts_with("b_")) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  filter(!between(V1, 0.05, 0.95))

agg_draw <- function(naive_model, informed_model) {
  bind_rows(
    as_draws_df(naive_model) %>%
      select(starts_with("b_"), starts_with("prior_b_"), -contains("Intercept")) %>%
      pivot_longer(cols = everything(), names_to = "var", values_to = "vals") %>%
      mutate(model = "likelihood"),
    as_draws_df(informed_model) %>%
      select(starts_with("b_"), starts_with("prior_b_"), -contains("Intercept")) %>%
      pivot_longer(cols = everything(), names_to = "var", values_to = "vals") %>%
      mutate(model = ifelse(grepl("prior", var), "prior", "posterior")) %>%
      mutate(var = gsub("prior_", "", var))
  )
}

agg_draw(m2_informed_nz, m2_naive_nz) %>%
  ggplot(aes(x=vals, color=model)) +
  geom_density() +
  facet_wrap(~var, scales = "free")
