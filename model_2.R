source("cleaning.R")
source("fun.R")

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

m_adv <- bf(advancement ~ political_party + age + sex)
m_proto <- bf(prototypicality ~ political_party + age + sex)
m_entre <- bf(entrepreneurship ~ political_party + age + sex)
m_imp <- bf(impresarioship ~ political_party + age + sex)

y_model <- bf(ingroup ~ advancement + prototypicality + entrepreneurship 
              + impresarioship + political_party + age + sex)

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
bayestestR::rope( #if I don't state exactly what I want they don't all come up
  m2_informed_nz,
  parameters = c(
    "advancement_political_partyNational",
    "advancement_age",
    "advancement_sexMale",
    "prototypicality_political_partyNational",
    "prototypicality_age",
    "prototypicality_sexMale",
    "entrepreneurship_political_partyNational",
    "entrepreneurship_age",
    "entrepreneurship_sexMale",
    "impresarioship_political_partyNational",
    "impresarioship_age",
    "impresarioship_sexMale",
    "ingroup_advancement",
    "ingroup_prototypicality",
    "ingroup_entrepreneurship",
    "ingroup_impresarioship",
    "ingroup_age",
    "ingroup_sexMale",
    "ingroup_political_partyNational"
  ),
  range = list(
    advancement = c(-0.05, 0.05),
    prototypicality = c(-0.05, 0.05),
    entrepreneurship = c(-0.05, 0.05),
    impresarioship = c(-0.05, 0.05),
    ingroup = c(-0.05, 0.05)
  )
)

# Model 2 - indirect / total effects--------------------------------------------
post <- as_draws_df(m2_informed_nz) %>%
  mutate(
    ind_adv = b_advancement_political_partyNational * b_ingroup_advancement,
    ind_proto = b_prototypicality_political_partyNational * b_ingroup_prototypicality,
    ind_entre = b_entrepreneurship_political_partyNational * b_ingroup_entrepreneurship,
    ind_imp = b_impresarioship_political_partyNational * b_ingroup_impresarioship,
    direct = b_ingroup_political_partyNational,
    total = direct + ind_adv + ind_proto + ind_entre + ind_imp
  )

post %>%
  summarise(
    across(
      c(ind_adv, ind_proto, ind_entre, ind_imp, direct, total),
      list(
        estimate = mean,
        lower_95 = ~quantile(.x, 0.025),
        upper_95 = ~quantile(.x, 0.975)
      )
    )
  )

results <- post %>% #View the indirect, direct and total effects in cute table
  summarise(
    across(
      c(ind_adv, ind_proto, ind_entre, ind_imp, direct, total),
      list(
        estimate = mean,
        lower_95 = ~quantile(.x, 0.025),
        upper_95 = ~quantile(.x, 0.975)
      )
    )) %>%
  pivot_longer(
    everything(),
    names_to = c("effect", ".value"),
    names_pattern = "(.*)_(estimate|lower_95|upper_95)"
  )

bayestestR::rope(
  post %>%
    select(ind_adv, ind_proto, ind_entre, ind_imp, direct, total),
  range = c(-0.05, 0.05)
)

#Model 2 cute graphs------------------------------------------------------------
agg_draw(m2_informed_nz, m2_naive_nz) %>%
  ggplot(aes(x=vals, color=model)) +
  geom_density() +
  facet_wrap(~var, scales = "free")