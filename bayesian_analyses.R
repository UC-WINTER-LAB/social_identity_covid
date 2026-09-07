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
bayestestR::rope(
  m1,
  range = c(-0.05, 0.05)
)

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
bayestestR::rope(m2_informed_nz)

bayestestR::rope(
  m2_informed_nz,
  parameters = c(
    "advancement_political_partyNational",
    "prototypicality_political_partyNational",
    "entrepreneurship_political_partyNational",
    "impresarioship_political_partyNational",
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

#Model 3 two mediators proto and ACT--------------------------------------------
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

m3_act <- bf(act ~ political_party + age + sex)
m3_proto <- bf(prototypicality ~ political_party + age + sex)

z_model <- bf(ingroup ~ act + prototypicality +
                political_party + age + sex
)

m3_naive_nz <- brm(
  m3_proto + m3_act + z_model + set_rescor(FALSE),
  data = analysis_df,
  family = gaussian,
  chains = 4, cores = 4
)

priors_ili_3 <- c(
  prior(normal(0, 1), class = "b", resp = "act"),
  prior(normal(0, 1), class = "b", resp = "prototypicality"),
  prior(normal(0, 1), class = "b", resp = "ingroup")
)

m3_informed_nz <- brm(
  m3_act + m3_proto + z_model + set_rescor(FALSE),
  prior = priors_ili_3,
  data = analysis_df,
  family = gaussian,
  sample_prior = "yes",
  chains = 4,
  cores = 4
)

summary(m3_informed_nz)
bayestestR::rope(m3_informed_nz)


bayestestR::rope(
  m3_informed_nz,
  parameters = c(
    "act_political_partyNational",
    "prototypicality_political_partyNational",
    "ingroup_political_partyNational"
  ),
  range = list(
    act = c(-0.05, 0.05),
    prototypicality = c(-0.05, 0.05),
    ingroup = c(-0.05, 0.05)
  )
)

# Model 3 - indirect / total effects--------------------------------------------
post <- as_draws_df(m3_informed_nz) %>%
  mutate(
    ind3_act = b_act_political_partyNational * b_ingroup_act,
    ind3_proto = b_prototypicality_political_partyNational * b_ingroup_prototypicality,
    direct = b_ingroup_political_partyNational,
    total = direct + ind3_act + ind3_proto
  )

post %>%
  summarise(
    across(
      c(ind3_proto, ind3_act, direct, total),
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
      c(ind3_proto, ind3_act, direct, total),
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
    select(ind3_proto, ind3_act, direct, total),
  range = c(-0.05, 0.05)
)

#Model 3 cute graphs------------------------------------------------------------
agg_draw(m3_informed_nz, m3_naive_nz) %>%
  ggplot(aes(x=vals, color=model)) +
  geom_density() +
  facet_wrap(~var, scales = "free")

#Model 4 two mediators----------------------------------------------------------
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

m_con <- bf(conservatism ~ political_party + age + sex)
m_proto <- bf(prototypicality ~ political_party + age + sex)

z_model <- bf(ingroup ~ conservatism + prototypicality +
  political_party + age + sex
)

m3_naive_nz <- brm(
  m_proto + m_con + z_model + set_rescor(FALSE),
  data = analysis_df,
  family = gaussian,
  chains = 4, cores = 4
)

priors_ili_3 <- c(
  prior(normal(0, 1), class = "b", resp = "conservatism"),
  prior(normal(0, 1), class = "b", resp = "prototypicality"),
  prior(normal(0, 1), class = "b", resp = "ingroup")
)

m3_informed_nz <- brm(
  m_con + m_proto + z_model + set_rescor(FALSE),
  prior = priors_ili_3,
  data = analysis_df,
  family = gaussian,
  sample_prior = "yes",
  chains = 4,
  cores = 4
)

summary(m3_informed_nz)
bayestestR::rope(m3_informed_nz)

# Model 4 - indirect / total effects--------------------------------------------
post <- as_draws_df(m3_informed_nz) %>%
  mutate(
    ind_con = b_conservatism_political_partyNational * b_ingroup_conservatism,
    ind_proto = b_prototypicality_political_partyNational * b_ingroup_prototypicality,
    direct = b_ingroup_political_partyNational,
    total = direct + ind_con + ind_proto
  )

post %>%
  summarise(
    across(
      c(ind_proto, ind_con, direct, total),
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
      c(ind_proto, ind_con, direct, total),
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
    select(ind_proto, ind_con, direct, total),
  range = c(-0.06, 0.06)
)

#Model 4 cute graphs------------------------------------------------------------
agg_draw(m3_informed_nz, m3_naive_nz) %>%
  ggplot(aes(x=vals, color=model)) +
  geom_density() +
  facet_wrap(~var, scales = "free")
