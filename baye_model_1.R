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
                   ind_act := a2 * b2

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

post_m3 <- as_draws_df(m3_informed_nz)
bayestestR::rope(
  post_m3 %>%
    select(
      b_act_political_partyNational,
      b_act_age,
      b_act_sexMale,
      
      b_prototypicality_political_partyNational,
      b_prototypicality_age,
      b_prototypicality_sexMale,
      
      b_ingroup_act,
      b_ingroup_prototypicality,
      b_ingroup_age,
      b_ingroup_sexMale,
      b_ingroup_political_partyNational
    ),
  range = c(-0.05, 0.05)
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

#Model 4 political party and orientation on ACT, proto, and ingroup-------------
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

m4_act <- bf(
  act ~ political_party + political_beliefs + age + sex)

m4_proto <- bf(
  prototypicality ~ political_party + political_beliefs + age + sex)

x_model <- bf(
  ingroup ~ act + prototypicality +
    political_party + political_beliefs + age + sex)

m4_naive_nz <- brm(
  m4_act + m4_proto + x_model + set_rescor(FALSE),
  data = analysis_df,
  family = gaussian,
  chains = 4, cores = 4
)

m4_informed_nz <- brm(
  m4_act + m4_proto + x_model + set_rescor(FALSE),
  prior = priors_ili_3,
  data = analysis_df,
  family = gaussian,
  sample_prior = "yes",
  chains = 4,
  cores = 4
)

summary(m4_informed_nz)

post_m4 <- as_draws_df(m4_informed_nz)
bayestestR::rope(
  post_m4 %>%
    select(
      b_act_political_partyNational,
      b_act_political_beliefs,
      b_act_age,
      b_act_sexMale,
      
      b_prototypicality_political_partyNational,
      b_prototypicality_political_beliefs,
      b_prototypicality_age,
      b_prototypicality_sexMale,
      
      b_ingroup_act,
      b_ingroup_prototypicality,
      b_ingroup_age,
      b_ingroup_sexMale,
      b_ingroup_political_partyNational, 
      b_ingroup_political_beliefs
    ),
  range = c(-0.05, 0.05)
)

# Model 4 - indirect / total effects--------------------------------------------
post <- as_draws_df(m4_informed_nz) %>%
  mutate(
    # Political Party indirect effects
    ind_act_part = b_act_political_partyNational * b_ingroup_act,
    ind_proto_part = b_prototypicality_political_partyNational * b_ingroup_prototypicality,
    
    # Political Beliefs indirect effects
    ind_act_orie = b_act_political_beliefs * b_ingroup_act,
    ind_proto_orie = b_prototypicality_political_beliefs * b_ingroup_prototypicality,
    
    # Direct effects
    direct_part = b_ingroup_political_partyNational,
    direct_orie = b_ingroup_political_beliefs,
    
    # Total effects
    total_part = direct_part + ind_act_part + ind_proto_part,
    total_orie = direct_orie + ind_act_orie + ind_proto_orie
  )

post %>%
  summarise(
    across(
      c(ind_act_part, ind_proto_part, ind_act_orie, ind_proto_orie, 
        direct_part, direct_orie, total_part, total_orie),
      list(
        estimate = mean,
        lower_95 = ~quantile(.x, 0.025),
        upper_95 = ~quantile(.x, 0.975)
      )
    )
  )

results_4 <- post %>% #View the indirect, direct and total effects in cute table
  summarise(
    across(
      c(ind_act_part, ind_proto_part, ind_act_orie, ind_proto_orie, 
        direct_part, direct_orie, total_part, total_orie),
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
    select(
      ind_act_part,
      ind_proto_part,
      ind_act_orie,
      ind_proto_orie,
      direct_part,
      direct_orie,
      total_part,
      total_orie
    ),
  range = c(-0.05, 0.05)
)

#Model 4 cute graphs------------------------------------------------------------
agg_draw(m4_informed_nz, m4_naive_nz) %>%
  ggplot(aes(x=vals, color=model)) +
  geom_density() +
  facet_wrap(~var, scales = "free")
