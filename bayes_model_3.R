source("cleaning.R")
source("fun.R")

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