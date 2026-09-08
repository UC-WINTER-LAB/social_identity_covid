#making graphs for bayes analysis-----------------------------------------------
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