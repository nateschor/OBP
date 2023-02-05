
pacman::p_load(
  tidyverse,
  tidylog,
  here,
  tidymodels,
  tictoc,
  ggthemes
)

df_training_raw <- read_csv(here("data/modeling/df_training.csv")) %>% 
  glimpse()

df_training <- df_training_raw %>% 
  filter(yearID <= 2017) %>% 
  select(bbrefID, yearID, cur_OBP, lagged_OBP_1:lagged_OBP_3, lagged_BB_1:lagged_BB_3) %>% 
  group_by(bbrefID) %>% 
  mutate(
    across(contains("lagged"), ~ coalesce(., mean(., na.rm = TRUE))) # if lagged value is missing, replace with the player's mean for that value
  ) %>% 
  ungroup() %>%
  group_by(yearID) %>% 
  mutate(
    across(
      contains("lagged"), ~ coalesce(., mean(., na.rm = TRUE)) # if lagged value still missing, replace with the league avarage of the lag for that year
    )
  ) %>% 
  ungroup()

df_validation <- df_training_raw %>% 
  filter(yearID %in% 2018:2020) %>% 
  select(bbrefID, yearID, cur_OBP, lagged_OBP_1:lagged_OBP_3, lagged_BB_1:lagged_BB_3) %>% 
  group_by(bbrefID) %>% 
  mutate(
    across(contains("lagged"), ~ coalesce(., mean(., na.rm = TRUE))) # if lagged value is missing, replace with the player's mean for that value
  ) %>% 
  ungroup() %>%
  group_by(yearID) %>% 
  mutate(
    across(
      contains("lagged"), ~ coalesce(., mean(., na.rm = TRUE)) # if lagged value still missing, replace with the league avarage of the lag for that year
    )
  ) %>% 
  ungroup()

Fit_Splines <- function(predictor_list, dof) {
  
  splines_recipe <- recipe(cur_OBP ~ ., data = df_training) %>% 
    update_role(bbrefID, yearID, new_role = "id") %>% 
    step_intercept() %>% 
    step_ns(predictor_list, deg_free = dof) %>% 
    prep(., strings_as_factors = FALSE)
  
  df_training_baked <- bake(splines_recipe, df_training) %>% 
    select(contains("_"))
  
  df_validation_baked <- bake(splines_recipe, df_validation) %>% 
    select(contains("_"))
  
  model_splines <- lm(cur_OBP ~ ., data = df_training_baked) 
  
  df_training_fitted <- augment(model_splines)
  
  train_rmse <- rmse_vec(
    df_training_fitted$cur_OBP,
    df_training_fitted$.fitted
  )
  
  df_validation_fitted <- augment(x = model_splines, newdata = df_validation_baked)
  
  val_rmse <- rmse_vec(
    df_validation_fitted$cur_OBP,
    df_validation_fitted$.fitted
  )
  
  tibble(
    "Spline DoF" = dof,
    "Predictors" = list(predictor_list),
    "Training RMSE" = train_rmse,
    "Validation RMSE"  = val_rmse 
  )
  
}

v_spline_dof <- 2:10

df_OBP_2 <- map_dfr(v_spline_dof, ~ Fit_Splines(c("lagged_OBP_1", "lagged_OBP_2"), .))
df_OBP_3 <- map_dfr(v_spline_dof, ~ Fit_Splines(c("lagged_OBP_1", "lagged_OBP_2", "lagged_OBP_3"), .))
df_OBP_2_BB_1 <- map_dfr(v_spline_dof, ~ Fit_Splines(c("lagged_OBP_1", "lagged_OBP_2", "lagged_BB_1"), .))
df_OBP_2_BB_2 <- map_dfr(v_spline_dof, ~ Fit_Splines(c("lagged_OBP_1", "lagged_OBP_2", "lagged_BB_1",  "lagged_BB_2"), .))
df_OBP_3_BB_2 <- map_dfr(v_spline_dof, ~ Fit_Splines(c("lagged_OBP_1", "lagged_OBP_2", "lagged_OBP_3", "lagged_BB_1", "lagged_BB_2"), .))

df_splines_models <- bind_rows(
  df_OBP_2,
  df_OBP_3,
  df_OBP_2_BB_1,
  df_OBP_2_BB_2,
  df_OBP_3_BB_2
) %>% 
  group_by(Predictors) %>% 
  mutate(
    `Group ID` = cur_group_id() %>% as.factor() # https://stackoverflow.com/questions/39650511/r-group-by-variable-and-then-assign-a-unique-id
  )
  
ggplot(df_splines_models, aes(x = `Spline DoF`, y = `Validation RMSE`, color = `Group ID`)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_ptol() +
  scale_x_continuous(breaks = 2:10) +
  labs(
    x = "Spline Degrees of Freedom",
    y = "Validation RMSE"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    text = element_text(size = 14)
  )

df_splines_models %>% 
  filter(`Spline DoF` == 3, `Group ID` == 2)

