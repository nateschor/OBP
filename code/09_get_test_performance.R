
pacman::p_load(
  tidyverse,
  tidylog,
  tidymodels,
  here
)


df_training_raw <- read_csv(here("data/modeling/df_training.csv")) %>% 
  glimpse()

df_test_raw <- read_csv(here("data/modeling/df_test.csv")) %>% 
  select(bbrefID, yearID, cur_OBP = OBP, num_range("lagged_OBP_", 1:3)) %>% 
  glimpse()

df_training <- df_training_raw %>% 
  select(bbrefID, yearID, cur_OBP, num_range("lagged_OBP_", 1:3)) %>% 
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
  ungroup() %>% 
  glimpse()

df_test_na_filled <- df_test_raw %>% 
  select(bbrefID, contains("lagged")) %>% 
  pivot_longer(., -bbrefID) %>% 
  group_by(bbrefID) %>% 
  mutate(
    own_player_mean = coalesce(value, mean(value, na.rm = TRUE))
  ) %>% 
  mutate(
    training_set_mean = coalesce(own_player_mean, .324) # mean of lags 1-3 in the training data
  ) %>% 
  ungroup() %>% 
  select(bbrefID, name, OBP = training_set_mean) %>% 
  pivot_wider(., names_from = name, values_from = OBP)

df_test <- df_test_na_filled %>% 
  inner_join(., df_test_raw %>% select(bbrefID, cur_OBP), by = "bbrefID")

splines_recipe <- recipe(cur_OBP ~ ., data = df_training) %>% 
  update_role(bbrefID, yearID, new_role = "id") %>% 
  step_intercept() %>% 
  step_ns(c("lagged_OBP_1", "lagged_OBP_2", "lagged_OBP_3"), deg_free = 3) %>% 
  prep(., strings_as_factors = FALSE)

df_training_baked <- bake(splines_recipe, df_training) %>% 
  select(contains("_"))

df_test_baked <- bake(splines_recipe, df_test) %>% 
  select(contains("_"))

df_final_model <- lm(cur_OBP ~ ., data = df_training_baked)

df_test_fitted <- augment(df_final_model, newdata = df_test_baked)

rmse_vec(
  truth = df_test_fitted$cur_OBP,
  estimate = df_test_fitted$.fitted
)




