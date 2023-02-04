
pacman::p_load(
  tidyverse,
  tidylog,
  here,
  yardstick,
  glmnetUtils
)

df_training_raw <- read_csv(here("data/modeling/df_training.csv")) %>% 
  glimpse()

df_training <- df_training_raw %>% 
  filter(yearID <= 2017) %>% 
  select(contains("_")) %>% 
  glimpse()
  
df_validation <- df_training_raw %>% 
  filter(yearID %in% 2018:2020) %>% 
  select(contains("_")) %>% 
  glimpse()

model_lasso <- glmnet(cur_OBP ~ ., data = df_training, alpha = 1)  

mx_coefs <- coef(model_lasso)

df_vars_selected_for_each_lambda <- tidy(mx_coefs) %>% 
  rename(
    variable = row,
    lambda_index = column,
    coefficient = value
  ) %>% 
  filter(variable != "(Intercept)") %>% 
  select(variable, lambda_index) %>% 
  group_by(lambda_index) %>% 
  mutate(
    selected_vars = list(variable)
  ) %>% 
  distinct(lambda_index, .keep_all = TRUE) %>% 
  select(-variable)

