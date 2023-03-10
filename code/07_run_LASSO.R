
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
  select(-cur_BB) %>% 
  glimpse()
  
df_validation <- df_training_raw %>% 
  filter(yearID %in% 2018:2020) %>% 
  select(contains("_")) %>% 
  select(-cur_BB) %>% 
  glimpse()

model_lasso <- glmnet(cur_OBP ~ ., data = df_training, alpha = 1) # alpha == 1 is LASSO, 0 is Ridge https://cran.r-project.org/web/packages/glmnetUtils/vignettes/intro.html 

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
  select(-variable) # most important vars look to be OBP lags 1-3 and BB lags 1-3





