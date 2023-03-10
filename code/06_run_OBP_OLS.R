
pacman::p_load(
  tidyverse,
  tidylog,
  here,
  yardstick,
  broom
)

df_training_raw <- read_csv(here("data/modeling/df_training.csv")) %>% 
  glimpse()

df_training <- df_training_raw %>% 
  filter(between(yearID, 1975, 2017)) %>% 
  select(contains("OBP")) %>% 
  print()

df_validation <- df_training_raw %>% 
  filter(yearID %in% 2018:2020) %>% 
  select(contains("OBP")) %>% 
  print()


model_obp <- lm(cur_OBP ~ ., data = df_training)

df_training_fitted <- augment(model_obp)

rmse_vec(
  truth = df_training_fitted$cur_OBP,
  estimate = df_training_fitted$.fitted
)

ggplot(df_training_fitted, aes(x = cur_OBP, y = .resid)) +
  geom_point() +
  theme_minimal()

df_validation_fitted <- augment(x = model_obp, newdata = df_validation)

rmse_vec(
  truth = df_validation_fitted$cur_OBP,
  estimate = df_validation_fitted$.fitted,
  na_rm = TRUE
)

p_resid <- ggplot(df_validation_fitted, aes(x = cur_OBP, y = .resid)) +
  geom_point() +
  theme_minimal() +
  labs(
    x = "True OBP",
    y = "OBP Residual"
  ) +
  theme(
    panel.grid.minor = element_blank()
  ) 

ggsave(plot = p_resid, filename = here("report/figures/OLS_residuals.png"))
  
