
pacman::p_load(
  tidyverse,
  tidylog,
  here,
  yardstick
)

df_training_raw <- read_csv(here("data/modeling/df_training.csv")) %>% 
  glimpse()

df_training <- df_training_raw %>% 
  filter(between(yearID, 1975, 2017)) %>%  
  select(bbrefID, yearID, cur_OBP) %>% 
  group_by(bbrefID) %>% 
  summarize(
    fitted_OBP = mean(cur_OBP, na.rm = TRUE)
  ) %>% 
  print()

median_obp_2013_2017 <- df_training_raw %>% 
  filter(between(yearID, 2013, 2017)) %>% 
  pull(cur_OBP) %>% 
  median(., na.rm = TRUE)

df_validation <- df_training_raw %>% 
  filter(yearID %in% 2018:2020) %>% 
  select(bbrefID, yearID, cur_OBP) %>%  
  left_join(., df_training, by = "bbrefID") %>% 
  mutate(
    fitted_OBP = coalesce(fitted_OBP, median_obp_2013_2017) # for players with no obserations before 2018, replace with league median OBP 2013-2017
  ) %>% 
  print()

rmse_vec(
  truth = df_validation$cur_OBP,
  estimate = df_validation$fitted_OBP
)




