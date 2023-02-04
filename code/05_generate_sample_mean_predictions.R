
pacman::p_load(
  tidyverse,
  tidylog,
  here,
  yardstick
)

df_training_raw <- read_csv(here("data/modeling/df_training.csv")) %>% 
  glimpse()

df_training <- df_training_raw %>% 
  filter(yearID < 2019) %>%  
  select(bbrefID, yearID, cur_OBP) %>% 
  group_by(bbrefID) %>% 
  summarize(
    fitted_OBP = mean(cur_OBP, na.rm = TRUE)
  ) %>% 
  print()

median_obp_2014_2018 <- df_training_raw %>% 
  filter(between(yearID, 2014, 2018)) %>% 
  pull(cur_OBP) %>% 
  median(., na.rm = TRUE)

df_validation <- df_training_raw %>% 
  filter(yearID == 2019, cur_OBP > 0) %>% # remove pitchers that never had Plate appearance
  select(bbrefID, yearID, cur_OBP) %>%  
  left_join(., df_training, by = "bbrefID") %>% 
  mutate(
    fitted_OBP = coalesce(fitted_OBP, median_obp_2014_2018) # for players with no obserations before 2019, replace with league median OBP 2014-2018
  ) %>% 
  print()

rmse_vec(
  truth = df_validation$cur_OBP,
  estimate = df_validation$fitted_OBP
)




