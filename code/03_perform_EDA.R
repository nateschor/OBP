
pacman::p_load(
  tidyverse,
  tidylog,
  skimr,
  here,
  DataExplorer
)


df_lahman <- read_csv(here("data/lahman/derived/df_batting_lag5.csv")) %>% 
  filter(between(yearID, 1995, 2020)) %>% 
  glimpse()

skim(df_lahman)

df_lahman %>% 
  group_by(yearID) %>% 
  skim()

df_lahman %>% 
  select(contains("OBP"), yearID) %>%
  na.omit() %>% 
  DataExplorer::plot_correlation()

df_lahman %>% 
  select(contains("_")) %>% 
  na.omit() %>% 
  DataExplorer::plot_prcomp(., variance_cap = .60)
