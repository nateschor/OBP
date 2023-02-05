
pacman::p_load(
  tidyverse,
  tidylog,
  skimr,
  here,
  DataExplorer
)


df_lahman <- read_csv(here("data/lahman/derived/df_batting_lag5.csv")) %>% 
  filter(between(yearID, 1970, 2020)) %>% 
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

df_fg <- read_csv("data/fangraphs/raw/obp.csv") %>% 
  mutate(
    playerid = as.character(playerid)
  ) %>% 
  select(-OBP_21, -PA_21)


df_fg %>% 
  map_dbl(~ sum(is.na(.)))

skimr::skim(df)

df_fg %>% 
  select(playerid, contains("OBP")) %>% 
  pivot_longer(-playerid) %>%
  group_by(playerid) %>% 
  summarize(
    num_missing = sum(is.na(value))
  ) %>% 
  filter(num_missing >1)

df_fg %>% 
  select(playerid, num_range("PA_", 16:20)) %>% 
  filter(
    PA_16 >= 100,
    PA_17 >= 100,
    PA_18 >= 100,
    PA_19 >= 100,
    PA_20 >= 30,
  )
