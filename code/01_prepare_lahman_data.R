
pacman::p_load(
  tidyverse,
  tidylog,
  Lahman,
  here,
  tsibble,
  tictoc
)

path_batting <- here("data/lahman/raw/batting.csv")
path_master <- here("data/lahman/raw/master.csv")

write_csv(Batting, path_batting)
write_csv(Master, path_master)

# https://www.smartfantasybaseball.com/tools/

df_fg <- read_csv(here("data/fangraphs/raw/obp.csv")) %>% 
  select(-birth_date) %>% 
  rename(
    "fgID" = playerid
  ) %>% 
  glimpse() 

df_fg_OBP_longer <- df_fg %>% 
  select(Name, fgID, starts_with("OBP")) %>% 
  pivot_longer(., starts_with("OBP"), names_to = "yearID", values_to = "OBP", names_prefix = "OBP_") %>% 
  mutate(
    yearID = 2000 + as.integer(yearID)
  ) %>% 
  glimpse()

df_fg_PA_longer <- df_fg %>% 
  select(Name, fgID, starts_with("PA")) %>% 
  pivot_longer(., starts_with("PA"), names_to = "yearID", values_to = "PA", names_prefix = "PA_") %>% 
  mutate(
    yearID = 2000 + as.integer(yearID)
  ) %>% 
  glimpse()

df_fg_longer <- inner_join(df_fg_OBP_longer, df_fg_PA_longer, by = c("fgID", "yearID"))

df_fg_longer %>% 
  group_by(fgID, yearID) # confirm that dataset is now at the player-year level

df_batting <- read_csv(path_batting) %>% 
  select(playerID, yearID, stint, G:GIDP) %>% 
  glimpse()

df_batting %>% 
  group_by(playerID, yearID, stint) # confirm that dataset is at the player-year-stint level 

df_master <- read_csv(path_master) %>% 
  select(playerID, bbrefID, nameFirst, nameLast) %>% 
  glimpse()

df_crosswalk <- read_csv(here("data/sfbb_player_id_crosswalk.csv")) %>% 
  select(bbrefID = "BREFID", fgID = "IDFANGRAPHS") %>% 
  glimpse()

df_batting_aggregated <- df_batting %>% 
  group_by(playerID, yearID) %>% 
  summarize(
    across(G:GIDP, ~ sum(., na.rm = TRUE))
  ) %>% 
  ungroup() %>% 
  left_join(df_master, by = "playerID") %>% 
  select(-playerID) %>% 
  glimpse()

df_batting_aggregated %>% 
  group_by(bbrefID, yearID) # confirm that dataset is now at the player-year level

df_batting_stats <- df_batting_aggregated %>% 
  mutate(
    PA = AB + BB + HBP + SF + SH,
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
    AVG = H / AB,
    SLG = (X2B + (2 * X3B) + (3 * HR)) / AB + AVG,
  ) %>% 
  glimpse()

df_tsibble <- as_tsibble(df_batting_stats, key = bbrefID, index = yearID) %>% 
  fill_gaps() %>% 
  arrange(bbrefID, yearID) %>% 
  group_by(bbrefID)

Grab_Lags <- function(tsibble_df, stat, num_lags) {
  
  tsibble_df %>% 
    transmute(
      bbrefID, 
      yearID,
      "lagged_{stat}_{num_lags}":= lag(.data[[stat]], n = num_lags)
    ) 
  
  
}

v_stats <- df_tsibble %>% 
  ungroup() %>%
  as_tibble() %>% 
  select(G:GIDP, PA, OBP, AVG, SLG) %>%
  names()

df_combos <- expand_grid(
  v_stats,
  number_of_lags = 1:5
)

# 2 minutes

tic()
df_lags <- map2_dfc(df_combos$v_stats, df_combos$number_of_lags, function(x, y) {
  print(x)
  Grab_Lags(df_tsibble, x, y)
}) 
toc()

df_lags_cleaned <- df_lags %>% 
  as_tibble() %>% 
  select(bbrefID = "bbrefID...1", yearID = "yearID...2", contains("_")) %>% 
  glimpse()

df_lags_cleaned %>% 
  filter(bbrefID == "howarry01") %>% 
  select(yearID, contains("OBP")) # sanity check

df_lags_cleaned %>% 
  group_by(bbrefID, yearID) # confirm at year-player level

v_stats_to_rename <- names(df_batting_stats) == str_to_upper(names(df_batting_stats))

df_batting_stats_renamed <- df_batting_stats %>% 
  rename_with(., ~ paste0("cur_", .), c(-yearID, - bbrefID, -nameFirst, -nameLast)) # give all stats time horizon in name

df_lahman_merged <- left_join(df_lags_cleaned, df_batting_stats_renamed, by = c("bbrefID", "yearID")) %>% 
  glimpse()

write_csv(df_lahman_merged, here("data/lahman/derived/df_batting_lag5.csv"))


df_fg_longer <- df_fg %>% 
  select(Name, birth_date, starts_with("OBP")) %>% 
  pivot_longer(., starts_with("OBP"), names_to = "yearID", values_to = "OBP", names_prefix = "OBP_") %>% 
  mutate(
    yearID = 2000 + as.integer(yearID)
  )

left_join(df_all_lags, by = c("Name", "birth_date", "yearID"))

v_21_players <- df_fg %>% 
  pull("Name")

df_all_lags %>% 
  filter(Name %in% v_21_players) %>% 
  distinct(playerID)
