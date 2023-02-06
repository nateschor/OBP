
pacman::p_load(
  tidyverse,
  tidylog,
  Lahman,
  here,
  tsibble,
  tictoc
)

path_batting <- here("data/lahman/raw/batting.csv")
path_people <- here("data/lahman/raw/people.csv")

write_csv(Lahman::Batting, path_batting)
write_csv(Lahman::People, path_people)

# FanGraphs ---------------------------------------------------------------

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


# Lahman ------------------------------------------------------------------

df_batting <- Lahman::Batting %>% 
  select(playerID, yearID, stint, G:GIDP) %>% 
  glimpse()

df_batting %>% 
  group_by(playerID, yearID, stint) # confirm that dataset is at the player-year-stint level 

df_people <- Lahman::People %>% 
  select(playerID, bbrefID, nameFirst, nameLast) %>% 
  glimpse()

df_batting_aggregated <- df_batting %>% 
  group_by(playerID, yearID) %>% 
  summarize(
    across(G:GIDP, ~ sum(., na.rm = TRUE))
  ) %>% 
  ungroup() %>% 
  left_join(df_people, by = "playerID") %>% 
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

df_tsibble <- as_tsibble(df_batting_stats, key = bbrefID, index = yearID) %>% # https://tsibble.tidyverts.org/
  fill_gaps() %>% # make missing seasons explicit instead of implicit so that previous row = last played season
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

# 2.5 minutes

tic()
df_lags <- map2_dfc(df_combos$v_stats, df_combos$number_of_lags, function(x, y) {
  print(x) # Make sure code is running
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

v_stats_to_rename <- names(df_batting_stats) == str_to_upper(names(df_batting_stats)) # stats are in all caps, other variables are not all caps

df_batting_stats_renamed <- df_batting_stats %>% 
  rename_with(., ~ paste0("cur_", .), c(-yearID, - bbrefID, -nameFirst, -nameLast)) # give all stats time horizon in name, easier to remove for testing

df_lahman_merged <- left_join(df_lags_cleaned, df_batting_stats_renamed, by = c("bbrefID", "yearID")) %>% 
  glimpse()

write_csv(df_lahman_merged, here("data/lahman/derived/df_batting_lag5.csv"))
