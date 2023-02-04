
pacman::p_load(
  tidyverse,
  tidylog,
  here,
  Lahman
)

df_lahman <- read_csv(here("data/lahman/derived/df_batting_lag5.csv")) %>% 
  glimpse()

df_fg <- read_csv(here("data/fangraphs/raw/obp.csv")) %>% 
  select(-birth_date) %>% 
  rename(
    "fgID" = playerid
  ) %>% 
  glimpse() 

df_crosswalk <- read_csv(here("data/sfbb_player_id_crosswalk.csv")) %>% # "https://www.smartfantasybaseball.com/tools/"
  select(bbrefID = "BREFID", fgID = "IDFANGRAPHS", PLAYERNAME) %>% 
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

df_fg_longer <- inner_join(df_fg_OBP_longer, df_fg_PA_longer, by = c("fgID", "yearID", "Name")) %>% 
  mutate(
    fgID = as.character(fgID)
  )

df_fg_longer %>% 
  group_by(fgID, yearID) # confirm that dataset is now at the player-year level

df_fg_crosswalk <- df_fg_longer %>% 
  left_join(., df_crosswalk, by = "fgID")

df_fg_crosswalk %>% 
  select(Name, PLAYERNAME, everything()) %>% 
  distinct(Name, .keep_all = TRUE)  # confirm that for players with bbrefID they are merged correctly with fgID


df_missing_bbrefid <- df_fg_longer %>% 
  left_join(., df_crosswalk, by = "fgID") %>% 
  filter(is.na(bbrefID))


df_missing_bbrefid_filled <- df_missing_bbrefid %>% 
  select(Name, fgID, bbrefID) %>% 
  distinct(Name, .keep_all = TRUE) %>% 
  print() %>% 
  mutate(
    bbrefID = c( # manually add missing bbref IDs from bbref URL
      "marchra01",
      "deverjo01",
      "fargajo01",
      "waltodo01",
      "craigwi01",
      "ramoshe01",
      "palacjo01",
      "mazeipa01",
      "riverse01",
      "gittech01",
      "celesgi01",
      "godoyjo01",
      "hagerja01",
      "heathni01",
      "deichgr01"
    )
  ) %>% 
  print() %>% 
  select(-Name)

df_missing_merged <- df_missing_bbrefid %>% 
  select(-bbrefID) %>% 
  left_join(., df_missing_bbrefid_filled, by = "fgID")

df_fg_missing_resolved <- df_fg_crosswalk %>% 
  filter(!is.na(bbrefID)) %>% 
  bind_rows(df_missing_merged)

df_fg_missing_resolved %>% 
  group_by(Name, yearID) %>% 
  count(., sort = TRUE) %>% 
  filter(n > 1) # hanlde Ohtani showing up as pitcher and hitter

df_fg_cleaned <- df_fg_missing_resolved %>% 
  distinct(bbrefID, yearID, .keep_all = TRUE) # remove duplicated Ohtani entry

df_fg_cleaned %>% 
  group_by(fgID) # confirm there are still 572 players

df_fg_cleaned %>% 
  group_by(fgID, yearID) # confirm data at year-playerid level

df_fg_cleaned %>% 
  left_join(., df_lahman, by = c("bbrefID", "yearID")) %>% 
  transmute(
    Name,
    bbrefID,
    yearID,
    OBP,
    cur_OBP,
    delta_OBP = OBP - cur_OBP
  ) %>% 
  pull(delta_OBP) %>% 
  qplot()

df_test <- left_join(df_fg_cleaned, df_lahman, by = c("bbrefID", "yearID")) %>% 
  filter(yearID == 2021) %>% 
  select(Name, fgID, bbrefID, yearID, OBP, PA, starts_with("lagged_")) %>% 
  glimpse()

write_csv(df_test, here("data/modeling/df_test.csv"))

df_training_2020 <- df_lahman %>% 
  filter(yearID == 2020) %>% 
  filter(cur_PA >= 30) %>% 
  mutate(
    Name = paste(nameFirst, nameLast)
  ) %>% 
  select(Name, bbrefID, yearID, cur_OBP, starts_with("lagged_")) %>% 
  glimpse()

df_training_1975_2019 <- df_lahman %>% 
  filter(between(yearID, 1975, 2019)) %>% 
  filter(cur_PA >= 100) %>% 
  mutate(
    Name = paste(nameFirst, nameLast)
  ) %>% 
  select(Name, bbrefID, yearID, cur_OBP, starts_with("lagged_")) %>% 
  glimpse()

df_training <- bind_rows(
  df_training_1975_2019,
  df_training_2020
)

df_training %>% 
  group_by(yearID, bbrefID)

write_csv(df_training, here("data/modeling/df_training.csv"))
