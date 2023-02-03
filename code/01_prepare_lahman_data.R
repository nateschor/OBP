
pacman::p_load(
  tidyverse,
  tidylog,
  Lahman,
  lubridate,
  here,
  slider,
  ggridges,
  tsibble,
  tictoc
)

path_batting <- here("data/lahman/raw/batting.csv")
path_master <- here("data/lahman/raw/master.csv")

write_csv(Batting, path_batting)
write_csv(Master, path_master)

df_fg <- read_csv(here("data/fangraphs/raw/obp.csv")) %>% 
  mutate(
    Name = str_replace_all(Name, " ", "_") # better to fill empty space with "_" for merging
  ) %>% 
  glimpse() 

df_batting <- read_csv(path_batting) %>% 
  select(playerID, yearID, stint, G:GIDP) %>% 
  glimpse()

df_batting %>% 
  group_by(playerID, yearID, stint) # confirm that dataset is at the player-year-stint level 

df_master <- read_csv(path_master) %>% 
  select(playerID, nameFirst, nameLast, birthYear, birthMonth, birthDay) %>% 
  glimpse()

df_batting_aggregated <- df_batting %>% 
  group_by(playerID, yearID) %>% 
  summarize(
    across(G:GIDP, ~ sum(., na.rm = TRUE))
  ) %>% 
  ungroup() %>% 
  left_join(df_master, by = "playerID") 

df_batting_aggregated %>% 
  group_by(playerID, yearID) # confirm that dataset is now at the player-year level

df_batting_stats <- df_batting_aggregated %>% 
  mutate(
    PA = AB + BB + HBP + SF + SH,
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
    AVG = H / AB,
    SLG = (X2B + (2 * X3B) + (3 * HR)) / AB + AVG,
    birth_date = paste0(birthYear, birthMonth, birthDay) %>% ymd(),
    Name = paste(nameFirst, nameLast, sep = "_")
  ) %>% 
  glimpse()

df_plotting <- df_batting_stats %>% 
  select(yearID, OBP, PA) %>% 
  na.omit() %>% 
  filter(between(OBP, .0001, .999)) %>% 
  filter(between(yearID, 2000, 2020))
    
    
ggplot(df_plotting) +
  geom_density_ridges(aes(x = OBP/PA, y = factor(yearID)))

df_fg %>% 
  left_join(., df_batting_stats) %>% 
  group_by()

df_PA <- df_fg %>% 
  select(Name:birth_date, starts_with("PA")) %>% 
  pivot_longer(., starts_with("PA"), names_to = "Year", values_to = "PA", names_prefix = "PA_")

df_OBP <- df_fg %>% 
  select(Name:birth_date, starts_with("OBP")) %>% 
  pivot_longer(., starts_with("OBP"), names_to = "Year", values_to = "OBP", names_prefix = "OBP_")

df <- inner_join(df_OBP, df_PA) %>% 
  filter(Year != 21) %>% 
  mutate(
    OBP_PA = OBP / PA
  )

ggplot(df, aes(x = OBP, y = Year)) +
  geom_density_ridges()

ggplot(df, aes(x = OBP_PA, y = Year)) +
  geom_density_ridges() +
  scale_x_continuous(limits = c(0, .005))


temp_ <- df_batting_stats %>% 
  arrange(playerID, yearID) 

df_tsibble <- as_tsibble(temp_, key = playerID, index = yearID) %>% 
  fill_gaps() %>% 
  filter(yearID >= 1995) %>% 
  arrange(playerID, yearID) %>% 
  group_by(playerID)

temp <- df_tsibble %>% 
  
  mutate(
    lagged_HR = lag(HR)
  )

Grab_Lags <- function(tsibble_df, stat, num_lags) {
  
  tsibble_df %>% 
    transmute(
      playerID, 
      yearID,
      Name,
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

names(df_tsibble)

tic()
df_lags <- map2_dfc(df_combos$v_stats, df_combos$number_of_lags, function(x, y) {
  print(x)
  Grab_Lags(df_tsibble, x, y)
}) 
toc()

df_lags_cleaned <- df_lags %>% 
  as_tibble() %>% 
  select(playerID = "playerID...1", yearID = "yearID...2", Name = "Name...3", contains("_"))

df_lags_cleaned %>% 
  filter(Name == "Barry_Bonds") %>% 
  select(yearID, OBP, contains("OBP"))


left_join(df_batting_stats, df_lags_cleaned, by = c("playerID", "yearID", "Name"))

