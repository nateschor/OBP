
pacman::p_load(
  tidyverse,
  tidylog,
  Lahman,
  here
)

path_batting <- here("data/lahman/raw/batting.csv")
path_master <- here("data/lahman/raw/master.csv")

write_csv(Batting, path_batting)
write_csv(Master, path_master)