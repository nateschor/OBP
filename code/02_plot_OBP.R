
pacman::p_load(
  tidyverse,
  tidylog,
  here,
  ggridges,
  ggthemes
)

df_lahman <- read_csv(here("data/lahman/derived/df_batting_lag5.csv")) %>% 
  glimpse()


# Quartiles over time -----------------------------------------------------

df_quartiles_pre_2020 <- df_lahman %>% 
  filter(yearID <= 2019) %>% 
  filter(cur_PA > 100) %>% 
  group_by(yearID) %>% 
  summarize(
    `25th` = quantile(cur_OBP, probs = .25, na.rm = TRUE),
    `50th` = quantile(cur_OBP, probs = .5, na.rm = TRUE),
    `75th` = quantile(cur_OBP, probs = .75, na.rm = TRUE),
  ) %>%  
  pivot_longer(., cols = -yearID, names_to = "Quartile", values_to = "OBP") %>% 
  mutate(
    Quartile = factor(Quartile, levels = c("75th", "50th", "25th"))
  )

df_quartiles_2020 <- df_lahman %>% 
  filter(yearID == 2020) %>% 
  filter(cur_PA > 30) %>% 
  group_by(yearID) %>% 
  summarize(
    `25th` = quantile(cur_OBP, probs = .25, na.rm = TRUE),
    `50th` = quantile(cur_OBP, probs = .5, na.rm = TRUE),
    `75th` = quantile(cur_OBP, probs = .75, na.rm = TRUE),
  ) %>%  
  pivot_longer(., cols = -yearID, names_to = "Quartile", values_to = "OBP") %>% 
  mutate(
    Quartile = factor(Quartile, levels = c("75th", "50th", "25th"))
  )

df_quartiles <- bind_rows(
  df_quartiles_pre_2020,
  df_quartiles_2020
)

p_quartiles <- ggplot(df_quartiles, aes(x = yearID, y = OBP, color = Quartile)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(1870, 2020, 10)) +
  scale_color_ptol() +
  geom_vline(xintercept = 1970, linetype = "dashed") +
  theme_minimal() +
  labs(
    x = "Season"
  ) +
  theme(
    panel.grid = element_blank()
  ) 

ggsave(plot = p_quartiles, filename = here("report/figures/quartiles_ts.png"))

# Lag Plots ---------------------------------------------------------------

df_lag_plot <- df_lahman %>% 
  filter(yearID %in% 2015:2020) %>% 
  filter(
    if_all(contains("OBP"), ~ between(., .2, .610))
  ) %>% 
  select(bbrefID, yearID, contains("OBP"))

Plot_Lags <- function(plotting_data, num_lag, save = FALSE) {
  
  x_axis <- paste0("lagged_OBP_", num_lag)
  x_axis_title <- paste0(num_lag, "-year Lag of OBP")
  
  p <- ggplot(data = df_lag_plot, aes_string(x = x_axis, y = "cur_OBP")) +
    geom_point(alpha = .3, size = 1.5) +
    geom_smooth(se = FALSE, color = "#4477AA", size = 1) +
    geom_abline(slope = 1, linetype = "dashed", color = "#CC6677", size = 1) +
    scale_x_continuous(limits = c(0, .7), expand = expansion(0, 0)) +
    scale_y_continuous(limits = c(0, .7), expand = expansion(0, 0)) +
    labs(
      x = x_axis_title,
      y = "Current Season OBP"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank()
    )
  
  if (save) {
    
    file_name <- paste0("lag_", num_lag, ".png")
    path_plot <- here("report/figures/", file_name)
    
    ggsave(plot = p, filename = path_plot)
    
  }
  
}

v_lags <- 1:5

walk(v_lags, ~ Plot_Lags(df_lag_plot, ., save = TRUE))


# Ridge Plots -------------------------------------------------------------

Plot_Ridges <- function(start_year, save = FALSE) {
  
  df_ridge_plot <- df_lahman %>% 
    filter(yearID %in% start_year:2020) %>% 
    filter(between(cur_OBP, .25, .610)) %>% 
    select(bbrefID, yearID, cur_OBP)
  
  p <- ggplot(df_ridge_plot, aes(x = cur_OBP, y = factor(yearID))) +
    geom_density_ridges(fill = "#4477AA") +
    scale_x_continuous(expand = expansion(0, 0)) +
    scale_y_discrete(expand = expansion(0, 0)) +
    labs(
      x = "OBP",
      y = "Season"
    ) +
    theme_minimal()
  
  if(save == TRUE) {
    
    file_name <- paste0("ridge_", start_year, "_2020.png")
    path_plot <- here("report/figures/", file_name)
    
    ggsave(plot = p, filename = path_plot)
    
  }
  
}

v_ridge_start_years <- c(2000, 2013, 2016)

walk(v_ridge_start_years, ~ Plot_Ridges(., save = TRUE))

df_ridge_plot <- df_lahman %>% 
  filter(yearID %in% 2016:2020) %>% 
  filter(between(cur_OBP, .001, .999)) %>% 
  transmute(
    bbrefID, 
    yearID, 
    OBP_per_PA = cur_OBP / cur_PA
)

p <- ggplot(df_ridge_plot, aes(x = OBP_per_PA, y = factor(yearID))) +
  geom_density_ridges(fill = "#4477AA") +
  scale_x_continuous(limits = c(0, .01), expand = expansion(0, .001)) +
  scale_y_discrete(expand = expansion(0, 0)) +
  labs(
    x = "OBP per PA",
    y = "Season"
  ) +
  theme_minimal()

ggsave(plot = p, filename = here("report/figures/ridge_2016_2020_OBP_per_PA.png"))
