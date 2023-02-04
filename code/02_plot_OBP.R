
pacman::p_load(
  tidyverse,
  tidylog,
  here,
  ggridges
)

df_lahman <- read_csv(here("data/lahman/derived/df_batting_lag5.csv")) %>% 
  glimpse()


# Lag Plots ---------------------------------------------------------------

df_lag_plot <- df_lahman %>% 
  filter(yearID %in% 2015:2020) %>% 
  filter(
    if_all(contains("OBP"), ~ between(., .001, .999))
  ) %>% 
  select(bbrefID, yearID, contains("OBP"))

Plot_Lags <- function(plotting_data, num_lag, save = FALSE) {
  
  x_axis <- paste0("lagged_OBP_", num_lag)
  x_axis_title <- paste0(num_lag, "-year Lag")
  
  p <- ggplot(data = df_lag_plot, aes_string(x = x_axis, y = "cur_OBP")) +
    geom_point(alpha = .3, size = 1.5) +
    geom_smooth(se = FALSE, color = "#4477AA", size = 1) +
    geom_abline(slope = 1, linetype = "dashed", color = "#CC6677", size = 1) +
    scale_x_continuous(limits = c(0, .7), expand = expansion(0, 0)) +
    scale_y_continuous(limits = c(0, .7), expand = expansion(0, 0)) +
    labs(
      x = x_axis_title,
      y = "Current Season"
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


# 2019 vs. 2020 to see if relationship persists ---------------------------

df_2020 <- df_lahman %>% 
  filter(yearID == 2020) %>% 
  select(bbrefID, yearID, cur_OBP, lagged_OBP_1) %>% 
  filter(
    if_all(contains("OBP"), ~ between(., .001, .999))
  )

p <- ggplot(data = df_2020, aes(x = lagged_OBP_1, y = cur_OBP)) +
  geom_point(alpha = .3, size = 1.5) +
  geom_smooth(se = FALSE, color = "#4477AA", size = 1) +
  geom_abline(slope = 1, linetype = "dashed", color = "#CC6677", size = 1) +
  scale_x_continuous(limits = c(0, .7), expand = expansion(0, 0)) +
  scale_y_continuous(limits = c(0, .7), expand = expansion(0, 0)) +
  labs(
    x = "2019 OBP",
    y = "2020 OBP"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(plot = p, filename = here("report/figures/2019_2020_lag.png"))

# Ridge Plots -------------------------------------------------------------

median_2019 <- df_ridge_plot %>% 
  filter(yearID == 2019) %>% 
  pull(cur_OBP) %>% 
  median(., na.rm = TRUE)

Plot_Ridges <- function(start_year, save = FALSE) {
  
  df_ridge_plot <- df_lahman %>% 
    filter(yearID %in% start_year:2020) %>% 
    filter(between(cur_OBP, .001, .999)) %>% 
    select(bbrefID, yearID, cur_OBP)
  
  p <- ggplot(df_ridge_plot, aes(x = cur_OBP, y = factor(yearID))) +
    geom_density_ridges(fill = "#4477AA") +
    scale_x_continuous(expand = expansion(0, 0)) +
    scale_y_discrete(expand = expansion(0, 0)) +
    labs(
      x = "OBP",
      y = "Season"
    ) +
    geom_vline(xintercept = median_2019, linetype = "dashed", color = "#CC6677", size = 1) +
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


