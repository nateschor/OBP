
pacman::p_load(
  tidyverse,
  tidylog,
  here,
  ggridges
)

df_lahman <- read_csv("data/lahman/derived/df_batting_lag5.csv") %>% 
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


