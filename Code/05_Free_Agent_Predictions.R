####################################################################################################################
## 05. 2025 FREE AGENCY PREDICTIONS
####################################################################################################################

library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(ggimage)

source("04_Modeling.R")
head(all_players_data)

# ----------------------------------------------------------------------------------------------------------------
# Predicting Salaries for 2025 Fee Agents
# ----------------------------------------------------------------------------------------------------------------

# Define 2025-26 NBA Salary Cap (Source: NBA.com)
cap_26 <- 154647000

# Get data for all 2025 free agents 
fas <- all_players_data %>% 
  filter(free_agent == 1 & season == 2025) %>%
  mutate(
    player_name = normalize_name(player), 
    pct_of_cap = salary / salary_cap,
    gs_pct = gs / g,
    mpg = mp / g,
    position_group = case_when(
      position %in% c("C", "PF") ~ "big",
      position %in% c("PG") ~ "guard",
      position %in% c("SG","SF") ~ "wing",
      TRUE ~ "Other"),
    position_group = factor(position_group)) %>%
  pivot_wider(
    names_from = position_group, 
    values_from = position_group, 
    values_fn = length, 
    values_fill = 0, 
    names_prefix = "pos_")

# Make predictions for 2025 free agents 
fas$predicted_pct <- predict(best_rf_model, newdata = fas)

# Convert predictions to dollars (salary AAV)
fas$predicted_salary <- fas$predicted_pct * cap_26

# For free agents who have signed as of 7/23, compare predictions to actuals
fas <- fas %>% 
  left_join(free_agents %>% 
              mutate(player_name = normalize_name(player)) %>%
              select(player_name, signed_for_aav), 
            by = "player_name") %>%
  filter(!is.na(signed_for_aav) & signed_for_aav != "") %>%
  mutate(signed_for_aav = as.numeric(signed_for_aav)) %>%
  select(season, player, age, team, position, salary, pct_of_cap, predicted_salary, predicted_pct, signed_for_aav)

# Calculate evaluation metrics
actuals <- fas$signed_for_aav
predictions <- fas$predicted_salary

# Metrics
mae <- mean(abs(actuals - predictions), na.rm = TRUE)
rmse <- sqrt(mean((actuals - predictions)^2, na.rm = TRUE))
r2 <- cor(actuals, predictions)^2

fa_eval <- tibble(
  MAE = round(mae, 2),
  RMSE = round(rmse, 2),
  R2 = round(r2, 4)
)

fa_eval
# Metrics (MAE, RMSE) are similar to the metrics on the test set 

# Plotting actuals vs predicted salaries
ggplot(fas, aes(x = predicted_salary, y = signed_for_aav)) +
  geom_point(size = 4, color = suns_purple, alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Predicted vs Actual Salary In Dollars",
    subtitle = "2025 Free Agents",
    x = "Predicted Salary (AAV)",
    y = "Actual Salary (AAV)"
  ) +
  scale_x_continuous(labels = label_dollar(scale = 1e-6, suffix = "M"), breaks = seq(0, 50000000, 10000000), limits = c(0, 50000000)) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M"), breaks = seq(0, 50000000, 10000000), limits = c(0, 50000000)) +
  theme_minimal() +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14))

# Plot distribution of prediction errors
ggplot(fas, aes(x = predicted_salary - signed_for_aav)) +
  geom_histogram(binwidth = 1000000, fill = suns_orange, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Distribution of Prediction Errors",
       x = "Prediction Error ($)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16))

# Model is over-predicting more often than under-predicting salaries for this year's free agents 


# ----------------------------------------------------------------------------------------------------------------
# Visualizations
# ----------------------------------------------------------------------------------------------------------------

# Define players to label
label_players <- tribble(
  ~player,            ~hjust, ~vjust, ~draw_order,
  "Chris Paul",         -0.15,  0.6,     2,
  "Brook Lopez",         0.1,   0.0,     2, 
  "Davion Mitchell",     -0.2,  0.6,     3, 
  "Ty Jerome",           0.8,   0.0,     4, 
  "Santi Aldama",        0.0,   1.0,     1, 
  "Fred VanVleet",       0.0,  -0.5,     4, 
  "Duncan Robinson",     0.7,   0.6,     3,
  "Myles Turner",        0.0,   1.3,     1, 
  "Caris LeVert",        -0.5,  0.0,     4,
  "Kyrie Irving",        0.5,  -1.9,     2   
)

fa_labels <- fas %>%
  filter(player %in% label_players$player) %>%
  left_join(label_players, by = "player") %>%
  arrange(draw_order)

# Add headshots 
photo_players <- tribble(
  ~player, ~headshot,             
  "Santi Aldama", "aldama.png",    
  "Fred VanVleet", "vanvleet.png",    
  "Kyrie Irving", "kyrie.png",      
  "Myles Turner", "turner.png",  
  "Duncan Robinson", "robinson.png",  
  "Davion Mitchell", "mitchell.png",  
  "Caris LeVert", "levert.png",     
  "Brook Lopez", "lopez.png",   
  "Chris Paul", "paul.png",      
  "Ty Jerome", "jerome.png",
)

fa_labels <- fa_labels %>%
  left_join(photo_players, by = "player")

unlabeled_df <- fas %>% filter(!player %in% fa_labels$player)

# Plot with player labels
ggplot() +
  # Unlabeled points with low alpha
  geom_point(data = unlabeled_df, aes(x = predicted_salary, y = signed_for_aav),
             color = suns_orange, alpha = 0.4, size = 5) +
  
  # Labeled points with full opacity
  geom_point(data = fa_labels, aes(x = predicted_salary, y = signed_for_aav),
             color = suns_orange, alpha = 1, size = 5) +
  
  # Reference line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  
  # Labels
  # geom_text(data = fa_labels,
  #           aes(x = predicted_salary, y = signed_for_aav,
  #               label = player, hjust = hjust, vjust = vjust),
  #           size = 4, color = "black") +
  
  geom_image(
    data = fa_labels,
    aes(x = predicted_salary, y = signed_for_aav, image = headshot),
    size = 0.23,
    nudge_x = fa_labels$hjust * 1e6,
    nudge_y = fa_labels$vjust * 1e6
  ) +
  
  # Labels & formatting
  labs(
    title = "Predicted vs Actual NBA Salaries",
    subtitle = "Model-Predicted vs Actual Signing Salary (AAV) for 2025 Free Agents",
    x = "Predicted Salary (AAV)",
    y = "Actual Salary (AAV)",
    caption = "Note: Additional points represent other 2025 free agents not individually labeled."
  ) +
  scale_x_continuous(
    labels = label_dollar(scale = 1e-6, suffix = "M"),
    breaks = seq(0, 55000000, 10000000),
    limits = c(0, 50000000)
  ) +
  scale_y_continuous(
    labels = label_dollar(scale = 1e-6, suffix = "M"),
    breaks = seq(0, 55000000, 10000000),
    limits = c(0, 50000000)
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text(hjust = -0.2, size = 9),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))

