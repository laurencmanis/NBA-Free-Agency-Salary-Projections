####################################################################################################################
## 03. EDA
####################################################################################################################

library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(stringr)

source("02_Data_Collection.R")

# Define custom colors for plots
suns_purple <- "#5f259f"
suns_orange <- "#e56020"

# ----------------------------------------------------------------------------------------------------------------
# EDA - Target Variable, Percent of Cap
# ----------------------------------------------------------------------------------------------------------------

# Look at summary stats & distribution by season 
dist_by_season <- stats %>%
  group_by(season) %>%
  summarise(
    mean = mean(pct_of_cap, na.rm = TRUE) * 100,
    sd = sd(pct_of_cap, na.rm = TRUE) * 100,
    min = min(pct_of_cap, na.rm = TRUE) * 100,
    median = median(pct_of_cap, na.rm = TRUE) * 100,
    q3 = quantile(pct_of_cap, probs = 0.75, na.rm = TRUE) * 100,
    max = max(pct_of_cap, na.rm = TRUE) * 100,
  )

# Plot summary stats by season 
ggplot(dist_by_season, aes(x = season)) +
  geom_line(aes(y = mean, color = "Mean"), linewidth = 1) +
  geom_line(aes(y = max, color = "Max"), linewidth = 1) +
  geom_point(aes(y = mean, color = "Mean"), size = 2) +
  geom_point(aes(y = max, color = "Max"), size = 2) +
  scale_color_manual(values = c("Mean" = suns_purple, "Max" = suns_orange)) +
  labs(
    title = "Mean and Max Percent of Cap by Season",
    x = "Season",
    y = "% of Cap",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  ) +
  scale_x_continuous(breaks = dist_by_season$season) 

# The plot shows the mean and maximum player salaries as a percentage of the cap by season. 
# We observe greater volatility and lower average values prior to 2018
# From 2018 onward, values become more stable, coinciding with the introduction of the supermax contract and better alignment between cap growth and player valuation. 
# For modeling purposes, we restrict our dataset to seasons from 2018 onward to reflect the current context of NBA free agency.

stats <- stats %>%
  filter(season >= 2018)

# ----------------------------------------------------------------------------------------------------------------
# EDA - Feature Relationships 
# ----------------------------------------------------------------------------------------------------------------

# Currently have >50 features, many of which are redundant/high-correlated
# Will look to eliminate some and condense the feature set before modeling

# Group features into buckets of similar stats, where high correlations are likely to occur
colnames(stats)

# Basic player info
basic <- c("season", "age", "yoe", "g", "gs", "mp", "mpg", "gs_pct", "all_nba")

# Shooting/Scoring metrics
shooting <- c("pts", "fga", "fgm", "fg_pct", "fg3m", "fg3a", "fg3_pct", "fg2m", "fg2a", "fg2_pct", 
              "ftm", "fta", "ft_pct", "efg", "ts_pct", "fg3ar", "ftr")

# Playmaking and other defensive metrics
playmaking <- c("ast","tov","ast_pct","tov_pct","stl","blk","stl_pct","blk_pct","pf")

# Advanced stats
adv <- c("per", "usg_pct", "ows", "dws", "ws", "ws_48", "obpm", "dbpm", "bpm", "vorp")

# Rebounding and defense
rebounding <- c("orb","drb","trb","orb_pct","drb_pct","trb_pct")

# ----------------------------------------------------------------------------------------------------------------
# Define recurring functions
# ----------------------------------------------------------------------------------------------------------------

# Define a function to plot a correlation matrix
plot_matrix <- function(matrix, name) {
  corrplot(matrix, method = "color", 
           tl.col = "black", tl.srt = 45, 
           col = colorRampPalette(c(suns_orange, "white", suns_purple))(200),
           title = paste0(name, " Features Correlation Matrix"), mar=c(0,0,1,0))
}

# Define target
target <- "pct_of_cap"

# Function to make a scatter plot with best fit line for each feature against pct of cap
plot_scatter <- function(vars, data = stats) {
  scatter_plots <- list()
  
  for (var in vars) {
    # Fit a simple linear model
    model <- lm(reformulate(var, "pct_of_cap"), data = data)
    r2 <- summary(model)$r.squared
    r2_label <- paste0("R² = ", round(r2, 3))
    
    # Build plot
    p <- ggplot(data, aes_string(x = var, y = "pct_of_cap")) +
      geom_point(color = suns_purple, alpha = 0.9) +
      geom_smooth(method = "lm", se = FALSE, color = suns_orange) +
      labs(
        title = paste0("Percent of Cap vs ", str_to_title(var)),
        x = str_to_title(var),
        y = "Pct of Cap"
      ) +
      annotate("text", x = -Inf, y = Inf, label = r2_label, 
               hjust = -0.1, vjust = 1.2, size = 4.5) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
      )
    
    scatter_plots[[var]] <- p
  }
  
  return(scatter_plots)
}

# Function to compute and plot variables with strong correlations with pct of cap
show_strong_corrs <- function(vars) {
  cols <- stats %>% 
    select(all_of(vars), pct_of_cap) 
  
  correlations <- cols %>%
    cor(use = "complete.obs") %>%
    as.data.frame() %>%
    select(pct_of_cap) %>%
    rownames_to_column("variable") %>%
    filter(variable != "pct_of_cap") %>%
    arrange(desc(abs(pct_of_cap)))
  
  # Filter for moderate/strong correlations
  corrs <- correlations %>% filter(abs(pct_of_cap) >= 0.3)
  
  # Plot correlations with percent of cap
  ggplot(corrs, aes(x = reorder(variable, pct_of_cap), y = pct_of_cap)) +
    geom_col(fill = suns_purple, alpha = 0.8) +
    geom_text(aes(label = round(pct_of_cap, 2)), hjust = ifelse(corrs$pct_of_cap > 0, 1, 1), color = "white") +
    coord_flip() +
    labs(
      title = "Variables Correlated with Pct of Cap",
      x = "Variable",
      y = "Correlation"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

# ----------------------------------------------------------------------------------------------------------------
# EDA - Basic Stats
# ----------------------------------------------------------------------------------------------------------------

# Plot correlation matrix for basic stats
basic_stats <- stats %>% 
  select(all_of(basic)) 

# Compute correlation matrix
basic_matrix <- cor(basic_stats, use = "complete.obs")
plot_matrix(basic_matrix, "Basic")

# Look at variables vs target
plot_scatter(basic)

# Look at correlations between variables and target 
show_strong_corrs(basic)

# Variables to drop, based off the above 
drop <- c("gs", "mp")

# ----------------------------------------------------------------------------------------------------------------
# EDA - Shooting Stats
# ----------------------------------------------------------------------------------------------------------------

# Plot correlation matrix for shooting stats
shooting_stats <- stats %>% 
  select(all_of(shooting)) 

# Compute correlation matrix
shooting_matrix <- cor(shooting_stats, use = "complete.obs")
plot_matrix(shooting_matrix, "Shooting")

# Find pairs with high correlation
high_corr_pairs <- which(abs(shooting_matrix) >= 0.7 & abs(shooting_matrix) < 1, arr.ind = TRUE)

# Remove duplicates and print results
high_corr_df <- data.frame(
  var1 = rownames(shooting_matrix)[high_corr_pairs[, 1]],
  var2 = colnames(shooting_matrix)[high_corr_pairs[, 2]],
  corr = shooting_matrix[high_corr_pairs]
) %>%
  filter(var1 < var2) %>%  
  arrange(desc(abs(corr)))

high_corr_df

# Many high correlations, do not need to keep attempts, makes, and percentages 
# Choose which to keep based on relationships with pct of cap

# Look at variables vs target
plot_scatter(shooting)

# Look at correlations between variables and target 
show_strong_corrs(shooting)

# Variables to drop, based off the above 
drop <- c(drop, "fga", "fg_pct", "fg3a", "fg3_pct", "fg2a", "fg2_pct", "fta", "ft_pct", "efg", "ts_pct", "fg3ar", "ftr")

# ----------------------------------------------------------------------------------------------------------------
# EDA - Playmaking/Defensive Stats
# ----------------------------------------------------------------------------------------------------------------

# Plot correlation matrix for playmaking stats
play_stats <- stats %>% 
  select(all_of(playmaking)) 

# Compute correlation matrix
play_matrix <- cor(play_stats, use = "complete.obs")
plot_matrix(play_matrix, "Playmaking")

# Near-perfect correlations between ast and ast%, etc. - do not need both

# Look at variables vs target
plot_scatter(playmaking)

# Look at correlations between variables and target 
show_strong_corrs(playmaking)

# Variables to drop, based off the above - seems only asists/ast % really matter
drop <- c(drop, "ast","tov_pct","stl","blk","pf")

# ----------------------------------------------------------------------------------------------------------------
# EDA - Rebounding Stats
# ----------------------------------------------------------------------------------------------------------------

# Plot correlation matrix for rebounding stats
rebounding_stats <- stats %>% 
  select(all_of(rebounding)) 

# Compute correlation matrix
rebounding_matrix <- cor(rebounding_stats, use = "complete.obs")
plot_matrix(rebounding_matrix, "Rebounding")

# Near-perfect correlations between all rebounding metrics 

# Look at variables vs target 
plot_scatter(rebounding)

# Look at correlations between variables and target - none are strongly correlated with salary cap pct
show_strong_corrs(rebounding)

# Variables to drop, based off the above - seems only asists/ast % really matter
drop <- c(drop, "orb","drb","orb_pct","drb_pct","trb")

# ----------------------------------------------------------------------------------------------------------------
# EDA - Advanced Stats
# ----------------------------------------------------------------------------------------------------------------

# Plot correlation matrix for rebounding stats
ad_stats <- stats %>% 
  select(all_of(adv)) 

# Compute correlation matrix
ad_matrix <- cor(ad_stats, use = "complete.obs")
plot_matrix(ad_matrix, "Advanced")

# Find pairs with high correlation
high_corr_pairs <- which(abs(ad_matrix) >= 0.7 & abs(ad_matrix) < 1, arr.ind = TRUE)

# Remove duplicates and print results
high_corr_df <- data.frame(
  var1 = rownames(ad_matrix)[high_corr_pairs[, 1]],
  var2 = colnames(ad_matrix)[high_corr_pairs[, 2]],
  corr = ad_matrix[high_corr_pairs]
) %>%
  filter(var1 < var2) %>%  
  arrange(desc(abs(corr)))

high_corr_df

# Look at variables vs target 
plot_scatter(adv)

# Look at correlations between variables and target - none are strongly correlated with salary cap pct
show_strong_corrs(adv)

# Variables to drop, based off the above - seems only asists/ast % really matter
drop <- c(drop, "ows","dws","obpm","dbpm","ws","ws_48")

# Drop all specified columns 
stats <- stats %>% select(-all_of(drop))

# ----------------------------------------------------------------------------------------------------------------
# EDA - Categorical Features
# ----------------------------------------------------------------------------------------------------------------

# Plot distribution of percent of cap by position
ggplot(stats, aes(x = position, y = pct_of_cap)) +
  geom_boxplot(fill = suns_purple, alpha = 0.6, outlier.color = suns_orange) +
  labs(title = "Percent of Cap by Position",
       x = "Position",
       y = "Pct of Cap") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Wider spread for PGs, higher upside/potential for them to make a larger percent of the cap
# Similar distributions for SG, SF, PF, and C
# Significant Outliers Across All Positions
# Every position has players earning 30%+ of the cap, but PGs appear most frequently in this top tier.
# Observations are similar when looking at grouped positions (big, wing, guard)

# ----------------------------------------------------------------------------------------------------------------
# EDA - Feature Distributions
# ----------------------------------------------------------------------------------------------------------------

# Create a list of histogram plots
hist_plots <- list()

numeric_cols <- stats %>% 
  select(-player, -team, -position, -position_group, -player_name, -free_agent, -id, -name, -salary, -salary_cap) 

for (var in names(numeric_cols)) {
  # Compute mean, median, and sd for current variable
  var_data <- stats[[var]]
  mean_val <- mean(var_data, na.rm = TRUE)
  median_val <- median(var_data, na.rm = TRUE)
  sd_val <- sd(var_data, na.rm = TRUE)
  
  # Build label text
  label_text <- paste0("Mean: ", round(mean_val, 1),
                       "\nMedian: ", round(median_val, 1),
                       "\nSt Dev: ", round(sd_val, 1))
  
  # Create plot
  p <- ggplot(stats, aes_string(x = var)) +
    geom_histogram(fill = suns_purple, alpha = 0.8, color = "black", bins = 15) +
    annotate("text", x = -Inf, y = Inf, label = label_text, hjust = -0.1, vjust = 1.1, size = 4) +
    labs(
      title = paste0("Distribution of ", str_to_title(var)),
      x = str_to_title(var),
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  hist_plots[[var]] <- p
}

# Notable Distributions & Observations
hist_plots[3]     # Games played is left-skewed (Mean = 54, Median = 59)
hist_plots[12]    # AST% is right-skewed (Mean = 14.5, Median = 11.6)
hist_plots[14]    # BLK% is right-skewed (Mean = 1.9, Median = 1.4)
hist_plots[17]    # VORP is right-skewed (Mean = 0.8, Median = 0.4)
  
# ----------------------------------------------------------------------------------------------------------------
# Feature Engineering
# ----------------------------------------------------------------------------------------------------------------

# One-Hot Encode (Binarize) position group
stats <- stats %>%
  mutate(position_group = factor(position_group)) %>%
  pivot_wider(names_from = position_group, 
              values_from = position_group, 
              values_fn = length, 
              values_fill = 0, 
              names_prefix = "pos_")

# Drop unnecessary/non-numeric columns
stats <- stats %>% select(-team, -position, -player_name, -id, -name, -salary, -salary_cap, -pos_big)
stats <- stats %>% select("pct_of_cap", everything())

# Remove players who are FAs this season - do not want to train on them
stats <- stats %>%
  filter(free_agent == 0) %>%
  select(-free_agent)

head(stats)

