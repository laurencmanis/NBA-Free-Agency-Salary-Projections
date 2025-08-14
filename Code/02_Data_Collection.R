####################################################################################################################
## 02. DATA COLLECTION & PREPARATION
####################################################################################################################

library(dplyr)
library(tidyverse)
library(hoopR)
library(ggplot2)
library(lubridate)
library(stringr)
library(rvest)
library(janitor)

# ----------------------------------------------------------------------------------------------------------------
# Scraping Player Stats Per 36 Minutes From basketball-reference
# ----------------------------------------------------------------------------------------------------------------
seasons <- 2015:2025

# Initialize empty list to hold data
all_stats <- list()

# Scrape player stats table for each season
for (season in seasons) {
  message("Scraping season: ", season)
  
  # Build URL for season
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", season, "_per_minute.html")
  
  # Read and parse the table
  tryCatch({
    page <- read_html(url)
    
    # Scrape the player stats table
    totals <- page %>%
      html_node("table#per_minute_stats") %>%
      html_table(fill = TRUE) %>%
      # Tidy up column names 
      clean_names() %>%
      # Ensure no duplicates
      filter(player != "Player") %>%        
      # Add a column for the season
      mutate(season = season)         
    
    all_stats[[as.character(season)]] <- totals
  }, error = function(e) {
    message("Failed to scrape season ", season, ": ", e$message)
  })
  
  Sys.sleep(5)
}

# Combine all seasons into one master dataframe
player_stats <- bind_rows(all_stats)
head(player_stats)

# ----------------------------------------------------------------------------------------------------------------
# Scraping Player Advanced Stats From basketball-reference
# ----------------------------------------------------------------------------------------------------------------

# Initialize empty list to hold data
adv_stats <- list()

# Scrape player stats table for each season
for (season in seasons) {
  message("Scraping season: ", season)
  
  # Build URL for season
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", season, "_advanced.html")
  
  # Read and parse the table
  tryCatch({
    page <- read_html(url)
    
    # Scrape the stats table
    totals <- page %>%
      html_node("table#advanced") %>%
      html_table(fill = TRUE) %>%
      # Tidy up column names 
      clean_names() %>%
      # Avoid duplicates
      filter(player != "Player") %>%         
      # Add a season column
      mutate(season = season)         
    
    adv_stats[[as.character(season)]] <- totals
  }, error = function(e) {
    message("Failed to scrape season ", season, ": ", e$message)
  })
  
  Sys.sleep(5)
}

# Combine all seasons into one master dataframe
advanced_stats <- bind_rows(adv_stats)
head(advanced_stats)

# ----------------------------------------------------------------------------------------------------------------
# Other Data Sources 
# ----------------------------------------------------------------------------------------------------------------

# Function to normalize player names 
normalize_name <- function(name) {
  name %>%
    # Remove accents
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%  
    # Replace hyphens with spaces 
    gsub("-"," ",.) %>%
    # Keep only letters and spaces
    gsub("[^[:alpha:] ]", "", .) %>%  
    # Remove trailing suffixes with space before
    gsub("\\s+(Jr|Sr|I|II|III|IV)\\s*$", "", ., ignore.case = TRUE) %>%  
    trimws() %>%
    # Make all names all lower case
    tolower() 
}

# Retrieve NBA Team Info from HoopR
teams <- espn_nba_teams() %>%
  select(team_id, team_abbrev = abbreviation, team = display_name)

# Retrieve general player information from HoopR/ESPN as of 2025
player_info <- rbind(
  nba_commonallplayers(league_id = '00', season = 2025)$CommonAllPlayers,
  nba_commonallplayers(league_id = '00', season = 2024)$CommonAllPlayers,
  nba_commonallplayers(league_id = '00', season = 2023)$CommonAllPlayers,
  nba_commonallplayers(league_id = '00', season = 2022)$CommonAllPlayers,
  nba_commonallplayers(league_id = '00', season = 2021)$CommonAllPlayers,
  nba_commonallplayers(league_id = '00', season = 2020)$CommonAllPlayers,
  nba_commonallplayers(league_id = '00', season = 2019)$CommonAllPlayers,
  nba_commonallplayers(league_id = '00', season = 2018)$CommonAllPlayers,
  nba_commonallplayers(league_id = '00', season = 2017)$CommonAllPlayers,
  nba_commonallplayers(league_id = '00', season = 2016)$CommonAllPlayers,
  nba_commonallplayers(league_id = '00', season = 2015)$CommonAllPlayers) %>%
  filter(TO_YEAR >= 2015 & TEAM_ABBREVIATION != "" & FROM_YEAR < 2025) %>%
  select(id = PERSON_ID, name = DISPLAY_FIRST_LAST, rookie_year = FROM_YEAR, team_abbrev = TEAM_ABBREVIATION, team_name = TEAM_NAME) %>%
  mutate(player_name = normalize_name(name),
         rookie_year = as.integer(rookie_year)) %>%
  distinct()

# Ensure only one row per player 
player_info %>% count(player_name, rookie_year) %>% filter(n > 1)
player_info <- distinct(player_info, id, player_name, .keep_all= TRUE)
player_info %>% count(player_name) %>% filter(n > 1)

# Read in 2025 free agents data, retrieved from Spotrac
free_agents <- read.csv('free_agents_2025.csv') %>%
  mutate(season = as.integer(2025))

# Read in salaries data, scraped from ESPN
salaries <- read.csv('salaries.csv') %>%
  select(player, season, salary) %>%
  mutate(player_name = normalize_name(player)) %>%
  distinct()

# Ensure no players have multiple salaries in a season
salaries %>% count(season, player) %>% filter(n > 1)

# Create a dataframe containing each season's salary cap (source: Spotrac)
cap <- data.frame(
  season = seq(2015, 2025, by = 1),
  salary_cap = c(63000000,84729000,89184000,91682000,98226000,98226000,101173000,
                 111290000,122418900,126529000,139182000)
)

# Read in All-NBA players (1st, 2nd, and 3rd team) since 2014 (Source: ESPN)
all_nba <- read.csv("all_nba.csv") %>%
  mutate(player_name = normalize_name(player),
         all_nba = 1)

# Indicate if a player has earned All-NBA honors in any of the last 3 seasons
all_nba <- all_nba %>%
  rowwise() %>%
  do({
    tibble(
      player_name = .$player_name,
      all_nba_season = .$season,
      season = (.$season):(.$season + 2) 
    )
  }) %>%
  ungroup() %>%
  distinct(player_name, season) %>%
  mutate(all_nba_last3 = 1)

head(all_nba)

# ----------------------------------------------------------------------------------------------------------------
# Joining all data
# ----------------------------------------------------------------------------------------------------------------

# Join the player per-36 stats and player advanced stats 
stats <- player_stats %>%
  left_join(advanced_stats, by = c("season","player","team","age","pos","g","gs","mp")) %>%
  select(season, player, age, team, position = pos, g, gs, mp, fgm = fg, fga, fg_pct = fg_percent, 
         fg3m = x3p, fg3a = x3pa,  fg3_pct = x3p_percent, fg2m = x2p, fg2a = x2pa, fg2_pct = x2p_percent, 
         efg = e_fg_percent, ftm = ft, fta, ft_pct = ft_percent, orb, drb, trb, ast, stl, blk, tov, pf, pts, 
         per, ts_pct = ts_percent, fg3ar = x3p_ar, ftr = f_tr, orb_pct = orb_percent, drb_pct = drb_percent,
         trb_pct = trb_percent, ast_pct = ast_percent, stl_pct = stl_percent, blk_pct = blk_percent, 
         tov_pct = tov_percent, usg_pct = usg_percent, ows, dws, ws, ws_48, obpm, dbpm, bpm, vorp) %>%
  mutate(player_name = normalize_name(player))

# Join free agents to stats dataframe
stats <- stats %>% 
  left_join(free_agents %>% 
              mutate(player_name = normalize_name(player)) %>%
              select(player_name, fa_type, season),
            by = c("player_name","season")) %>%
  mutate(free_agent = if_else(!is.na(fa_type), 1, 0)) %>%
  select(-fa_type)

# Ensure there are ~220 free agents this season
stats %>% filter(free_agent == 1) %>% summarise(n_distinct(player))

# Join player info to get their rookie seasons (~200 players unmapped)
stats <- stats %>% 
  left_join(player_info, by = c("player_name")) %>% 
  group_by(player_name) %>%  
  fill(rookie_year, .direction = "downup") %>%
  ungroup()

# Indicate if the player played in a prior season
stats <- stats %>%
  arrange(player_name, season) %>%
  group_by(player_name) %>%
  mutate(
    last_season = lag(season),
    played_prior = if_else(is.na(last_season), 0, 1),
    yoe = row_number() - 1
  ) %>%
  ungroup()

# Ensure only 1 row per player for players who changed teams mid-season
# Identify players who played for multiple teams in one season
stats <- stats %>%
  mutate(multi_team = if_else(team %in% c("2TM", "3TM", "4TM"), 1, 0))

multi_team_check <- stats %>%
  group_by(season, player) %>%
  summarise(multi_teams = max(multi_team))

# Join back to stats and drop un-needed rows 
stats <- stats %>%
  left_join(multi_team_check, by = c("season", "player")) %>%
  filter((multi_teams & multi_team) | (!multi_teams)) %>%
  select(-multi_team, -multi_teams)

# Double check that there is no player with multiple rows in a season
stats %>% count(season, player, team) %>% filter(n > 1)

# Double check that there are no 2 players with the same name in a season
stats %>% count(season, player) %>% filter(n > 1)

# Look at names that exist in one dataframe but not the other (ie will not join)
setdiff(unique(salaries$player_name), unique(stats$player_name))

# Appear to be mostly 2-way players, low minutes/end of rotation guys (117 total)

# Join player salaries to stats dataframe
stats <- stats %>%
  left_join(salaries, by = c("season","player_name")) %>%
  select(-player.y) %>%
  rename(player = player.x)

# Drop rows with missing salary information
stats <- stats %>%
  filter(!is.na(salary))

# Join salary cap back to stats dataframe
stats <- stats %>% left_join(cap, by="season")

# Join all-nba indicators
stats <- stats %>% 
  left_join(all_nba, by = c("player_name", "season")) %>%
  mutate(all_nba = if_else(is.na(all_nba_last3), 0, 1))

head(stats)

# ----------------------------------------------------------------------------------------------------------------
# Validation
# ----------------------------------------------------------------------------------------------------------------

# Total observations in the dataset 
nrow(stats)

# Ensure a proper number of players per season (>400, ~450)
stats %>% group_by(season) %>% summarise(players = n_distinct(player))

# Ensure 30 teams each season
stats %>% filter(!team %in% c("2TM","3TM","4TM")) %>% group_by(season) %>% summarise(teams = n_distinct(team))

# Ensure a reasonable number of players per team (8 teams with fewer than 10 players)
stats %>% filter(!team %in% c("2TM","3TM","4TM")) %>% group_by(season, team) %>% summarise(players = n_distinct(player)) %>% arrange(players)

# Ensure no duplicate players in a season
stats %>% group_by(season, player) %>% filter(n() > 1)

# Look at null values by column 
sum(is.na(stats))
colSums(is.na(stats))

# Impute nulls with 0 - nulls are mostly in 3pt pct, ft pct; all cases have 0 attempts, therefore NA percent
stats <- stats %>% mutate(across(where(is.numeric), ~replace_na(., 0)))

# Ensure no players have an unreasonable number of minutes played
stats %>% filter(mp > 4000)
summary(stats$mp) 

# Ensure no players have unreasonable number of games played
summary(stats$g) 

# Ensure no players have fg% > 100
summary(stats$fg_pct)

# Check for unrealistic ages
summary(stats$age)
summary(stats$yoe)

# Save a copy of this data before any further filtering for use later 
all_players_data <- stats

# ----------------------------------------------------------------------------------------------------------------
# Data Preparation
# ----------------------------------------------------------------------------------------------------------------

# Create a column for salary as a percent of the cap - outcome of interest/target variable
stats <- stats %>%
  mutate(pct_of_cap = salary / salary_cap)

# Remove players in their first 3 seasons (ie players on Rookie deals)
stats <- stats %>%
  filter(yoe > 3 | season - rookie_year >= 3)

# Filter out players who made less than the league minimum salary as a pct of the cap (2025 value for simplicity/consistency)
min_pct <- 1272870 / cap[cap$season == 2025, 2]

stats <- stats %>%
  filter(pct_of_cap >= min_pct)

# Drop redundant/unnecessary columns
stats <- stats %>% select(-rookie_year, -team_abbrev, -team_name, -last_season, -played_prior)

# Create some additional metrics 
stats <- stats %>%
  mutate(
    # Proportion of games started 
    gs_pct = gs / g,
    # Average minutes per game
    mpg = mp / g,
    # Grouped position
    position_group = case_when(
        position %in% c("C", "PF") ~ "big",
        position %in% c("PG") ~ "guard",
        position %in% c("SG","SF") ~ "wing",
        TRUE ~ "Other"
      ))

head(stats)
