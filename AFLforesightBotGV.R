library(rtweet)
library(fitzRoy)
library(dplyr)
library(purrr)

# Set up Twitter OAuth
client_id <- "hidden"
client_secret <- "hidden"

client <- rtweet_client(client_id, client_secret, "AFLforesight")

auth <- rtweet_oauth2(client)

# Function for parsing ladder form string
parse_form_string <- function(string) {
  split_string <- strsplit(string, "")[[1]]
  
  result <- 0
  
  for (letter in split_string) {
    if (letter == "W") {
      result <- result + 1
    } else if (letter == "D") {
      result <- result + 0.5
    }
  }
  
  return( result)
}

# Function for getting last n characters of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Function for returning matchup string
matchup_string <- function(round_number, match_id) {
  round_info <- fetch_fixture(2023, round = round_number) %>% 
    select(home.team.name, away.team.name) %>% 
    mutate(id = row_number())
  
  chosen_match_info <- round_info %>% filter(id == match_id)
  
  home_string <- chosen_match_info[1][1] %>% pull()
  
  away_string <- chosen_match_info[2][1] %>% pull()
  
  return(paste(home_string, "vs", away_string))
}

# Function for returning match hashtag
matchup_hashtag <- function(round_number, match_id) {
  round_info <- fetch_fixture(2023, round = round_number) %>% 
    select(home.team.nickname, away.team.nickname) %>% 
    mutate(id = row_number())
  
  chosen_match_info <- round_info %>% filter(id == match_id)
  
  home_string <- chosen_match_info[1][1] %>% pull() %>% tolower()
  
  if (home_string == "bulldogs") {
    home_string <- "dogs"
  } else if (home_string == "demons") {
    home_string <- "dees"
  } else if (home_string == "magpies") {
    home_string <- "pies"
  } else if (home_string == "dockers") {
    home_string <- "freo"
  } else if (home_string == "bombers") {
    home_string <- "dons"
  } else if (home_string == "kangaroos") {
    home_string <- "north"
  }
  
  away_string <- chosen_match_info[2][1] %>% pull() %>% tolower()
  
  if (away_string == "bulldogs") {
    away_string <- "dogs"
  } else if (away_string == "demons") {
    away_string <- "dees"
  } else if (away_string == "magpies") {
    away_string <- "pies"
  } else if (away_string == "dockers") {
    away_string <- "freo"
  } else if (away_string == "bombers") {
    away_string <- "dons"
  } else if (away_string == "kangaroos") {
    away_string <- "north"
  }
  
  return(paste("#AFL", home_string, away_string, sep=""))
}

# Function for getting adjusted margin
adj_margin <- function(model_result, round_number, match_id) {
  round_info <- fetch_fixture(2023, round = round_number) %>% 
    select(home.team.nickname, away.team.nickname) %>% 
    mutate(id = row_number())
  
  chosen_match_info <- round_info %>% filter(id == match_id)
  
  home_string <- chosen_match_info[1][1] %>% pull()
  
  away_string <- chosen_match_info[2][1] %>% pull()
  
  if (model_result > 0) {
    return(paste(home_string, "by", round(model_result), "points"))
  } else {
    return(paste(away_string, "by", round(model_result)*-1, "points"))
  }
}

# Function for parsing ladder form string
parse_form_string <- function(string) {
  split_string <- strsplit(string, "")[[1]]
  
  result <- 0
  
  for (letter in split_string) {
    if (letter == "W") {
      result <- result + 1
    } else if (letter == "D") {
      result <- result + 0.5
    }
  }
  
  return( result)
}

# Function for getting last n characters of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Main model function
afl_x_margin <- function(round_number, match_id) {
  round_info <- fetch_fixture(2023, round = round_number) %>% 
    select(home.team.abbreviation, away.team.abbreviation) %>% 
    mutate(id = row_number()) %>% 
    rename("home_team" = "home.team.abbreviation",
           "away_team" = "away.team.abbreviation")
  
  chosen_match_info <- round_info %>% filter(id == match_id) %>% select(home_team, away_team)
  
  # Fetch previous x matches
  previous_round_info <- data.frame()
  
  for (round in 6:round_number-1) { # starts at 6 to allow for past 5 game form
    round_data <- fetch_fixture(2023, round = round) %>% 
      select(home.team.abbreviation, away.team.abbreviation,
             home.score.totalScore, away.score.totalScore) %>% 
      rename("home_team" = "home.team.abbreviation",
             "away_team" = "away.team.abbreviation",
             "home_score" = "home.score.totalScore",
             "away_score" = "away.score.totalScore") %>% 
      mutate(round_no = round)
    
    previous_round_info <- bind_rows(previous_round_info, round_data)
  }
  
  # Get margin from scores
  previous_round_info <- previous_round_info %>%
    mutate(margin = home_score - away_score)
  
  # Fetch ladder positions
  ladder_positions <- fetch_ladder() %>%
    select(team.abbreviation, position)
  
  # Bind ladder positions with previous_round_info
  previous_round_info <- previous_round_info %>%
    left_join(ladder_positions, by = c("home_team" = "team.abbreviation")) %>%
    rename(home_position = position) %>%
    left_join(ladder_positions, by = c("away_team" = "team.abbreviation")) %>%
    rename(away_position = position)
  
  # Fetch scores of last match-up between teams
  season_results <- fetch_results(2023)
  last_season_results <- fetch_results(2022)
  for (row in 1:nrow(previous_round_info)) {
    season_matchup_info <- season_results %>%  arrange(desc(row_number())) %>%
      filter(match.homeTeam.abbr == previous_round_info[row, "home_team"] |
               match.homeTeam.abbr == previous_round_info[row, "away_team"],
             match.awayTeam.abbr == previous_round_info[row, "away_team"] |
               match.awayTeam.abbr == previous_round_info[row, "home_team"]) %>% 
      select(homeTeamScore.matchScore.totalScore,
             awayTeamScore.matchScore.totalScore, match.homeTeam.abbr)
    
    last_season_matchup_info <- last_season_results %>% arrange(desc(row_number())) %>%
      filter(match.homeTeam.abbr == previous_round_info[row, "home_team"] |
               match.homeTeam.abbr == previous_round_info[row, "away_team"],
             match.awayTeam.abbr == previous_round_info[row, "away_team"] |
               match.awayTeam.abbr == previous_round_info[row, "home_team"]) %>% 
      select(homeTeamScore.matchScore.totalScore,
             awayTeamScore.matchScore.totalScore, match.homeTeam.abbr)
    
    matchup_info <- bind_rows(season_matchup_info, last_season_matchup_info)
    
    if (previous_round_info[row, "home_team"] == matchup_info[2, "match.homeTeam.abbr"]) {
      previous_round_info[row, "previous_match_home_score"] <- matchup_info[2, "homeTeamScore.matchScore.totalScore"]
      previous_round_info[row, "previous_match_away_score"] <- matchup_info[2, "awayTeamScore.matchScore.totalScore"]
    } else {
      previous_round_info[row, "previous_match_home_score"] <- matchup_info[2, "awayTeamScore.matchScore.totalScore"]
      previous_round_info[row, "previous_match_away_score"] <- matchup_info[2, "homeTeamScore.matchScore.totalScore"]
    }
  }
  
  # Get margin from total scores
  previous_round_info <- previous_round_info %>%
    mutate(last_match_margin = previous_match_home_score - previous_match_away_score)
  
  # Select only necessary columns
  previous_round_info <- previous_round_info %>% 
    select(home_team, away_team, margin, home_position, away_position, last_match_margin, round_no)
  
  # Get wins in last 5 games for team being predicted
  for (row in 1:nrow(previous_round_info)) {
    previous_round_info[row, "predicted_form"] <- fetch_ladder(2023, round_number = previous_round_info[row, "round_no"]) %>%
      filter(team.abbreviation == previous_round_info[row, "home_team"]) %>% 
      select(form)
  }
  
  for (row in 1:nrow(previous_round_info)) {
    previous_round_info[row, "predicted_form"] = previous_round_info[row, "predicted_form"] %>% 
      substrRight(5)
  }
  
  previous_round_info <- previous_round_info %>%
    mutate(wins_in_last_5 = map_chr(predicted_form, parse_form_string))
  
  previous_round_info <- previous_round_info %>%
    select(-predicted_form)
  
  # Get wins in last 5 games for opponent team
  for (row in 1:nrow(previous_round_info)) {
    previous_round_info[row, "opponent_form"] <- fetch_ladder(2023, round_number = previous_round_info[row, "round_no"]) %>%
      filter(team.abbreviation == previous_round_info[row, "away_team"]) %>% 
      select(form)
  }
  
  for (row in 1:nrow(previous_round_info)) {
    previous_round_info[row, "opponent_form"] = previous_round_info[row, "opponent_form"] %>% 
      substrRight(5)
  }
  
  previous_round_info <- previous_round_info %>%
    mutate(opponent_wins_in_last_5 = map_chr(opponent_form, parse_form_string))
  
  previous_round_info <- previous_round_info %>%
    select(-opponent_form)
  
  previous_round_info$wins_in_last_5 <- 
    as.numeric(previous_round_info$wins_in_last_5)
  previous_round_info$opponent_wins_in_last_5 <- 
    as.numeric(previous_round_info$opponent_wins_in_last_5)
  
  # Get all player stats for the season up to round 16
  season_stats <- data.frame()
  for (round in 1:round_number-1) {
    round_stats <- fetch_player_stats(season = 2023, round_number = round)
    season_stats <- bind_rows(season_stats, round_stats)
  }
  
  # Get sum of ranking points of lineup from each home player's last 5 games
  for (row in 1:nrow(previous_round_info)) {
    home_lineup <- fetch_lineup(2023, previous_round_info[row, "round_no"]) %>%
      filter(teamAbbr == previous_round_info[row, "home_team"])
    
    home_rating <- 0
    
    for (prow in 1:nrow(home_lineup)) { # iterate over each player
      if (home_lineup[prow, "position"] %>% pull() != "EMERG") {
        player <- home_lineup[prow, ]
        
        pstats <- season_stats %>% 
          filter(player.player.player.playerId ==
                   player[1, "player.playerId"] %>% pull()) %>% 
          tail(5)
        
        home_rating <- home_rating + sum(pstats$dreamTeamPoints)
      }
    }
    
    previous_round_info[row, "home_rating"] <- home_rating
  }
  
  # Get sum of ranking points of lineup from each away player's last 5 games
  for (row in 1:nrow(previous_round_info)) {
    away_lineup <- fetch_lineup(2023, previous_round_info[row, "round_no"]) %>%
      filter(teamAbbr == previous_round_info[row, "away_team"])
    
    away_rating <- 0
    
    for (prow in 1:nrow(away_lineup)) { # iterate over each player
      if (away_lineup[prow, "position"] %>% pull() != "EMERG") {
        player <- away_lineup[prow, ]
        
        pstats <- season_stats %>% 
          filter(player.player.player.playerId ==
                   player[1, "player.playerId"] %>% pull()) %>% 
          tail(5)
        
        away_rating <- away_rating + sum(pstats$dreamTeamPoints)
      }
    }
    
    previous_round_info[row, "away_rating"] <- away_rating
  }
  
  model <- lm(margin ~ home_position + away_position + last_match_margin + 
                wins_in_last_5 + opponent_wins_in_last_5 + home_rating + 
                away_rating, data = previous_round_info)
  
  # Fetch ladder positions
  ladder_positions <- fetch_ladder() %>%
    select(team.abbreviation, position)
  
  # Bind ladder positions with chosen_match_info
  chosen_match_info <- chosen_match_info %>%
    left_join(ladder_positions, by = c("home_team" = "team.abbreviation")) %>%
    rename(home_position = position) %>%
    left_join(ladder_positions, by = c("away_team" = "team.abbreviation")) %>%
    rename(away_position = position)
  
  # Fetch scores of last match-up between teams
  season_matchup_info <- fetch_results() %>%  arrange(desc(row_number())) %>%
    filter(match.homeTeam.abbr == chosen_match_info[1, "home_team"] %>% pull |
             match.homeTeam.abbr == chosen_match_info[1, "away_team"] %>% pull,
           match.awayTeam.abbr == chosen_match_info[1, "away_team"] %>% pull |
             match.awayTeam.abbr == chosen_match_info[1, "home_team"] %>% pull) %>% 
    select(homeTeamScore.matchScore.totalScore,
           awayTeamScore.matchScore.totalScore, match.homeTeam.abbr)
  
  last_season_matchup_info <- fetch_results(2022) %>% arrange(desc(row_number())) %>%
    filter(match.homeTeam.abbr == chosen_match_info[1, "home_team"] %>% pull |
             match.homeTeam.abbr == chosen_match_info[1, "away_team"] %>% pull,
           match.awayTeam.abbr == chosen_match_info[1, "away_team"] %>% pull |
             match.awayTeam.abbr == chosen_match_info[1, "home_team"] %>% pull) %>% 
    select(homeTeamScore.matchScore.totalScore,
           awayTeamScore.matchScore.totalScore, match.homeTeam.abbr)
  
  matchup_info <- bind_rows(season_matchup_info, last_season_matchup_info)
  
  if (chosen_match_info[1, "home_team"] == matchup_info[1, "match.homeTeam.abbr"]) {
    chosen_match_info[1, "previous_match_home_score"] <- matchup_info[1, "homeTeamScore.matchScore.totalScore"]
    chosen_match_info[1, "previous_match_away_score"] <- matchup_info[1, "awayTeamScore.matchScore.totalScore"]
  } else {
    chosen_match_info[1, "previous_match_home_score"] <- matchup_info[1, "awayTeamScore.matchScore.totalScore"]
    chosen_match_info[1, "previous_match_away_score"] <- matchup_info[1, "homeTeamScore.matchScore.totalScore"]
  }
  
  # Get margin from total scores
  chosen_match_info <- chosen_match_info %>%
    mutate(last_match_margin = previous_match_home_score - previous_match_away_score)
  
  # Get wins in last 5 games for home team
  chosen_match_info[1, "home_form"] <- fetch_ladder(2023) %>%
    filter(team.abbreviation == chosen_match_info[1, "home_team"] %>% pull()) %>% 
    select(form)
  
  chosen_match_info[1, "home_form"] = chosen_match_info[1, "home_form"] %>% 
    substrRight(5)
  
  chosen_match_info <- chosen_match_info %>%
    mutate(wins_in_last_5 = map_chr(home_form, parse_form_string))
  
  # Get wins in last 5 games for away team
  chosen_match_info[1, "away_form"] <- fetch_ladder(2023) %>%
    filter(team.abbreviation == chosen_match_info[1, "away_team"] %>% pull()) %>% 
    select(form)
  
  chosen_match_info[1, "away_form"] = chosen_match_info[1, "away_form"] %>% 
    substrRight(5)
  
  chosen_match_info <- chosen_match_info %>%
    mutate(opponent_wins_in_last_5 = map_chr(away_form, parse_form_string))
  
  chosen_match_info <- chosen_match_info %>%
    select(-previous_match_home_score, -previous_match_away_score, 
           -home_form, -away_form)
  
  chosen_match_info$wins_in_last_5 <- 
    as.numeric(chosen_match_info$wins_in_last_5)
  chosen_match_info$opponent_wins_in_last_5 <- 
    as.numeric(chosen_match_info$opponent_wins_in_last_5)
  
  # Get all player stats for the season up to round 16
  season_stats <- data.frame()
  for (round in 1:round_number-1) {
    round_stats <- fetch_player_stats(season = 2023, round_number = round)
    season_stats <- bind_rows(season_stats, round_stats)
  }
  
  # Get sum of ranking points of lineup from each home player's last 5 games
  home_lineup <- fetch_lineup(2023, 16) %>%
    filter(teamAbbr == chosen_match_info[1, "home_team"] %>% pull())
  
  home_rating <- 0
  
  for (prow in 1:nrow(home_lineup)) { # iterate over each player
    if (home_lineup[prow, "position"] %>% pull() != "EMERG") {
      player <- home_lineup[prow, ]
      
      pstats <- season_stats %>% 
        filter(player.player.player.playerId ==
                 player[1, "player.playerId"] %>% pull()) %>% 
        tail(5)
      
      home_rating <- home_rating + sum(pstats$dreamTeamPoints)
    }
  }
  
  chosen_match_info[1, "home_rating"] <- home_rating
  
  # Get sum of ranking points of lineup from each away player's last 5 games
  
  away_lineup <- fetch_lineup(2023, 16) %>%
    filter(teamAbbr == chosen_match_info[1, "away_team"] %>% pull())
  
  away_rating <- 0
  
  for (prow in 1:nrow(away_lineup)) { # iterate over each player
    if (away_lineup[prow, "position"] %>% pull() != "EMERG") {
      player <- away_lineup[prow, ]
      
      pstats <- season_stats %>% 
        filter(player.player.player.playerId ==
                 player[1, "player.playerId"] %>% pull()) %>% 
        tail(5)
      
      away_rating <- away_rating + sum(pstats$dreamTeamPoints)
    }
  }
  
  chosen_match_info[1, "away_rating"] <- away_rating
  
  model_result <- predict(model, newdata = chosen_match_info)
  
  return(model_result)
}

# Modify the function to include tweeting
afl_x_margin_tweet <- function(round_number, match_id) {
  # Existing code for margin prediction
  
  # Get the result of the function
  model_result <- afl_x_margin(round_number, match_id)
  
  # Get adjusted margin
  predicted_result <- adj_margin(model_result, round_number, match_id)
  
  # Get the match up string
  match_string <- matchup_string(round_number, match_id)
  
  # Get the match up hash tag
  match_hashtag <- matchup_hashtag(round_number, match_id)
  
  # Compose the tweet text
  tweet_text <- paste0("Round ", round_number, ", ", match_string,
                       ": Predicted margin is ", predicted_result,
                       ". #AFL", " ", match_hashtag)
  
  # Post the tweet
  tweet_post(tweet_text, token=auth)
}

# Call the function
for (match_id in 1:9) {
  afl_x_margin_tweet(21, match_id)
}