library(jsonlite)
library(tidyverse)
library(lubridate)

# -----------------------------------------------------------------------------

# Reads the data from all Veikkausliiga matches available in the InStat API
read_instat_api_data = function(loadnew = FALSE) {
  if(loadnew) {
    print("Parsing tournament JSON")
    # Root URL of the tournament
    tournament_url = "http://mc.instatfootball.com/api/v1/seasons?tournament_id=70"
    tournament_page = fromJSON(tournament_url)
    # Get id's of the seasons
    season_ids = tournament_page$id
    # Paste to create seasonURL's
    url_start = "http://mc.instatfootball.com/api/v1/matches?tournament_id=70&season_id="
    season_urls = paste(url_start, season_ids, sep = "")
    
    # Initialize result data.frames
    
    matches = data.frame()
    teams = data.frame()
    players = data.frame()
    events = data.frame()
    player_stats = data.frame()
    team_stats = data.frame()
    tactics = data.frame()
    
    ## Parses the stats from one match in the URL x. 
    parse_instat_api_match = function(x) {
      ## Read the JSON into a list.
      doc = fromJSON(x)
      ## If list is empty and match is unavailable, return NULL.
      if (length(doc$events) == 0) {
        return(NULL);
      }
      print(paste("Parsing match", doc$match_id))
      # Get match data from the list
      match = data.frame(
        "id" = doc$match_id,
        "date" = doc$date,
        "name" = doc$name,
        "home_team_id" = doc$first_team$team_id, 
        "away_team_id" = doc$second_team$team_id, 
        "home_team_score" = doc$score_first_team, 
        "away_team_score" = doc$score_second_team,
        stringsAsFactors = F
      )
      # Parse date and season
      match = match %>% mutate(
        "season" = year(as.POSIXct(date)), 
        "date" = as.POSIXct(date)
      )
      
      # Get player data from the list
      players = data.frame(
        "id" = doc$players$player_id, 
        "name" = doc$players$display_name,
        stringsAsFactors = F
      )
      
      # Get team data from the list for both teams
      team1 = data.frame(
        "id" = doc$first_team$team_id, 
        "name" = doc$first_team$name, 
        stringsAsFactors = F
      )
      team2 = data.frame(
        "id" = doc$second_team$team_id, 
        "name" = doc$second_team$name, 
        stringsAsFactors = F
      )
      # Bind together
      teams = team1 %>% bind_rows(team2)
      
      # Get playerstats from the list and add the playerid
      player_stats = doc$players$player_id %>% 
        as.data.frame() %>% 
        bind_cols(doc$players$team_id %>% as.data.frame()) %>% 
        bind_cols(doc$players$statistics)
      
      # Add matchid and change names
      player_stats = player_stats %>% mutate("match_id" = doc$match_id)
      names(player_stats)[1:2] = c("player_id", "team_id")
      
      ## Remove possible duplicate rows
      player_stats = player_stats %>% 
        group_by(player_id, team_id) %>% 
        slice(1) %>% 
        ungroup()
      
      ## Get first team stats from the list with team_id and match_id
      team_stats = c(
        "team_id" = doc$first_team$team_id, 
        "match_id" = doc$match_id, 
        doc$first_team$statistics %>% unlist()
      )
      ## bind with second team stats
      team_stats = team_stats %>% 
        bind_rows(c(
          "team_id" = doc$second_team$team_id, 
          "match_id" = doc$match_id, 
          doc$second_team$statistics %>% unlist())
        )
      # Get events and add match_id
      events = doc$events
      events = events %>% mutate("match_id" = doc$match_id)
      
      # Bind all tactics data.frames from the list and add match_id
      tactics = do.call(rbind, doc$tactics$tactics)
      tactics = tactics %>% mutate(match_id = doc$match_id)
      
      # Reference data.frame for position codes and names
      pos_names = c(
        "GK", "LB", "LCB","CB", "RCB", "RB", "LWB", 
        "LCDM","CDM", "RCDM", "RWB", "LM", "LCM", "RCM", "RM",
        "LW","LCAM", "CAM", "RCAM", "RW", "LS" ,"ST", "RS"
      )
      
      pos_codes = c(
        31, 12, 22, 32, 42, 52, 13, 
        23, 33, 43, 53, 14, 24, 44, 54, 15,
        25, 35,45, 55, 26, 36, 46
      )
      pos_ref = data.frame(
        "pos_name" = pos_names, 
        "code" = pos_codes, 
        stringsAsFactors = F
      )
      
      ## Add position names
      tactics = tactics %>% left_join(pos_ref, by = c("position" = "code"))
      
      # Remove possible duplicate rows
      tactics = unique(tactics)
      events = unique(events)
      
      # Return parsed data.frames in a list.
      return(
        list(
          "match" = match, 
          "players" = players, 
          "teams" = teams, 
          "player_stats" = player_stats, 
          "team_stats" = team_stats, 
          "events" = events,
          "tactics" = tactics
        )
      )
    }
    
    # Loop all seasons and all matches in them
    for (i in 1:length(season_urls)) {
      # Get season page and loop through match_id's
      parsed_season = fromJSON(season_urls[i])
      
      for (j in 1:length(parsed_season$match_id)) {
        # Create match url from id and get match page.
        curr_match_id = parsed_season$match_id[j]
        match_url_start = "http://mc.instatfootball.com/api/v1/matches/"
        
        curr_match_url = paste(
          match_url_start, 
          paste(curr_match_id, "?locale=en", sep = ""), 
          sep = ""
        )
        
        curr_match_data = parse_instat_api_match(curr_match_url)
        # Check for null and if match is already parsed.
        if (!is.null(curr_match_data)) {
          if (!curr_match_data$match$id %in% matches$id) {
            # Bind to previous data
            matches = matches %>% bind_rows(curr_match_data$match)
            player_stats = player_stats %>% bind_rows(curr_match_data$player_stats)
            team_stats = team_stats %>% bind_rows(curr_match_data$team_stats)
            events = events %>% bind_rows(curr_match_data$events)
            tactics = tactics %>% bind_rows(curr_match_data$tactics)
          }
          # Add teams if not present
          for (k in nrow(curr_match_data$teams)) {
            if(!curr_match_data$teams$id[k] %in% teams$id) {
              teams = teams %>% bind_rows(curr_match_data$teams[k, ])
            }
          }
          #  Add players if not present
          for (l in 1:nrow(curr_match_data$players)) {
            if(!curr_match_data$players$id[l] %in% players$id) {
              if(curr_match_data$players$name[l] %in% players$name) {
                curr_match_data$players$name[l] = paste(curr_match_data$players$name[l], curr_match_data$players$id[l], sep = "#")
              }
              players = players %>% bind_rows(curr_match_data$players[l, ])
            }
          }
        }
      }
    }
  }
  else {
    events = read.table( "datatest/events.csv", sep = ";", header = T, encoding = "utf-8", fileEncoding = "utf-8", stringsAsFactors = F)
    matches = read.table( "datatest/matches.csv", sep = ";", header = T, encoding = "utf-8", fileEncoding = "utf-8", stringsAsFactors = F)
    matches$date = as.POSIXct(matches$date)
    player_stats = read.table( "datatest/player_stats.csv", sep = ";", header = T, encoding = "utf-8", fileEncoding = "utf-8", stringsAsFactors = F)
    team_stats = read.table("datatest/team_stats.csv", sep = ";", header = T, encoding = "utf-8", fileEncoding = "utf-8", stringsAsFactors = F)
    teams = read.table("datatest/teams.csv", sep = ";", header = T, encoding = "utf-8", fileEncoding = "utf-8", stringsAsFactors = F)
    tactics = read.table("datatest/tactics.csv", sep = ";", header = T, encoding = "utf-8", fileEncoding = "utf-8", stringsAsFactors = F)
    players = read.table("datatest/players.csv", sep = ";", header = T, encoding = "utf-8", fileEncoding = "utf-8", stringsAsFactors = F)  }
  # Return data.frames in a list
  return(
    list(
      "matches" = matches, 
      "players" = players, 
      "player_stats" = player_stats, 
      "teams" = teams, 
      "events" = events, 
      "team_stats" = team_stats, 
      "tactics" = tactics
    )
  )
}
