library(shiny)
library(shinythemes)
library(fmsb)
library(DT)
library(magick)
source("read_instat_api_data.R")
data_list = read_instat_api_data(loadnew = F)
matches = data_list$matches
players = data_list$players
teams = data_list$teams
events = data_list$events
player_stats = data_list$player_stats
team_stats = data_list$team_stats
tactics = data_list$tactics

## Contents of helper_functions.R start

# Team events for match
get_team_events_match = function( team_ids, match_ids) {
    events %>% filter(
        team_id %in% team_ids, 
        match_id %in% match_ids
    )
}

# Team events for season
get_team_events_season = function(team_ids, seasons) {
    get_team_events_match(
        team_ids,
        matches %>% 
            filter(season %in% seasons) %>%
            select(id) %>% 
            unlist()
    )
}

# Player events for match
get_player_events_match = function(player_ids, match_ids) {
    events %>% 
        filter(
            player_id %in% player_ids, 
            match_id %in% match_ids
        )
}

# Player events for season
get_player_events_season = function(player_ids, seasons) {
    get_player_events_match(
        player_ids,
        matches %>%
            filter(season %in% seasons) %>% 
            select(id) %>% 
            unlist()
    )
}

# Player stats for match
get_player_stats_match = function(player_ids, match_ids) {
    player_stats %>% 
        filter(
            match_id %in% match_ids,
            player_id %in% player_ids
        )
}

# Player stats for season
get_player_stats_season = function(player_ids, seasons) {
    get_player_stats_match(
        player_ids,
        matches %>%
            filter(season %in% seasons) %>% 
            select(id) %>% 
            unlist()
    )
}

# Get summary of player stats for a season
get_player_stat_summary = function(player_ids, seasons) {
    stats = get_player_stats_season(player_ids, seasons) %>%
        filter(mof > 0) %>% 
        summarise(
            isi = mean(isi), 
            mof = sum(mof), 
            n = n(),
            g = sum(g),
            a = sum(a),
            s = sum(s),
            st = sum(st),
            f = sum(f),
            fop = sum(fop),
            offs = sum(offs),
            pa = sum(pa),
            pap = mean(pap),
            c = sum(c),
            cw = sum(cw),
            cwp = mean(cwp),
            lb = sum(lb),
            t = sum(t),
            spda = mean(spda),
            spdm = mean(spdm),
            spdmm = max(spdm)
        ) %>% 
        mutate(
            `g/90` = g * 90 / mof,
            `a/90` = a * 90 / mof,
            `s/90` = s * 90 / mof,
            `st/90` = st * 90 / mof,
            `f/90` = f * 90 / mof,
            `fop/90` = fop * 90 / mof,
            `offs/90` = offs * 90 / mof,
            `pa/90` = pa * 90 / mof,
            `c/90` = c * 90 / mof,
            `cw/90` = cw * 90 / mof,
            `lb/90` = lb * 90 / mof,
            `t/90` = t * 90 / mof
        )
    
    if(stats[2] == 0) {
      return(NA)
    }
    else{
      return(stats)
    }
}

## Get player id with name
get_player_id = function(player_name) {
    players %>% 
        filter(name %in% player_name) %>% 
        select(id) %>% 
        unlist()
}

## Get team id with name
get_team_id = function(team_name) {
    teams %>% 
        filter(name %in% team_name) %>% 
        select(id) %>% 
        unlist()
}

## All matches for season
get_season_matches = function(seasons) {
    matches %>% filter(season %in% seasons)
}

## Team matches for season
get_team_matches = function(team_ids, seasons = 2016:2019) {
    get_season_matches(seasons) %>% 
        filter(home_team_id %in% team_ids | away_team_id %in% team_ids)
}

## Team points from match
get_points = function(team_id, match_id) {
    match = matches %>% filter(id == match_id)
    home = match$home_team_id == team_id
    result = 0
    if (match$home_team_score == match$away_team_score) {
        result = 1
    } else if (home) {
        if(match$home_team_score > match$away_team_score) {
            result = 3
        }
    } else {
        if(match$home_team_score < match$away_team_score) {
            result = 3
        }
    }
    result
}

## Team points from multiple matches
get_points_from_matches = function(team_id, match_ids) {
    points = numeric()
    for(i in 1:length(match_ids)) {
        points[i] = get_points(team_id, match_ids[i])
    }
    points
}

## Season leaderboard for stat
get_leaderboard = function(seas, stat, matchfilter = 5) {
    results = NULL
    base = player_stats %>% 
        filter(match_id %in% (
            get_season_matches(seas) %>% select(id) %>% unlist()
        )) %>% 
        group_by(player_id) %>% 
        filter(mof > 0)
    base = base %>% 
      filter(player_id %in% 
               (base %>% 
                  summarise(kpl = n()) %>% 
                  filter(kpl > matchfilter) %>% 
                  select(player_id) %>% unlist()
                )
             )
    switch(stat,
           mof = {
               results = base %>% 
                   select(player_id, mof) %>% 
                   summarise(mof_sum = sum(mof), mof_mean = mean(mof)) %>% 
                   arrange(-mof_sum) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id) %>% rename("Sum" = mof_sum, "Mean" = mof_mean, "Name" = name)
           },
           isi = {
               results = base %>% 
                   select(player_id, isi) %>% 
                   summarise(isi_sum = sum(isi), isi_mean = mean(isi)) %>% 
                   arrange(-isi_mean) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id) %>% rename("Total" = isi_sum, "Mean" = isi_mean, "Name" = name)
           },
           g = {
               results = base %>% 
                   select(player_id, g, mof) %>% 
                   summarise(g_sum = sum(g), mof_sum = sum(mof)) %>%
                   mutate(g_per_ninety = g_sum * 90 / mof_sum) %>% 
                   select(-mof_sum) %>% 
                   arrange(-g_sum) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id) %>% rename("Total" = g_sum, "Per90" = g_per_ninety, "Name" = name)
           },
           a = {
               results = base %>% 
                   select(player_id, a, mof) %>% 
                   summarise(a_sum = sum(a), mof_sum = sum(mof)) %>%
                   mutate(a_per_ninety = a_sum * 90 / mof_sum) %>% 
                   select(-mof_sum) %>% 
                   arrange(-a_sum) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id)  %>% rename("Total" = a_sum, "Per90" = a_per_ninety, "Name" = name)
           },
           s = {
               results = base %>% 
                   select(player_id, s, st, mof) %>% 
                   summarise(s_sum = sum(s), st_sum = sum(st), mof_sum = sum(mof)) %>%
                   mutate(
                       s_per_ninety = s_sum * 90 / mof_sum, 
                       st_per_ninety = st_sum * 90 / mof_sum,
                       s_percent = st_sum / s_sum * 100
                   ) %>% 
                   select(-mof_sum) %>% 
                   arrange(-s_sum) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id) %>% 
                   rename(
                     "ShotsTotal" = s_sum, 
                     "ShotsPer90" = s_per_ninety,
                     "ShotsOnTargetTotal" = st_sum, 
                     "ShotsOnTargetPer90" = st_per_ninety, 
                     "Shot-%" = s_percent, 
                     "Name" = name
                   )
           },
           st = {
               results = base %>% 
                   select(player_id, s, st, mof) %>% 
                   summarise(s_sum = sum(s), st_sum = sum(st), mof_sum = sum(mof)) %>%
                   mutate(
                       s_per_ninety = s_sum * 90 / mof_sum, 
                       st_per_ninety = st_sum * 90 / mof_sum, 
                       s_percent = st_sum / s_sum * 100
                   ) %>% 
                   select(-mof_sum) %>% 
                   arrange(-st_sum) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id)
           },
           f = {
               results = base %>% 
                   select(player_id, f, mof) %>% 
                   summarise(f_sum = sum(f), mof_sum = sum(mof)) %>%
                   mutate(f_per_ninety = f_sum * 90 / mof_sum) %>% 
                   select(-mof_sum) %>% 
                   arrange(-f_sum) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id) %>% 
                   rename("Total" = f_sum, "Per90" = f_per_ninety, "Name" = name)
           },
           fop = {
               results = base %>% 
                   select(player_id, fop, mof) %>% 
                   summarise(fop_sum = sum(fop), mof_sum = sum(mof)) %>%
                   mutate(fop_per_ninety = fop_sum * 90 / mof_sum) %>% 
                   select(-mof_sum) %>% 
                   arrange(-fop_sum) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id) %>% 
                   rename("Total" = fop_sum, "Per90" = fop_per_ninety, "Name" = name)
           },
           offs = {
               results = base %>% 
                   select(player_id, offs, mof) %>% 
                   summarise(offs_sum = sum(offs), mof_sum = sum(mof)) %>%
                   mutate(offs_per_ninety = offs_sum * 90 / mof_sum) %>% 
                   select(-mof_sum) %>% 
                   arrange(-offs_sum) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id) %>% 
                   rename("Total" = offs_sum, "Per90" = offs_per_ninety, "Name" = name)
           },
           pa = {
               results = base %>% 
                   select(player_id, pa, pap, mof) %>% 
                   summarise(
                       pa_sum = sum(pa), 
                       pap_mean = mean(pap),
                       mof_sum = sum(mof)
                   ) %>%
                   mutate(pa_per_ninety = pa_sum * 90 / mof_sum) %>% 
                   select(-mof_sum) %>% 
                   arrange(-pa_sum) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id) %>% 
                   rename("Total" = pa_sum, "Per90" = pa_per_ninety, "Accuracy-%" = pap_mean, "Name" = name)
           },
           pap = {
               results = base %>% 
                   select(player_id, pa, pap, mof) %>% 
                   summarise(
                       pa_sum = sum(pa), 
                       pap_mean = mean(pap), 
                       mof_sum = sum(mof)
                   ) %>%
                   mutate(pa_per_ninety = pa_sum * 90 / mof_sum) %>% 
                   filter(mof_sum > 180) %>% 
                   select(-mof_sum) %>% 
                   arrange(-pap_mean) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id)
           },
           c = {
               results = base %>% 
                   select(player_id, c, cw,cwp, mof) %>% 
                   summarise(
                       c_sum = sum(c),
                       cw_sum = sum(cw),
                       cwp_mean = mean(cwp), 
                       mof_sum = sum(mof)
                   ) %>%
                   mutate(
                       c_per_ninety = c_sum * 90 / mof_sum, 
                       cw_per_ninety = cw_sum * 90 / mof_sum
                   ) %>% 
                   filter(mof_sum > 180) %>% 
                   select(-mof_sum) %>% 
                   arrange(-c_sum) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id) %>% 
                   rename(
                     Total = c_sum, 
                     Per90 = c_per_ninety, 
                     Total_won = cw_sum, 
                     WonPer90 = cw_per_ninety, 
                     "Win%" = cwp_mean, 
                     "Name" = name
                   )
           },
           cw = {
               results = base %>% 
                   select(player_id, c, cw,cwp, mof) %>% 
                   summarise(
                       c_sum = sum(c),
                       cw_sum = sum(cw),
                       cwp_mean = mean(cwp), 
                       mof_sum = sum(mof)
                   ) %>%
                   mutate(
                       c_per_ninety = c_sum * 90 / mof_sum, 
                       cw_per_ninety = cw_sum * 90 / mof_sum
                   ) %>% 
                   filter(mof_sum > 180) %>% 
                   select(-mof_sum) %>% 
                   arrange(-cw_sum) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id)
           },
           cwp = {
               results = base %>% 
                   select(player_id, c, cw,cwp, mof) %>% 
                   summarise(
                       c_sum = sum(c),
                       cw_sum = sum(cw),
                       cwp_mean = mean(cwp), 
                       mof_sum = sum(mof)
                   ) %>%
                   mutate(
                       c_per_ninety = c_sum * 90 / mof_sum, 
                       cw_per_ninety = cw_sum * 90 / mof_sum
                   ) %>% 
                   filter(mof_sum > 180) %>% 
                   select(-mof_sum) %>% 
                   arrange(-cwp_mean) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id)
           },
           lb = {
               results = base %>% 
                   select(player_id, lb, mof) %>% 
                   summarise(lb_sum = sum(lb), mof_sum = sum(mof)) %>%
                   mutate(lb_per_ninety = lb_sum * 90 / mof_sum) %>% 
                   select(-mof_sum) %>% 
                   arrange(-lb_sum) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id) %>% 
                   rename("Total" = lb_sum, "Per90" = lb_per_ninety, "Name" = name)
           },
           t = {
               results = base %>% 
                   select(player_id, t, mof) %>% 
                   summarise(t_sum = sum(t), mof_sum = sum(mof)) %>%
                   mutate(t_per_ninety = t_sum * 90 / mof_sum) %>% 
                   select(-mof_sum) %>% 
                   arrange(-t_sum) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% 
                   select(-player_id) %>% 
                   rename("Total" = t_sum, "Per90" = t_per_ninety, "Name" = name)
           },
           d = {
               results = base %>% 
                   filter(!is.na(d) & d != 0) %>% 
                   summarise(d_sum = sum(d), mof_sum = sum(mof)) %>%
                   mutate(d_per_ninety = d_sum * 90 / mof_sum) %>% 
                   select(-mof_sum) %>% 
                   arrange(-d_per_ninety) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% select(-player_id) %>% 
                   rename("Total" = d_sum, "Per90" = t_per_ninety, "Name" = name)
           },
           spdm = {
               results = base %>% 
                   filter(!is.na(spdm) & d != spdm) %>% 
                   summarise(
                       spdm_max = max(spdm), 
                       spdm_mean = mean(spdm), 
                       spda_mean = mean(spda), 
                       mof_sum = sum(mof)
                   ) %>%
                   select(-mof_sum) %>% 
                   arrange(-spdm_max) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup() %>% select(-player_id) %>% 
                   rename("Max" = spdm_max, "Average max" = spdm_mean, "Average" = spda_mean, "Name" = name)
           },
           spda = {
               results = base %>% 
                   filter(!is.na(spdm) & d != spdm) %>% 
                   summarise(
                       spdm_max = max(spdm), 
                       spdm_mean = mean(spdm), 
                       spda_mean = mean(spda), 
                       mof_sum = sum(mof)
                   ) %>%
                   select(-mof_sum) %>% 
                   arrange(-spda_mean) %>% 
                   left_join(players, by = c("player_id" = "id")) %>%
                   ungroup()
           }
    )
    return(results)
}

## Season points for team (to date)
get_season_points = function(team_ids, seasons, from_date = NULL, before = T) {
    if(is.null(from_date)) {
        from_date = Sys.time()
    }
    if(!before) {
        from_date = as.POSIXct(from_date) + days(1)
    }
    played_matches = get_team_matches(team_ids, seasons) %>% filter(date <= from_date)
    if(nrow(played_matches) == 0) {
        return(NULL)
    }
    points = get_points_from_matches(team_ids, played_matches$id)
    points
}

## Teams for a season
get_season_teams = function(seas) {
    teams %>% 
        filter(
            id %in% (
                get_season_matches(seas) %>% 
                    select(home_team_id, away_team_id) %>% 
                    unlist()
            )
        ) %>% 
        select(id) %>% 
        unlist() %>% 
        as.numeric()
}

get_gd = function(teamid, matchid) {
    home_matches = matches %>% filter(home_team_id == teamid, id %in% matchid)
    away_matches = matches %>% filter(away_team_id == teamid, id %in% matchid)
    home_diffs = home_matches$home_team_score - home_matches$away_team_score
    away_diffs = away_matches$away_team_score - away_matches$home_team_score
    sum(home_diffs) + sum(away_diffs)
}

## League table for season (to date)
get_league_table = function(seas, from_date = NULL) {
    if (is.null(from_date)) {
      from_date = Sys.time()
    }
  if (from_date > as.POSIXct("2019-10-20") & seas == 2019) {
    from_date = "2019-10-20"
  }
    seas_teams = get_season_teams(seas)
    if(seas == 2019) {
      seas_teams = seas_teams[seas_teams != get_team_id("TPS")]
    }
    points = numeric()
    matches = numeric()
    gd = numeric()
    for(i in 1:length(seas_teams)) {
        point_seq = get_season_points(seas_teams[i], seas, from_date, before = F)
        if(is.null(point_seq)) {
            matches[i] = 0
            points[i] = 0
            gd[i] = 0
        }
        else {
            points[i] = sum(point_seq)
            matches[i] = length(point_seq)
            gd[i] = get_gd(
              seas_teams[i], 
              get_team_matches(seas_teams[i], seas) %>%
                filter(date < as.POSIXct(from_date) + days(1)) %>% 
                select(id) %>% 
                unlist()
            )
        }
    }
    
    data.frame(seas_teams, points, matches, gd) %>% 
        arrange(-points, -gd) %>% 
        left_join(teams, by = c("seas_teams" = "id")) %>% select(-seas_teams)
}

# Team name with id
get_team_name = function(team_ids) {
    teams %>% 
        filter(id %in% team_ids) %>% 
        select(name) %>% 
        unlist() %>% 
        as.character()
}

# Player name with id
get_player_name = function(player_ids) {
    players %>% 
        filter(id %in% player_ids) %>%
        select(name) %>%
        unlist() %>% 
        as.character()
}

# Team seed for season (to date)
get_seed = function(team_ids, seas, from_date = NULL) {
    tbl = get_league_table(seas, from_date) %>% mutate("seed" = row_number())
    tbl %>% filter(name %in% get_team_name(team_ids)) %>% 
        select(seed) %>% 
        unlist() %>% 
        as.numeric()
}

# Opposing matches for two teams
get_last_opp_matches = function(team_id1, team_id2, from_date = NULL) {
    if (is.null(from_date)) {
        from_date = Sys.time()
    }
    matches %>% 
        filter(
            home_team_id %in% c(team_id1, team_id2), 
            away_team_id %in% c(team_id1, team_id2), 
            date < from_date
        ) %>% 
        arrange(desc(date))
}

# Last matches for team
get_last_matches = function(team_id, seas = 2019, from_date = NULL) {
    if (is.null(from_date)) {
        from_date = Sys.time()
    }
    get_team_matches(team_id, seas) %>% 
        filter(date < from_date) %>% 
        arrange(desc(date))
}

# Differenet positions for player
get_positions = function(player_ids) {
    tactics %>% 
        filter(player_id %in% player_ids) %>% 
        select(pos_name) %>% 
        group_by(pos_name) %>% 
        summarise(kpl = n()) %>% 
        arrange(-kpl) %>% 
        filter(!is.na(pos_name))
}

# Match lineup for team
get_lineup = function(team_ids, match_ids) {
    tactics %>% 
        filter(team_id %in% team_ids, match_id %in% match_ids, second == 0) %>%
        select(player_id, pos_name)
}

# Match Formation for team
get_formation = function(team_ids, match_ids) {
    tacs = tactics %>% 
        filter(team_id == team_ids, match_id == match_ids, second == 0)
    formguess = ""
    if ("LB" %in% tacs$pos_name & "RB" %in% tacs$pos_name) {
        if ("LW" %in% tacs$pos_name) {
            if ("CAM" %in% tacs$pos_name) {
                formguess = "4-3-3 (attack)"
            } else if ("CDM" %in% tacs$pos_name) {
                formguess = "4-3-3 (defend)"
            } else {
                formguess = "4-3-3"
            }
        } else if ("LM" %in% tacs$pos_name) {
            if ("ST" %in% tacs$pos_name) {
                formguess = "4-1-4-1"
            } else {
                if("CAM" %in% tacs$pos_name) {
                    formguess = "4-4-2 (diamond)"
                } else {
                    formguess = "4-4-2"
                }
            }
        }
    } else if("LWB" %in% tacs$pos_name) {
        if("LS" %in% tacs$pos_name) {
            formguess = "3-5-2"
        } else if("LCAM" %in% tacs$pos_name){
            formguess = "3-4-2-1"
        }
    } 
    formguess
}

#
get_simple_pos = function(positions) {
  spos = character()
  for(pos in positions) {
    switch(pos, 
           "LS" = {
              spos = c(spos, "ST")
           },
           "RS" = {
              spos = c(spos, "ST")
           },
           "RCDM" = {
             spos = c(spos, "CDM")
           },
           "LCDM" = {
             spos = c(spos, "CDM")
           },
           "RCB" =  {
             spos = c(spos, "CB")
           },
           "LCB" = {
             spos = c(spos, "CB")
           },
           "RCM" = {
             spos = c(spos, "CM")
           },
           "LCM" = {
             spos = c(spos, "CM")
           },
           "LCAM" = {
             spos = c(spos, "CAM")
           },
           "RCAM" = {
             spos = c(spos, "CAM")
           },
           {
             spos = c(spos, pos)
           }
    )      
  }
  spos
}

# Get all players off a season for a team
get_players_season = function(seas, team_ids = teams$id) {
    players %>% 
        filter(
            id %in%(
                player_stats %>% 
                    filter(
                        match_id %in% (
                            get_season_matches(seas) %>% 
                                select(id) %>% 
                                unlist()
                        ),
                        team_id %in% team_ids
                    ) %>% 
                    select(player_id) %>% 
                    unlist()
            )
        )
}

# Get team for a player for specisified season
get_team_for_player = function(seas, player_ids) {
  ids = get_player_stats_season(player_ids, seas) %>% 
    group_by(team_id) %>% 
    summarise(kpl = n()) %>% 
    arrange(desc(kpl)) %>% 
    select(team_id) %>% 
    unlist()
  ids[1]
}

# Get the url of image of team logo from InStat database and generate html
get_team_image = function(team_id, width = 100, height = 100) {
  imgs = character()
  for (id in team_id) {
    img = paste(c(
      '<img src="',
      paste(
        paste(
          "http://instatfootball.tv/shakhtar_donetsk/instat_data/images/teams_logos/",
          id,
          sep = ""
        ),
        ".png",
        sep = ""
      ),
      '" width = "',
      width,
      '" height="',
      height,
      '">'
    ),
    collapse = "")
    imgs = c(imgs, img)
  }
  imgs
}

# Get the url of image of player from InStat database and generate html
get_player_image = function(player_id, width = 100, height = 100) {
  paste(c(
    '<img src="',
    paste(
      paste(
        "http://instatscout.com/images/players/180/",
        player_id,
        sep = ""
      ),
      ".png",
      sep = ""
    ),
    '" witdth = "',
    width,
    '" height="',
    height,
    '">'
  ),
  collapse = "")
}

# Create a radarchart for the player in the specified season
rChart = function(player_id, season) {
  test = get_player_stat_summary(player_id, season)
  if(anyNA(test)) {
    return()
  }
  summaries = data.frame()
  for(i in get_players_season(season)$id) {
    if(!("GK" %in% get_positions(i))) {
      summaries = rbind(summaries, get_player_stat_summary(i, season))
    }
  }
  mins = summaries %>% 
    filter(n>4) %>% 
    summarise_all(.funs = quantile, probs = 0.01) %>% 
    select("pap", "pa/90",  "g/90","s/90", "a/90","cw/90","cwp", "f/90", "t/90", "lb/90")
  maxs = summaries %>% 
    filter(n>4) %>% 
    summarise_all(.funs = quantile, probs = 0.99) %>% 
    select("pap", "pa/90",  "g/90","s/90", "a/90","cw/90","cwp", "f/90", "t/90", "lb/90")
  test = test %>% select("pap", "pa/90",  "g/90","s/90", "a/90","cw/90","cwp", "f/90", "t/90", "lb/90")
  mins = mins %>% unlist()
  maxs = maxs %>% unlist()
  test = rbind(c(maxs[1:7], mins[8], maxs[9], mins[10]), c(mins[1:7],maxs[8], mins[9], maxs[10]), test)
  names(test) = c(
    "Pass-%", 
    "Accurate passes/90",
    "Goals/90", 
    "Shots/90", 
    "Assists/90", 
    "Challenges Won/90", 
    "Challenge-%", 
    "Fouls/90 <-", 
    "Tackles/90", 
    "Lost balls/90 <-"
  )
  radarchart(
    axistype = 2,
    test, 
    pcol = rgb(0.2,0.5,0.5,0.9), 
    pfcol = rgb(0.2,0.5,0.5,0.5), 
    plwd=4, 
    cglcol="grey", 
    cglty=1, 
    axislabcol="grey", 
    #caxislabels=seq(0,20,5), 
    paxislabels = round(c(maxs[1:7], mins[8], maxs[9], mins[10]), 2),
    cglwd=0.8,
    vlcex=0.8
  )
  
}

# Create a formLine plot for the player in the specified season
formLine = function(player_id, season) {
  stats = get_player_stats_season(player_id, season)
  if(nrow(stats %>% filter(isi > 0)) > 5) {
    stats = stats  %>% mutate(i = row_number())
    ggplot(stats %>% filter(isi != 0), aes(x=i, y = isi)) + 
      geom_smooth(method = "loess") + 
      geom_point() + 
      xlab("Kierros") + 
      ylab("InStat Index")
  }
  else {
    text = "\n Needs more than 5 games  \n with InStat Index"
    ggplot() + 
      annotate("text", x = 4, y = 75, size=6, label = text)  + ylim(c(0,100)) +
      theme_void()
  }
  
}

shotMap = function(player_ids, 
                   seas = 2016:2019, 
                   types = c("Goal", "Shot on target", "Shot blocked", "Shot wide", "Shot on post")
                   ) {
  image_ggplot(image_crop(image_read("sf.jpg"), "256x312+256+4")) + 
    geom_point(
      data = events %>% 
        left_join(matches, by = c("match_id" = "id")) %>% 
        filter(player_id == player_ids, title %in% types, season %in% seas) %>% 
        mutate(title = factor(title, levels = c("Goal", "Shot on target", "Shot wide", "Shot blocked", "Shot on post"))), 
      aes(x = (pos_x - (105/2))/(105/2) * 240, y = (68-pos_y)/68 * 311, col = title, shape = title), size = 3, alpha = 0.6
    ) + 
    scale_color_manual(values = c("Goal" = "green", "Shot on target" = "blue", "Shot blocked" = "yellow", "Shot wide" = "red", "Shot on post" = "black")) + 
    scale_shape_manual(values = c("Goal" = 19, "Shot on target" = 15, "Shot blocked" = 17, "Shot wide" = 3, "Shot on post" = 10)) + 
    labs(col = "Tapahtuma", shape = "Tapahtuma")
}

# Create a seedplot for the team in the specified season
pf = function(team_id, season) {
    all_days = (get_team_matches(team_id, season))$date
    seeds = numeric()
    for(i in 1:(length(all_days)-1)) {
        seeds[i+1] = get_seed(team_id, season, from_date = all_days[i])
    }
    seeds[1] = 6
    freim = data.frame(seeds = seeds, TIMESTAMP = all_days) %>% mutate(date = as.character(TIMESTAMP))
    print(freim)
    ggplot(freim, aes(x = TIMESTAMP)) + 
        geom_line(aes(y = seeds), size = 1) + 
        xlab("")+
        scale_y_continuous(breaks = 1:12, labels = 1:12) + 
        theme_minimal()
}

## Contents of helper_functions.R end

# initalizations
season_team_choices = teams$name

ui <- fluidPage(theme = shinytheme("flatly"),
  navbarPage(title = "Veikkausliiga Statexplorer", id = "tabs",
    tabPanel("Ottelut",
       sidebarPanel(
         selectInput(
           "team_select", 
           "Valitse joukkue", 
           choices = season_team_choices, 
           selected = "KuPS"
         ),
         selectInput(
           "year_select1", 
           "Valitse kausi", 
           choices = 2016:2019,
           selected = 2019
         )
       ),
       mainPanel(
         fluidRow(
          column(12, htmlOutput("team_picture"))
         )
       ),
       dataTableOutput("match_table")
    ),
    tabPanel("Joukkuetilastot",
      fluidRow(
        column(3,selectInput(
          "match_select",
          "Valitse ottelu",
          choices = setNames(matches$id, paste(matches$name, strftime(matches$date, format = "%d.%m."), sep = " ")), 
          selected = (matches %>% tail(1))$id
        )),
        column(9, selectInput("year_filter", "Suodata kauden mukaan", choices = 2016:2019, selected = 2019))
      ),
      textOutput("match_date"),
      dataTableOutput("match_stats")
             
    ),
    tabPanel(
      "Pelaajatilastot",
      sidebarPanel(
        selectInput(
          "player_select",
          "Valitse pelaaja",
          choices = get_players_season(2019) %>% 
            select(name) %>% 
            arrange(name), 
          selected = get_players_season(2019) %>% 
            select(name) %>% 
            sample_n(1)
        ),
        selectInput(
          "year_select2",
          "Valitse kausi",
          choices = 2016:2019,
          selected = 2019
        ),
        selectInput(
          "team_filter",
          "Suodata joukkueen mukaan",
          choices = c("Kaikki", teams$name),
          selected = "Kaikki"
        ),
        selectInput(
          "pos_filter",
          "Suodata pelipaikan mukaan",
          choices = c(
            "Kaikki",
            "GK",
            "LB",
            "CB",
            "RB",
            "LWB",
            "RWB",
            "CDM",
            "LM",
            "CM",
            "RM",
            "CAM",
            "LW",
            "RW",
            "ST"
          ), 
          selected = "Kaikki"
        ),
        actionButton("filterButton1", "Suodata"),
        radioButtons(
          "type_select",
          "Valitse tyyppi",
          choices = c(
            "Ottelukohtaiset" = "matches", 
            "Yhteenveto" = "summary"
          ),
          selected = "summary"
        )
      
      ),
      mainPanel(
        fluidRow(
          column(2, htmlOutput("player_picture"), 
                 htmlOutput("team_picture2"), 
                 span(textOutput("position"), style = "font-size: 120%"),
                 actionButton("gotosm", "Laukaisukartta")
          ),
          column(10,
            plotOutput("rcplot")
          )
        )
      ),

      dataTableOutput("player_table")
      
    ),
    tabPanel("Parhaat",
        fluidRow(
          column(3, 
            selectInput(
              "stat_select", 
              "Valitse tilasto", 
              choices = setNames( 
                names(player_stats)[c(3:7, 9:12, 14, 17:20)],  
                c(
                  "Minutes on the field", 
                  "InStat index", "Goals", 
                  "Assists", 
                  "Shots",  
                  "Fouls", 
                  "Fouled", 
                  "Offsides", 
                  "Pass accuracy", 
                  "Challenges", 
                  "Lost balls", 
                  "Tackles", 
                  "Distance", 
                  "Speed"
                )
              ), selected = "isi"
            )
          ),
          column(3,
            selectInput(
              "year_select3", 
              "Valitse kausi", 
              choices = 2016:2019, 
              selected = 2019
            )
          ),
          column(3,
                 selectInput(
                   "pos_filter2",
                   "Suodata pelipaikan mukaan",
                   choices = c(
                     "Kaikki",
                     "GK",
                     "LB",
                     "CB",
                     "RB",
                     "LWB",
                     "RWB",
                     "CDM",
                     "LM",
                     "CM",
                     "RM",
                     "CAM",
                     "LW",
                     "RW",
                     "ST"
                   ),
                   selected = "Kaikki",
                   multiple = T
                 )
          ),
          column(3, 
            sliderInput(
              "matchfilter", 
              "Min games played", 
              min = 0, 
              max = 33, 
              value = 5, 
              step = 1
            )
          )
        ),
        dataTableOutput("leaderboard")
    ),
    tabPanel("Sarjataulukko",
        fluidRow(
          column(2,
            selectInput(
              "year_select4",
              "Valitse kausi",
              choices = 2016:2019,
              selected = 2019
            )
          ), 
          column(10,
            dateInput(
              "date_select", 
              "Ennen ajankohtaa"
            )
          )
        ),
      dataTableOutput("leaguetable")
    ),
    tabPanel("Laukaisukartta",
        fluidRow(
        
          column(2, 
                 checkboxGroupInput("smtype", label = "Valitse tapahtumat", choices = c("Goal", "Shot on target",  "Shot wide","Shot blocked", "Shot on post"), selected = c("Goal", "Shot on target",  "Shot wide","Shot blocked", "Shot on post"))
          ),
         
          column(2, htmlOutput("player_picture2"), h3(textOutput("player_name"))),
          column(8,
                 plotOutput("eventmap")
          )
      )  
    )
  )
)

server <- function(input, output, session) {
  # Filter team SelectInput in the "Teams" tabPanel according to the selected year
  observeEvent (input$year_select1,{
    seas = input$year_select1
    curr_selected = input$team_select
    curr_teams = teams %>%
      filter(id %in%
               (
                 matches %>%
                   filter(season == as.numeric(seas)) %>%
                   select(home_team_id, away_team_id) %>%
                   unlist()
               )) %>%
      select(name) %>%
      unlist() %>%
      as.character()
    if (!(curr_selected %in% curr_teams)) {
      curr_selected = "KuPS"
    }
    updateSelectInput(session,
                      "team_select",
                      choices = curr_teams,
                      selected = curr_selected)
  })
  # Filter the selection of players in the "Players" tabPanel according to selected team, year and position
  observeEvent(input$filterButton1, {
    seas = input$year_select2
    curr_selected = input$player_select
    team = input$team_filter
    pos = input$pos_filter
    if(team == "Kaikki") {
      team = teams$name
    }
    curr_players = get_players_season(seas, get_team_id(team))
    
    if(pos != "Kaikki") {
      names = character()
      for( i in 1:nrow(curr_players)) {
        ids = curr_players$id[i]
        poss = get_simple_pos(get_positions(ids)$pos_name[1:3]) %>% unique()
        if(pos %in% poss) {
          names = c(names, curr_players$name[i])
        }
      }
      curr_players = names
    }
    else {
      curr_players = curr_players$name
    }
   
    if(!(curr_selected %in% curr_players)) {
      curr_selected = curr_players[1]
    }
    updateSelectInput(
        session, 
        "player_select",
        "Valitse Pelaaja",
        choices = curr_players, 
        selected = curr_selected
    )
  })
  
  observeEvent(input$leaderboard_cell_clicked, {
    if(length(input$leaderboard_cell_clicked) > 0 && input$leaderboard_cell_clicked$col == 3) {
      updateTabsetPanel(session, "tabs", selected = "Pelaajatilastot")
      updateSelectInput(session, "player_select", choices = get_players_season(input$year_select3)$name, selected = input$leaderboard_cell_clicked$value)
      updateSelectInput(session, "year_select2", selected = input$year_select3)
    }
  })
  observeEvent(input$match_table_cell_clicked, {
    if(length(input$match_table_cell_clicked) > 0) {
      nm = input$match_table_cell_clicked$value
      curdata = get_team_matches(get_team_id(input$team_select), seas = input$year_select1) %>% arrange(date)
      newDate = curdata$date[input$match_table_cell_clicked$row]
      newID = (matches %>% filter(name == nm, date == newDate) %>% select(id))$id[1]
      updateTabsetPanel(session, "tabs", selected = "Joukkuetilastot")
      updateSelectInput(session, "year_filter", selected = input$year_select1)
      updateSelectInput(session, "match_select", selected = newID)
    }
  })
  observeEvent(input$year_filter, {
    matchs = matches %>% filter(season == input$year_filter)
    sel = input$match_select
    if(!(sel %in% matchs$id)) {
      sel = matchs$id[1]
    }
    updateSelectInput(session,"match_select", selected = sel, choices = setNames(matchs$id, paste(matchs$name, strftime(matchs$date, format = "%d.%m."), sep = " ")))
  })
  
  observeEvent(input$gotosm, {
    updateTabsetPanel(session, "tabs", selected = "Laukaisukartta")
    updateSelectInput(session, "smtype", selected = "Goal")
  })
  # Match results in "team" tab           
  output$match_table = renderDataTable(
    get_team_matches(get_team_id(input$team_select), seas = input$year_select1) %>% 
      arrange(date) %>% 
      transmute(
        "Ottelu" = name, 
        "Kotimaalit" = home_team_score, 
        "Vierasmaalit" = away_team_score, 
        "Päivämäärä" = as.character(date)
      ) %>% datatable(selection = list(target = "cell", type = "single"),options = list(dom  = 't',columnDefs = list(list(targets = '_all', searchable = FALSE)), pageLength = "40"))
  )
  output$match_date = renderText({
    ts = team_stats %>% filter(match_id == input$match_select) %>% left_join(matches, by = c("match_id" = "id"))
    strftime(ts$date[1], format = "%d.%m.%Y %H:%M:%S")
  })
  output$match_stats = renderDataTable(
    team_stats %>% 
      filter(match_id == input$match_select) %>% 
      left_join(matches, by = c("match_id" = "id")) %>%
      mutate(
        Team = get_team_image(team_id), 
        Score = c(home_team_score[1], away_team_score[1])
      ) %>% rename("Ball Possession" = bpp, "Shots" = s, "On target" = st, "Corners" = ck, "Fouls" = f, "Offsides" = offs, "Yellow Cards" = yc, "Passes" = pa, "Pass-%" = pap, "Challenges won" = cw, "Challenge-%" = cwp, "Red Cards"= rc, "Date" = date) %>%
      select("Team", "Score", "Shots", "On target", "Ball Possession", "Passes", "Pass-%", "Challenges won", "Challenge-%", "Fouls", "Yellow Cards", "Red Cards", "Corners", "Offsides") %>% 
      t() %>% 
      datatable(selection = "none", escape = F, options = list("pageLength" = 50, dom = "t"), colnames = c("", ""))
  )
  # Team picture in "team" tab
  output$team_picture = renderText(
    { 
      get_team_image(
        get_team_id(input$team_select)[1], 
        width = 200, 
        height = 200
      )
    }
  )
  # Player picture in "player" tab
  output$player_picture = renderText(
    {
      get_player_image(
        get_player_id(input$player_select), 
        width = 120, 
        height = 120
      )
    }
  )
  # Team picture in "player" tab
  output$team_picture2 = renderText(
    { 
      tm = get_team_for_player(input$year_select2, get_player_id(input$player_select))
      if(!is.na(tm)) {
        get_team_image(
          get_team_for_player(
            input$year_select2, 
            get_player_id(input$player_select)
          ), 
          width = 120, 
          height = 120
        )
      }
      else {
        "Not affiliated with any club this season"
      }
    }
  )
  # The table of player stats. Either matches of summary according to typefilter
  output$player_table = renderDataTable(
    if(input$player_select != "") {
      switch(input$type_select,
        "matches" = {
          get_player_stats_season(get_player_id(input$player_select), seas = input$year_select2) %>% 
            left_join(matches, by = c("match_id" = "id")) %>% 
            left_join(players %>% transmute(player_id = id, player_name = name), by = "player_id") %>% 
            arrange(date) %>% 
            transmute(
              "Ottelu" = name, 
              "Pelaaja" = player_name,
              "Päivämäärä" = as.character(date), 
              "Aika" = mof, 
              "Index" = isi, 
              "M" = g, 
              "MS" = a,
              "L" = s,
              "LKM" = st,
              "R" = f,
              "HV" = fop,
              "P" = offs,
              "TS" = pa,
              "S-%" = pap,
              "KK" = c,
              "KK-%" = cwp,
              "MP" = lb,
              "T" = t
            ) %>% 
            filter(Aika != 0)
        },
        "summary" = {
          summaries = get_player_stat_summary(get_player_id(input$player_select), input$year_select2) %>% t()
          if(anyNA(summaries)) {
            data.frame("Description" = "This player has played no matches")
          }
          else {
            data.frame(
              "Statistic" = c(
                "InStat Index", 
                "Minutes on the Field", 
                "Appearances", 
                "Goals" ,
                "Assists", 
                "Shots", 
                "Shots on target", 
                "Fouls", 
                "Fouled", 
                "Offsides", 
                "Accurate passes", 
                "Pass-%", 
                "Challenges", 
                "Challenges won", 
                "Challenges-%", 
                "Lost balls", 
                "Tackles", 
                "Average speed", 
                "Average maximum speed", 
                "Maximum speed"
              ), 
              "Value" = as.integer(summaries[1:20]), 
              "Per 90" = c(
                "", 
                "", 
                "", 
                round(summaries[21:28],2), 
                "",
                round(summaries[29:30],2),
                "", 
                round(summaries[31:length(summaries)],2), 
                "", 
                "", 
                ""
              )
            )
          }
        }
      )
    }, options = list(pageLength = 25 ,dom  = 't', columnDefs = list(list(targets = '_all', searchable = FALSE)), ordering = FALSE)
  )
  # Leaderboard in the learderboard tab
  output$leaderboard = renderDataTable( {
    board = get_leaderboard(input$year_select3, input$stat_select, input$matchfilter)
    tids = c()
    pids = c()
    pos = c()
    sel = logical()
    for(i in 1:nrow(board)) {
      currpos = get_positions(get_player_id(board$Name[i]))$pos_name
      if(length(currpos) > 3) {
        currpos = currpos[1:3]
      }
      currpos = unique(get_simple_pos(currpos))
      if(any(input$pos_filter2 %in% currpos) | "Kaikki" %in% input$pos_filter2) {
        tids = c(tids, 
                 get_team_image(
                  get_team_for_player(input$year_select3, get_player_id(board$Name[i])),
                  width = 50, 
                  height = 50
                )
        )
        pids = c(pids, get_player_image(get_player_id(board$Name[i]), width = 50, height = 50))
        pos = c(pos, paste(currpos, collapse = ", "))
        sel[i] = T
      }
      else {
        sel[i] = F
      }
    }
    board = board[sel,]
    board$Team = tids
    board$Player = pids
    board$Positions = pos
    board = board %>% mutate_if(is.numeric, .funs = round, digits = 2)
    board %>% DT::datatable(selection = list(target = "cell", mode = "single"),caption = h3("Click name to go to player statistics"),options = list(columnDefs = list(list(targets = '_all', searchable = FALSE))), escape = F)
  }
  )
  # League table tab
  output$leaguetable = renderDataTable({
    tabl = get_league_table(input$year_select4, from_date = as.POSIXct(input$date_select)) %>% rename("Pisteet" = points, "Ottelut" = matches, "Maaliero" = gd, "Joukkue" = name)
    team_imgs = character()
    for(i in 1:(nrow(tabl))) {
      team_imgs[i] = get_team_image(get_team_id(tabl$Joukkue[i]), width = 50, height = 50)
    }
    tabl %>% mutate(" " = team_imgs) %>% datatable(escape = F, options = list(pageLength = 20, dom = "t"))
  })
  # RadarChart or formLine in the player tab
  output$rcplot = renderPlot(
    switch(input$type_select,
           "matches" = {
             formLine(get_player_id(input$player_select), input$year_select2)
           },
           "summary" = {
             par(mar=c(0, 0, 0, 0))
             rChart(get_player_id(input$player_select), input$year_select2)
           }
           
    ), height = 400, width = 500
   
  )
  # Position in the player tab
  output$position = renderText( {
    pos = get_positions(get_player_id(input$player_select)) %>% select(pos_name) %>% unlist()
    if(length(pos) >= 3) {
      pos = pos[1:3]
    }
    unique(get_simple_pos(pos))
  }
  )
  output$eventmap = renderPlot({
      shotMap(get_player_id(input$player_select), input$year_select2, types = input$smtype)
  }, height = 400, width = 400
  )
  output$player_picture2 = renderText(
    {
      get_player_image(
        get_player_id(input$player_select), 
        width = 120, 
        height = 120
      )
    }
  )
  output$player_name = renderText(input$player_select)
}
shinyApp(ui = ui, server = server)