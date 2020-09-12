Joshua Burrows Project 1
================

# Title

Hi

``` r
recBaseURL <- "https://records.nhl.com/site/api"

# Return the requested table
getTable <- function(table){
  
  if(!is.string(table)){
    return(warning("Input must be a character string")) 
  }
  
  recFullURL <- paste0(recBaseURL, "/", table) 
  info <- recFullURL %>% GET() %>% content("text") %>% fromJSON(flatten = "TRUE")
  info <- info[[1]] %>% as_tibble()
  
  return(info)
}

# filter info from requested table by team name or team Id
filterTable <- function(table, nameName, teamIdName, name, idNum){
  
  if(!is.null(name)){
    if(!is.null(idNum)){
      return(warning("Search by either name or idNum, not both"))
    } else if(name == "all"){ 
        return(table)
      } else{
          team <- table %>% filter(table[[nameName]] == name)
          if(length(team[[nameName]]) == 0){
            return(warning("name should be a team name or all"))
          } else{
              return(team)
            }
        }
  } else if(!is.null(idNum)){
      if(idNum == "all"){
        return(table)
      } else{
          team <- table %>% filter(table[[teamIdName]] == idNum)
          if(length(team[[teamIdName]]) == 0){ 
            return(warning("Not a valid team idNum"))
          } else{
              return(team)
          } 
      }
    } 
  
  # if both arguments are null, return full table 
  return(table) 
  
}
```

``` r
# Get franchise info by name or teamId
franchise <- function(name = NULL, idNum = NULL){

  franchises <- getTable("franchise") 
  
  filterTable(franchises, nameName = "teamCommonName", teamIdName = "id", name, idNum) 
  
}

franchise(name = "Canadiens")
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 1 x 6
    ##      id firstSeasonId lastSeasonId
    ##   <int>         <int>        <int>
    ## 1     1      19171918           NA
    ## # ... with 3 more variables:
    ## #   mostRecentTeamId <int>,
    ## #   teamCommonName <chr>,
    ## #   teamPlaceName <chr>

``` r
# Return franchise team totals
teamTotals <- function(name = NULL, idNum = NULL){
  
  teamTotals <- getTable("franchise-team-totals") 
  
  filterTable(teamTotals, nameName = "teamName", teamIdName = "franchiseId", name, idNum) 
  
} 

teamTotals(name = "Montréal Canadiens")
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 2 x 30
    ##      id activeFranchise firstSeasonId
    ##   <int>           <int>         <int>
    ## 1    15               1      19171918
    ## 2    16               1      19171918
    ## # ... with 27 more variables:
    ## #   franchiseId <int>,
    ## #   gameTypeId <int>,
    ## #   gamesPlayed <int>,
    ## #   goalsAgainst <int>,
    ## #   goalsFor <int>, homeLosses <int>,
    ## #   homeOvertimeLosses <int>,
    ## #   homeTies <int>, homeWins <int>,
    ## #   lastSeasonId <int>, losses <int>,
    ## #   overtimeLosses <int>,
    ## #   penaltyMinutes <int>,
    ## #   pointPctg <dbl>, points <int>,
    ## #   roadLosses <int>,
    ## #   roadOvertimeLosses <int>,
    ## #   roadTies <int>, roadWins <int>,
    ## #   shootoutLosses <int>,
    ## #   shootoutWins <int>,
    ## #   shutouts <int>, teamId <int>,
    ## #   teamName <chr>, ties <int>,
    ## #   triCode <chr>, wins <int>

``` r
# Return season records for a given franchise
season <- function(name = NULL, idNum = NULL){
  
  seasonRecords <- getTable("franchise-season-records") 
  
  filterTable(seasonRecords, nameName = "franchiseName", teamIdName = "franchiseId", name, idNum) 
} 

season(name = "Montréal Canadiens")
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 1 x 57
    ##      id fewestGoals fewestGoalsAgai~
    ##   <int>       <int>            <int>
    ## 1     8         155              131
    ## # ... with 54 more variables:
    ## #   fewestGoalsAgainstSeasons <chr>,
    ## #   fewestGoalsSeasons <chr>,
    ## #   fewestLosses <int>,
    ## #   fewestLossesSeasons <chr>,
    ## #   fewestPoints <int>,
    ## #   fewestPointsSeasons <chr>,
    ## #   fewestTies <int>,
    ## #   fewestTiesSeasons <chr>,
    ## #   fewestWins <int>,
    ## #   fewestWinsSeasons <chr>,
    ## #   franchiseId <int>,
    ## #   franchiseName <chr>,
    ## #   homeLossStreak <int>,
    ## #   homeLossStreakDates <chr>,
    ## #   homePointStreak <int>,
    ## #   homePointStreakDates <chr>,
    ## #   homeWinStreak <int>,
    ## #   homeWinStreakDates <chr>,
    ## #   homeWinlessStreak <int>,
    ## #   homeWinlessStreakDates <chr>,
    ## #   lossStreak <int>,
    ## #   lossStreakDates <chr>,
    ## #   mostGameGoals <int>,
    ## #   mostGameGoalsDates <chr>,
    ## #   mostGoals <int>,
    ## #   mostGoalsAgainst <int>,
    ## #   mostGoalsAgainstSeasons <chr>,
    ## #   mostGoalsSeasons <chr>,
    ## #   mostLosses <int>,
    ## #   mostLossesSeasons <chr>,
    ## #   mostPenaltyMinutes <int>,
    ## #   mostPenaltyMinutesSeasons <chr>,
    ## #   mostPoints <int>,
    ## #   mostPointsSeasons <chr>,
    ## #   mostShutouts <int>,
    ## #   mostShutoutsSeasons <chr>,
    ## #   mostTies <int>,
    ## #   mostTiesSeasons <chr>,
    ## #   mostWins <int>,
    ## #   mostWinsSeasons <chr>,
    ## #   pointStreak <int>,
    ## #   pointStreakDates <chr>,
    ## #   roadLossStreak <int>,
    ## #   roadLossStreakDates <chr>,
    ## #   roadPointStreak <int>,
    ## #   roadPointStreakDates <chr>,
    ## #   roadWinStreak <int>,
    ## #   roadWinStreakDates <chr>,
    ## #   roadWinlessStreak <int>,
    ## #   roadWinlessStreakDates <chr>,
    ## #   winStreak <int>,
    ## #   winStreakDates <chr>,
    ## #   winlessStreak <int>,
    ## #   winlessStreakDates <chr>

``` r
# Test 
test <- GET("https://records.nhl.com/site/api/franchise-season-records?cayenneExp=franchiseId=1") %>% content("text") %>% fromJSON(flatten = TRUE) 
```

    ## No encoding supplied: defaulting to UTF-8.

``` r
test
```

    ## $data
    ##   id fewestGoals fewestGoalsAgainst
    ## 1  8         155                131
    ##   fewestGoalsAgainstSeasons
    ## 1              1955-56 (70)
    ##   fewestGoalsSeasons fewestLosses
    ## 1       1952-53 (70)            8
    ##   fewestLossesSeasons fewestPoints
    ## 1        1976-77 (80)           65
    ##   fewestPointsSeasons fewestTies
    ## 1        1950-51 (70)          5
    ##   fewestTiesSeasons fewestWins
    ## 1      1983-84 (80)         25
    ##   fewestWinsSeasons franchiseId
    ## 1      1950-51 (70)           1
    ##        franchiseName homeLossStreak
    ## 1 Montréal Canadiens              7
    ##                                    homeLossStreakDates
    ## 1 Dec 16 1939 - Jan 18 1940, Oct 28 2000 - Nov 25 2000
    ##   homePointStreak
    ## 1              34
    ##        homePointStreakDates
    ## 1 Nov 01 1976 - Apr 02 1977
    ##   homeWinStreak
    ## 1            13
    ##                                     homeWinStreakDates
    ## 1 Nov 02 1943 - Jan 08 1944, Jan 30 1977 - Mar 26 1977
    ##   homeWinlessStreak
    ## 1                15
    ##      homeWinlessStreakDates lossStreak
    ## 1 Dec 16 1939 - Mar 07 1940         12
    ##             lossStreakDates
    ## 1 Feb 13 1926 - Mar 13 1926
    ##   mostGameGoals
    ## 1            16
    ##             mostGameGoalsDates
    ## 1 Mar 03 1920 - MTL 16 @ QBD 3
    ##   mostGoals mostGoalsAgainst
    ## 1       387              295
    ##   mostGoalsAgainstSeasons
    ## 1            1983-84 (80)
    ##   mostGoalsSeasons mostLosses
    ## 1     1976-77 (80)         40
    ##                          mostLossesSeasons
    ## 1 1983-84 (80), 2000-01 (82), 2017-18 (82)
    ##   mostPenaltyMinutes
    ## 1               1847
    ##   mostPenaltyMinutesSeasons mostPoints
    ## 1              1995-96 (82)        132
    ##   mostPointsSeasons mostShutouts
    ## 1      1976-77 (80)           22
    ##   mostShutoutsSeasons mostTies
    ## 1        1928-29 (44)       23
    ##   mostTiesSeasons mostWins
    ## 1    1962-63 (70)       60
    ##   mostWinsSeasons pointStreak
    ## 1    1976-77 (80)          28
    ##            pointStreakDates
    ## 1 Dec 18 1977 - Feb 23 1978
    ##   roadLossStreak
    ## 1             10
    ##         roadLossStreakDates
    ## 1 Jan 16 1926 - Mar 13 1926
    ##   roadPointStreak
    ## 1              23
    ##        roadPointStreakDates
    ## 1 Nov 27 1974 - Mar 12 1975
    ##   roadWinStreak
    ## 1             8
    ##                                     roadWinStreakDates
    ## 1 Dec 18 1977 - Jan 18 1978, Jan 21 1982 - Feb 21 1982
    ##   roadWinlessStreak
    ## 1                12
    ##                                 roadWinlessStreakDates
    ## 1 Nov 26 1933 - Jan 28 1934, Oct 20 1951 - Dec 13 1951
    ##   winStreak            winStreakDates
    ## 1        12 Jan 06 1968 - Feb 03 1968
    ##   winlessStreak
    ## 1             8
    ##                                     winlessStreakDates
    ## 1 Nov 16 2019 - Dec 01 2019, Dec 28 2019 - Jan 09 2020
    ## 
    ## $total
    ## [1] 1

``` r
# Return franchise goalie records 
goalie <- function(name = NULL, idNum = NULL){
  
  goalieRecords <- getTable("franchise-goalie-records")
  
  filterTable(goalieRecords, nameName = "franchiseName", teamIdName = "franchiseId", name, idNum)
  
} 

goalie(name = "Montréal Canadiens")
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 37 x 29
    ##       id activePlayer firstName
    ##    <int> <lgl>        <chr>    
    ##  1   261 FALSE        Patrick  
    ##  2   294 TRUE         Carey    
    ##  3   296 FALSE        Jacques  
    ##  4   327 FALSE        George   
    ##  5   414 FALSE        Stephane 
    ##  6   437 FALSE        Jeff     
    ##  7   450 FALSE        Brian    
    ##  8   457 FALSE        Denis    
    ##  9   469 FALSE        Pat      
    ## 10   511 FALSE        Roland   
    ## # ... with 27 more rows, and 26 more
    ## #   variables: franchiseId <int>,
    ## #   franchiseName <chr>,
    ## #   gameTypeId <int>,
    ## #   gamesPlayed <int>, lastName <chr>,
    ## #   losses <int>,
    ## #   mostGoalsAgainstDates <chr>,
    ## #   mostGoalsAgainstOneGame <int>,
    ## #   mostSavesDates <chr>,
    ## #   mostSavesOneGame <int>,
    ## #   mostShotsAgainstDates <chr>,
    ## #   mostShotsAgainstOneGame <int>,
    ## #   mostShutoutsOneSeason <int>,
    ## #   mostShutoutsSeasonIds <chr>,
    ## #   mostWinsOneSeason <int>,
    ## #   mostWinsSeasonIds <chr>,
    ## #   overtimeLosses <int>,
    ## #   playerId <int>,
    ## #   positionCode <chr>,
    ## #   rookieGamesPlayed <int>,
    ## #   rookieShutouts <int>,
    ## #   rookieWins <int>, seasons <int>,
    ## #   shutouts <int>, ties <int>,
    ## #   wins <int>

``` r
skater <- function(name = NULL, idNum = NULL){
  
  skaterRecords <- getTable("franchise-skater-records") 
  
  filterTable(skaterRecords, nameName = "franchiseName", teamIdName = "franchiseId", name, idNum) 
  
}

skater(name = "Montréal Canadiens")
```

    ## No encoding supplied: defaulting to UTF-8.

    ## # A tibble: 788 x 30
    ##       id activePlayer assists firstName
    ##    <int> <lgl>          <int> <chr>    
    ##  1 16891 FALSE            712 Jean     
    ##  2 16911 FALSE            688 Henri    
    ##  3 16990 FALSE            422 Maurice  
    ##  4 17000 FALSE            728 Guy      
    ##  5 17025 FALSE             87 Chris    
    ##  6 17054 FALSE            368 Steve    
    ##  7 17074 FALSE            346 Peter    
    ##  8 17138 FALSE            369 Mats     
    ##  9 17191 FALSE            686 Larry    
    ## 10 17199 FALSE              0 Reg      
    ## # ... with 778 more rows, and 26 more
    ## #   variables: franchiseId <int>,
    ## #   franchiseName <chr>,
    ## #   gameTypeId <int>,
    ## #   gamesPlayed <int>, goals <int>,
    ## #   lastName <chr>,
    ## #   mostAssistsGameDates <chr>,
    ## #   mostAssistsOneGame <int>,
    ## #   mostAssistsOneSeason <int>,
    ## #   mostAssistsSeasonIds <chr>,
    ## #   mostGoalsGameDates <chr>,
    ## #   mostGoalsOneGame <int>,
    ## #   mostGoalsOneSeason <int>,
    ## #   mostGoalsSeasonIds <chr>,
    ## #   mostPenaltyMinutesOneSeason <int>,
    ## #   mostPenaltyMinutesSeasonIds <chr>,
    ## #   mostPointsGameDates <chr>,
    ## #   mostPointsOneGame <int>,
    ## #   mostPointsOneSeason <int>,
    ## #   mostPointsSeasonIds <chr>,
    ## #   penaltyMinutes <int>,
    ## #   playerId <int>, points <int>,
    ## #   positionCode <chr>,
    ## #   rookiePoints <int>, seasons <int>

``` r
statsBaseURL <- "https://statsapi.web.nhl.com/api/v1/teams"

getStatsTable <- function(modifier = NULL, seasonId = NULL, teamIds = NULL){
  
  if(is.null(modifier)){
    base <- statsBaseURL %>% GET() %>% content("text") %>% fromJSON(flatten = TRUE)
  base <- base[[2]] %>% as_tibble()
  return(base)
  }
    
  if(modifier == "team.roster"){
    mod = "?expand=team.roster"
  } 
  
  if(modifier == "person.names"){
    mod = "?expand=person.names" 
  }
  
  if(modifier == "team.schedule.next"){
    mod = "?expand=team.schedule.next"
  } 
  
  if(modifier == "team.schedule.previous"){
    mod = "?expand=team.schedule.previous"
  }
  
  if(modifier == "team.stats"){
    mod = "?expand=team.stats"
  }
  
  if(modifier == "team.roster&season"){
    mod = paste0("?expand=team.roster&season=", seasonId)
  }
  
  if(modifier == "teamId"){
    mod = paste0("?teamId=", teamIds)
  }
  
  statsFullURL <- paste0(statsBaseURL, "/", mod)
  
  full <- statsFullURL %>% GET() %>% content("text") %>% fromJSON(flatten = TRUE)
  full <- full[[2]] %>% as_tibble()
  
  return(full)
}

getStatsTable(modifier = "teamId", teamIds = "1,2,5")
```

    ## # A tibble: 3 x 29
    ##      id name  link  abbreviation
    ##   <int> <chr> <chr> <chr>       
    ## 1     1 New ~ /api~ NJD         
    ## 2     2 New ~ /api~ NYI         
    ## 3     5 Pitt~ /api~ PIT         
    ## # ... with 25 more variables:
    ## #   teamName <chr>,
    ## #   locationName <chr>,
    ## #   firstYearOfPlay <chr>,
    ## #   shortName <chr>,
    ## #   officialSiteUrl <chr>,
    ## #   franchiseId <int>, active <lgl>,
    ## #   venue.name <chr>,
    ## #   venue.link <chr>,
    ## #   venue.city <chr>, venue.id <int>,
    ## #   venue.timeZone.id <chr>,
    ## #   venue.timeZone.offset <int>,
    ## #   venue.timeZone.tz <chr>,
    ## #   division.id <int>,
    ## #   division.name <chr>,
    ## #   division.nameShort <chr>,
    ## #   division.link <chr>,
    ## #   division.abbreviation <chr>,
    ## #   conference.id <int>,
    ## #   conference.name <chr>,
    ## #   conference.link <chr>,
    ## #   franchise.franchiseId <int>,
    ## #   franchise.teamName <chr>,
    ## #   franchise.link <chr>

``` r
teamStats <- function(modifier = NULL, seasonId = NULL, teamIds = NULL){
  
  # get base table 
  base <- getStatsTable()
  
  # if no modifier is present
  if(is.null(modifier)){
    return(base)
  }

  # team.roster modifier
  if(modifier == "team.roster"){
    
    stats <- getStatsTable(modifier) 
    
    roster <- stats[["roster.roster"]] 
    
    for(i in 1:length(roster)){
      roster[[i]]$name <- stats[["name"]][[i]]
    }
    
    roster <- do.call(rbind, roster)
    statsFlatten <- left_join(roster, stats, by = "name") %>% select(-c("roster.roster"))
    return(statsFlatten)
  } 
  
  # person.names modifier, which appears to be the same as the unmodified endpoint 
  if(modifier == "person.names"){ 
    stats <- getStatsTable(modifier) 
    return(stats) 
  } 
  
  # team.schedule.next modifier 
  if(modifier == "team.schedule.next"){
    
    stats <- getStatsTable(modifier)
    
    # Grab column that needs to be flattened 
    dates <- stats[["nextGameSchedule.dates"]]
    
    # Create names column to track data by team
    # Create list of teams that have a next game scheduled 
    datesNonNull <- list()
    for(i in 1:length(dates)){
      dates[[i]]$name <- stats[["name"]][[i]]
      if(length(dates[[i]]) > 1){
        datesNonNull <- cbind(datesNonNull, dates[i])
      }
    }
    
    # Flatten datesNonNull
    for(i in 1:length(datesNonNull)){
      datesNonNull[[i]] <- cbind(datesNonNull[[i]], datesNonNull[[i]][["games"]]) %>% select(-c("games")) 
      
      datesNonNull[[i]][["events"]] <- 
        datesNonNull[[i]][["events"]][[1]][1][[1]]
      
      datesNonNull[[i]][["matches"]] <- 
        datesNonNull[[4]][["matches"]][[1]][1][[1]]
    } 
    
    # Put dates info in one tibble 
    datesVert <- tibble() 
    for(i in 1:length(datesNonNull)){
      datesVert <- rbind(datesVert, datesNonNull[[i]]) 
    } 
    
    # Join flattened data with full data set 
    schedule <- left_join(stats, datesVert, by = "name") %>% select(-c("nextGameSchedule.dates"))
    # need to rename any cols? 
    
    return(schedule) 
  } 
  
  # team.schedule.previous modifier
  if(modifier == "team.schedule.previous"){
    
    # Have to type modifier here? 
    stats <- getStatsTable(modifier) 
    
    # Grab col that needs to be flattened 
    dates <- stats[["previousGameSchedule.dates"]]
    
    # Create names column to track data by team
    # Create list of teams that have a next game scheduled 
    datesNonNull <- list()
    for(i in 1:length(dates)){
      dates[[i]]$name <- stats[["name"]][[i]]
      if(length(dates[[i]]) > 1){
        datesNonNull <- cbind(datesNonNull, dates[i])
      }
    }
    
    # Flatten datesNonNull
    for(i in 1:length(datesNonNull)){
      datesNonNull[[i]] <- cbind(datesNonNull[[i]], datesNonNull[[i]][["games"]]) %>% select(-c("games")) 
      
      datesNonNull[[i]][["events"]] <- 
        datesNonNull[[i]][["events"]][[1]][1][[1]]
      
      datesNonNull[[i]][["matches"]] <- 
        datesNonNull[[4]][["matches"]][[1]][1][[1]]
    } 
    
    # Create venue.id col where it doesn't exist 
    for(i in 1:length(datesNonNull)){
      if(is.null(datesNonNull[[i]][["venue.id"]])){
        datesNonNull[[i]][["venue.id"]] <- NA
      }
    }
    
    # Put dates info in one tibble 
    datesVert <- tibble() 
    for(i in 1:length(datesNonNull)){
      datesVert <- rbind(datesVert, datesNonNull[[i]]) 
    } 
  
    # Join flattened data with full data set 
    schedule <- left_join(stats, datesVert, by = "name") %>% select(-c("previousGameSchedule.dates"))
    # need to rename any cols? 
    
    return(schedule) 
  } 
  
  # MODIFIER
  # team.stats
  if(modifier == "team.stats"){ 
    stats <- getStatsTable(modifier = "team.stats")
    
    statsFull <- tibble()
    for(i in stats[["name"]]){
      teamStats <- stats %>% filter(name == i)
    
      teamStatsSub <- teamStats[["teamStats"]]
    
      wideRanks <- teamStatsSub[[1]][["splits"]][[1]] 
    
      longRanks <- tibble(statName = colnames(wideRanks[, 1:31]), statValue = unlist(wideRanks[1, 1:31]), statRank = unlist(wideRanks[2, 1:31]))
  
      ranks <- cbind(longRanks, wideRanks[1, 32:34], row.names = NULL)
      
      ranks <- cbind(ranks, teamStatsSub[[1]], row.names = NULL) %>% select(-c("splits"))
    
      ranks <- ranks %>% rename("name" = "team.name") 
    
      teamStats <- left_join(ranks, teamStats, by = "name") %>% select(-c("teamStats"))
      
      statsFull <- rbind(statsFull, teamStats)
    } 
    return(statsFull) 
  } 
  
  # MODIFIER
  # team.roster&season
  if(modifier == "team.roster&season"){
    stats <- getStatsTable(modifier, seasonId) 
    
    roster <- stats[["roster.roster"]]
    
    for(i in 1:length(roster)){
      roster[[i]]$name <- stats[["name"]][[i]]
    }
    
    roster <- do.call(rbind, roster)
    statsFlatten <- left_join(roster, stats, by = "name") %>% select(-c("roster.roster"))
    return(statsFlatten)
  }
  
  # MODIFIER
  # teamId
  if(modifier == "teamId"){
    stats <- getStatsTable(modifier, teamIds = teamIds) 
    return(stats) 
  }
  
} 

teamStats()
```

    ## # A tibble: 31 x 29
    ##       id name  link  abbreviation
    ##    <int> <chr> <chr> <chr>       
    ##  1     1 New ~ /api~ NJD         
    ##  2     2 New ~ /api~ NYI         
    ##  3     3 New ~ /api~ NYR         
    ##  4     4 Phil~ /api~ PHI         
    ##  5     5 Pitt~ /api~ PIT         
    ##  6     6 Bost~ /api~ BOS         
    ##  7     7 Buff~ /api~ BUF         
    ##  8     8 Mont~ /api~ MTL         
    ##  9     9 Otta~ /api~ OTT         
    ## 10    10 Toro~ /api~ TOR         
    ## # ... with 21 more rows, and 25 more
    ## #   variables: teamName <chr>,
    ## #   locationName <chr>,
    ## #   firstYearOfPlay <chr>,
    ## #   shortName <chr>,
    ## #   officialSiteUrl <chr>,
    ## #   franchiseId <int>, active <lgl>,
    ## #   venue.name <chr>,
    ## #   venue.link <chr>,
    ## #   venue.city <chr>, venue.id <int>,
    ## #   venue.timeZone.id <chr>,
    ## #   venue.timeZone.offset <int>,
    ## #   venue.timeZone.tz <chr>,
    ## #   division.id <int>,
    ## #   division.name <chr>,
    ## #   division.nameShort <chr>,
    ## #   division.link <chr>,
    ## #   division.abbreviation <chr>,
    ## #   conference.id <int>,
    ## #   conference.name <chr>,
    ## #   conference.link <chr>,
    ## #   franchise.franchiseId <int>,
    ## #   franchise.teamName <chr>,
    ## #   franchise.link <chr>

``` r
teamStats(modifier = "team.schedule.previous")
```

    ## # A tibble: 31 x 68
    ##       id name  link.x abbreviation
    ##    <int> <chr> <chr>  <chr>       
    ##  1     1 New ~ /api/~ NJD         
    ##  2     2 New ~ /api/~ NYI         
    ##  3     3 New ~ /api/~ NYR         
    ##  4     4 Phil~ /api/~ PHI         
    ##  5     5 Pitt~ /api/~ PIT         
    ##  6     6 Bost~ /api/~ BOS         
    ##  7     7 Buff~ /api/~ BUF         
    ##  8     8 Mont~ /api/~ MTL         
    ##  9     9 Otta~ /api/~ OTT         
    ## 10    10 Toro~ /api/~ TOR         
    ## # ... with 21 more rows, and 64 more
    ## #   variables: teamName <chr>,
    ## #   locationName <chr>,
    ## #   firstYearOfPlay <chr>,
    ## #   shortName <chr>,
    ## #   officialSiteUrl <chr>,
    ## #   franchiseId <int>, active <lgl>,
    ## #   venue.name.x <chr>,
    ## #   venue.link.x <chr>,
    ## #   venue.city <chr>,
    ## #   venue.id.x <int>,
    ## #   venue.timeZone.id <chr>,
    ## #   venue.timeZone.offset <int>,
    ## #   venue.timeZone.tz <chr>,
    ## #   division.id <int>,
    ## #   division.name <chr>,
    ## #   division.nameShort <chr>,
    ## #   division.link <chr>,
    ## #   division.abbreviation <chr>,
    ## #   conference.id <int>,
    ## #   conference.name <chr>,
    ## #   conference.link <chr>,
    ## #   franchise.franchiseId <int>,
    ## #   franchise.teamName <chr>,
    ## #   franchise.link <chr>,
    ## #   previousGameSchedule.totalItems <int>,
    ## #   previousGameSchedule.totalEvents <int>,
    ## #   previousGameSchedule.totalGames <int>,
    ## #   previousGameSchedule.totalMatches <int>,
    ## #   date <chr>, totalItems <int>,
    ## #   totalEvents <int>,
    ## #   totalGames <int>,
    ## #   totalMatches <int>, gamePk <int>,
    ## #   link.y <chr>, gameType <chr>,
    ## #   season <chr>, gameDate <chr>,
    ## #   status.abstractGameState <chr>,
    ## #   status.codedGameState <chr>,
    ## #   status.detailedState <chr>,
    ## #   status.statusCode <chr>,
    ## #   status.startTimeTBD <lgl>,
    ## #   teams.away.score <int>,
    ## #   teams.away.leagueRecord.wins <int>,
    ## #   teams.away.leagueRecord.losses <int>,
    ## #   teams.away.leagueRecord.ot <int>,
    ## #   teams.away.leagueRecord.type <chr>,
    ## #   teams.away.team.id <int>,
    ## #   teams.away.team.name <chr>,
    ## #   teams.away.team.link <chr>,
    ## #   teams.home.score <int>,
    ## #   teams.home.leagueRecord.wins <int>,
    ## #   teams.home.leagueRecord.losses <int>,
    ## #   teams.home.leagueRecord.ot <int>,
    ## #   teams.home.leagueRecord.type <chr>,
    ## #   teams.home.team.id <int>,
    ## #   teams.home.team.name <chr>,
    ## #   teams.home.team.link <chr>,
    ## #   venue.name.y <chr>,
    ## #   venue.link.y <chr>,
    ## #   content.link <chr>,
    ## #   venue.id.y <int>

``` r
teamStats(modifier = "team.stats")
```

    ##                       statName
    ## 1             stat.gamesPlayed
    ## 2                    stat.wins
    ## 3                  stat.losses
    ## 4                      stat.ot
    ## 5                     stat.pts
    ## 6                  stat.ptPctg
    ## 7            stat.goalsPerGame
    ## 8     stat.goalsAgainstPerGame
    ## 9              stat.evGGARatio
    ## 10    stat.powerPlayPercentage
    ## 11         stat.powerPlayGoals
    ## 12  stat.powerPlayGoalsAgainst
    ## 13 stat.powerPlayOpportunities
    ## 14  stat.penaltyKillPercentage
    ## 15           stat.shotsPerGame
    ## 16           stat.shotsAllowed
    ## 17          stat.winScoreFirst
    ## 18       stat.winOppScoreFirst
    ## 19        stat.winLeadFirstPer
    ## 20       stat.winLeadSecondPer
    ## 21         stat.winOutshootOpp
    ## 22        stat.winOutshotByOpp
    ## 23          stat.faceOffsTaken
    ## 24            stat.faceOffsWon
    ## 25           stat.faceOffsLost
    ## 26   stat.faceOffWinPercentage
    ##    statValue statRank team.id
    ## 1         69     <NA>       1
    ## 2         28     29th       1
    ## 3         29     23rd       1
    ## 4         12      3rd       1
    ## 5         68     25th       1
    ## 6       49.3     25th       1
    ## 7      2.681     24th       1
    ## 8      3.246     29th       1
    ## 9     0.7821     29th       1
    ## 10      17.9     21st       1
    ## 11        42     18th       1
    ## 12        39     14th       1
    ## 13       234      3rd       1
    ## 14      82.3      7th       1
    ## 15   30.6812     22nd       1
    ## 16   32.6957     26th       1
    ## 17     0.484     29th       1
    ## 18     0.342     10th       1
    ## 19       0.5     30th       1
    ## 20     0.704     28th       1
    ## 21     0.417     24th       1
    ## 22     0.381     24th       1
    ## 23      3990     16th       1
    ## 24      1869     27th       1
    ## 25      2121     30th       1
    ## 26      46.8     29th       1
    ##                 name       team.link
    ## 1  New Jersey Devils /api/v1/teams/1
    ## 2  New Jersey Devils /api/v1/teams/1
    ## 3  New Jersey Devils /api/v1/teams/1
    ## 4  New Jersey Devils /api/v1/teams/1
    ## 5  New Jersey Devils /api/v1/teams/1
    ## 6  New Jersey Devils /api/v1/teams/1
    ## 7  New Jersey Devils /api/v1/teams/1
    ## 8  New Jersey Devils /api/v1/teams/1
    ## 9  New Jersey Devils /api/v1/teams/1
    ## 10 New Jersey Devils /api/v1/teams/1
    ## 11 New Jersey Devils /api/v1/teams/1
    ## 12 New Jersey Devils /api/v1/teams/1
    ## 13 New Jersey Devils /api/v1/teams/1
    ## 14 New Jersey Devils /api/v1/teams/1
    ## 15 New Jersey Devils /api/v1/teams/1
    ## 16 New Jersey Devils /api/v1/teams/1
    ## 17 New Jersey Devils /api/v1/teams/1
    ## 18 New Jersey Devils /api/v1/teams/1
    ## 19 New Jersey Devils /api/v1/teams/1
    ## 20 New Jersey Devils /api/v1/teams/1
    ## 21 New Jersey Devils /api/v1/teams/1
    ## 22 New Jersey Devils /api/v1/teams/1
    ## 23 New Jersey Devils /api/v1/teams/1
    ## 24 New Jersey Devils /api/v1/teams/1
    ## 25 New Jersey Devils /api/v1/teams/1
    ## 26 New Jersey Devils /api/v1/teams/1
    ##     type.displayName type.gameType.id
    ## 1  statsSingleSeason                R
    ## 2  statsSingleSeason                R
    ## 3  statsSingleSeason                R
    ## 4  statsSingleSeason                R
    ## 5  statsSingleSeason                R
    ## 6  statsSingleSeason                R
    ## 7  statsSingleSeason                R
    ## 8  statsSingleSeason                R
    ## 9  statsSingleSeason                R
    ## 10 statsSingleSeason                R
    ## 11 statsSingleSeason                R
    ## 12 statsSingleSeason                R
    ## 13 statsSingleSeason                R
    ## 14 statsSingleSeason                R
    ## 15 statsSingleSeason                R
    ## 16 statsSingleSeason                R
    ## 17 statsSingleSeason                R
    ## 18 statsSingleSeason                R
    ## 19 statsSingleSeason                R
    ## 20 statsSingleSeason                R
    ## 21 statsSingleSeason                R
    ## 22 statsSingleSeason                R
    ## 23 statsSingleSeason                R
    ## 24 statsSingleSeason                R
    ## 25 statsSingleSeason                R
    ## 26 statsSingleSeason                R
    ##    type.gameType.description
    ## 1             Regular season
    ## 2             Regular season
    ## 3             Regular season
    ## 4             Regular season
    ## 5             Regular season
    ## 6             Regular season
    ## 7             Regular season
    ## 8             Regular season
    ## 9             Regular season
    ## 10            Regular season
    ## 11            Regular season
    ## 12            Regular season
    ## 13            Regular season
    ## 14            Regular season
    ## 15            Regular season
    ## 16            Regular season
    ## 17            Regular season
    ## 18            Regular season
    ## 19            Regular season
    ## 20            Regular season
    ## 21            Regular season
    ## 22            Regular season
    ## 23            Regular season
    ## 24            Regular season
    ## 25            Regular season
    ## 26            Regular season
    ##    type.gameType.postseason id
    ## 1                     FALSE  1
    ## 2                     FALSE  1
    ## 3                     FALSE  1
    ## 4                     FALSE  1
    ## 5                     FALSE  1
    ## 6                     FALSE  1
    ## 7                     FALSE  1
    ## 8                     FALSE  1
    ## 9                     FALSE  1
    ## 10                    FALSE  1
    ## 11                    FALSE  1
    ## 12                    FALSE  1
    ## 13                    FALSE  1
    ## 14                    FALSE  1
    ## 15                    FALSE  1
    ## 16                    FALSE  1
    ## 17                    FALSE  1
    ## 18                    FALSE  1
    ## 19                    FALSE  1
    ## 20                    FALSE  1
    ## 21                    FALSE  1
    ## 22                    FALSE  1
    ## 23                    FALSE  1
    ## 24                    FALSE  1
    ## 25                    FALSE  1
    ## 26                    FALSE  1
    ##               link abbreviation
    ## 1  /api/v1/teams/1          NJD
    ## 2  /api/v1/teams/1          NJD
    ## 3  /api/v1/teams/1          NJD
    ## 4  /api/v1/teams/1          NJD
    ## 5  /api/v1/teams/1          NJD
    ## 6  /api/v1/teams/1          NJD
    ## 7  /api/v1/teams/1          NJD
    ## 8  /api/v1/teams/1          NJD
    ## 9  /api/v1/teams/1          NJD
    ## 10 /api/v1/teams/1          NJD
    ## 11 /api/v1/teams/1          NJD
    ## 12 /api/v1/teams/1          NJD
    ## 13 /api/v1/teams/1          NJD
    ## 14 /api/v1/teams/1          NJD
    ## 15 /api/v1/teams/1          NJD
    ## 16 /api/v1/teams/1          NJD
    ## 17 /api/v1/teams/1          NJD
    ## 18 /api/v1/teams/1          NJD
    ## 19 /api/v1/teams/1          NJD
    ## 20 /api/v1/teams/1          NJD
    ## 21 /api/v1/teams/1          NJD
    ## 22 /api/v1/teams/1          NJD
    ## 23 /api/v1/teams/1          NJD
    ## 24 /api/v1/teams/1          NJD
    ## 25 /api/v1/teams/1          NJD
    ## 26 /api/v1/teams/1          NJD
    ##    teamName locationName
    ## 1    Devils   New Jersey
    ## 2    Devils   New Jersey
    ## 3    Devils   New Jersey
    ## 4    Devils   New Jersey
    ## 5    Devils   New Jersey
    ## 6    Devils   New Jersey
    ## 7    Devils   New Jersey
    ## 8    Devils   New Jersey
    ## 9    Devils   New Jersey
    ## 10   Devils   New Jersey
    ## 11   Devils   New Jersey
    ## 12   Devils   New Jersey
    ## 13   Devils   New Jersey
    ## 14   Devils   New Jersey
    ## 15   Devils   New Jersey
    ## 16   Devils   New Jersey
    ## 17   Devils   New Jersey
    ## 18   Devils   New Jersey
    ## 19   Devils   New Jersey
    ## 20   Devils   New Jersey
    ## 21   Devils   New Jersey
    ## 22   Devils   New Jersey
    ## 23   Devils   New Jersey
    ## 24   Devils   New Jersey
    ## 25   Devils   New Jersey
    ## 26   Devils   New Jersey
    ##    firstYearOfPlay  shortName
    ## 1             1982 New Jersey
    ## 2             1982 New Jersey
    ## 3             1982 New Jersey
    ## 4             1982 New Jersey
    ## 5             1982 New Jersey
    ## 6             1982 New Jersey
    ## 7             1982 New Jersey
    ## 8             1982 New Jersey
    ## 9             1982 New Jersey
    ## 10            1982 New Jersey
    ## 11            1982 New Jersey
    ## 12            1982 New Jersey
    ## 13            1982 New Jersey
    ## 14            1982 New Jersey
    ## 15            1982 New Jersey
    ## 16            1982 New Jersey
    ## 17            1982 New Jersey
    ## 18            1982 New Jersey
    ## 19            1982 New Jersey
    ## 20            1982 New Jersey
    ## 21            1982 New Jersey
    ## 22            1982 New Jersey
    ## 23            1982 New Jersey
    ## 24            1982 New Jersey
    ## 25            1982 New Jersey
    ## 26            1982 New Jersey
    ##                    officialSiteUrl
    ## 1  http://www.newjerseydevils.com/
    ## 2  http://www.newjerseydevils.com/
    ## 3  http://www.newjerseydevils.com/
    ## 4  http://www.newjerseydevils.com/
    ## 5  http://www.newjerseydevils.com/
    ## 6  http://www.newjerseydevils.com/
    ## 7  http://www.newjerseydevils.com/
    ## 8  http://www.newjerseydevils.com/
    ## 9  http://www.newjerseydevils.com/
    ## 10 http://www.newjerseydevils.com/
    ## 11 http://www.newjerseydevils.com/
    ## 12 http://www.newjerseydevils.com/
    ## 13 http://www.newjerseydevils.com/
    ## 14 http://www.newjerseydevils.com/
    ## 15 http://www.newjerseydevils.com/
    ## 16 http://www.newjerseydevils.com/
    ## 17 http://www.newjerseydevils.com/
    ## 18 http://www.newjerseydevils.com/
    ## 19 http://www.newjerseydevils.com/
    ## 20 http://www.newjerseydevils.com/
    ## 21 http://www.newjerseydevils.com/
    ## 22 http://www.newjerseydevils.com/
    ## 23 http://www.newjerseydevils.com/
    ## 24 http://www.newjerseydevils.com/
    ## 25 http://www.newjerseydevils.com/
    ## 26 http://www.newjerseydevils.com/
    ##    franchiseId active
    ## 1           23   TRUE
    ## 2           23   TRUE
    ## 3           23   TRUE
    ## 4           23   TRUE
    ## 5           23   TRUE
    ## 6           23   TRUE
    ## 7           23   TRUE
    ## 8           23   TRUE
    ## 9           23   TRUE
    ## 10          23   TRUE
    ## 11          23   TRUE
    ## 12          23   TRUE
    ## 13          23   TRUE
    ## 14          23   TRUE
    ## 15          23   TRUE
    ## 16          23   TRUE
    ## 17          23   TRUE
    ## 18          23   TRUE
    ## 19          23   TRUE
    ## 20          23   TRUE
    ## 21          23   TRUE
    ## 22          23   TRUE
    ## 23          23   TRUE
    ## 24          23   TRUE
    ## 25          23   TRUE
    ## 26          23   TRUE
    ##           venue.name
    ## 1  Prudential Center
    ## 2  Prudential Center
    ## 3  Prudential Center
    ## 4  Prudential Center
    ## 5  Prudential Center
    ## 6  Prudential Center
    ## 7  Prudential Center
    ## 8  Prudential Center
    ## 9  Prudential Center
    ## 10 Prudential Center
    ## 11 Prudential Center
    ## 12 Prudential Center
    ## 13 Prudential Center
    ## 14 Prudential Center
    ## 15 Prudential Center
    ## 16 Prudential Center
    ## 17 Prudential Center
    ## 18 Prudential Center
    ## 19 Prudential Center
    ## 20 Prudential Center
    ## 21 Prudential Center
    ## 22 Prudential Center
    ## 23 Prudential Center
    ## 24 Prudential Center
    ## 25 Prudential Center
    ## 26 Prudential Center
    ##             venue.link venue.city
    ## 1  /api/v1/venues/null     Newark
    ## 2  /api/v1/venues/null     Newark
    ## 3  /api/v1/venues/null     Newark
    ## 4  /api/v1/venues/null     Newark
    ## 5  /api/v1/venues/null     Newark
    ## 6  /api/v1/venues/null     Newark
    ## 7  /api/v1/venues/null     Newark
    ## 8  /api/v1/venues/null     Newark
    ## 9  /api/v1/venues/null     Newark
    ## 10 /api/v1/venues/null     Newark
    ## 11 /api/v1/venues/null     Newark
    ## 12 /api/v1/venues/null     Newark
    ## 13 /api/v1/venues/null     Newark
    ## 14 /api/v1/venues/null     Newark
    ## 15 /api/v1/venues/null     Newark
    ## 16 /api/v1/venues/null     Newark
    ## 17 /api/v1/venues/null     Newark
    ## 18 /api/v1/venues/null     Newark
    ## 19 /api/v1/venues/null     Newark
    ## 20 /api/v1/venues/null     Newark
    ## 21 /api/v1/venues/null     Newark
    ## 22 /api/v1/venues/null     Newark
    ## 23 /api/v1/venues/null     Newark
    ## 24 /api/v1/venues/null     Newark
    ## 25 /api/v1/venues/null     Newark
    ## 26 /api/v1/venues/null     Newark
    ##    venue.id venue.timeZone.id
    ## 1        NA  America/New_York
    ## 2        NA  America/New_York
    ## 3        NA  America/New_York
    ## 4        NA  America/New_York
    ## 5        NA  America/New_York
    ## 6        NA  America/New_York
    ## 7        NA  America/New_York
    ## 8        NA  America/New_York
    ## 9        NA  America/New_York
    ## 10       NA  America/New_York
    ## 11       NA  America/New_York
    ## 12       NA  America/New_York
    ## 13       NA  America/New_York
    ## 14       NA  America/New_York
    ## 15       NA  America/New_York
    ## 16       NA  America/New_York
    ## 17       NA  America/New_York
    ## 18       NA  America/New_York
    ## 19       NA  America/New_York
    ## 20       NA  America/New_York
    ## 21       NA  America/New_York
    ## 22       NA  America/New_York
    ## 23       NA  America/New_York
    ## 24       NA  America/New_York
    ## 25       NA  America/New_York
    ## 26       NA  America/New_York
    ##    venue.timeZone.offset
    ## 1                     -4
    ## 2                     -4
    ## 3                     -4
    ## 4                     -4
    ## 5                     -4
    ## 6                     -4
    ## 7                     -4
    ## 8                     -4
    ## 9                     -4
    ## 10                    -4
    ## 11                    -4
    ## 12                    -4
    ## 13                    -4
    ## 14                    -4
    ## 15                    -4
    ## 16                    -4
    ## 17                    -4
    ## 18                    -4
    ## 19                    -4
    ## 20                    -4
    ## 21                    -4
    ## 22                    -4
    ## 23                    -4
    ## 24                    -4
    ## 25                    -4
    ## 26                    -4
    ##    venue.timeZone.tz division.id
    ## 1                EDT          18
    ## 2                EDT          18
    ## 3                EDT          18
    ## 4                EDT          18
    ## 5                EDT          18
    ## 6                EDT          18
    ## 7                EDT          18
    ## 8                EDT          18
    ## 9                EDT          18
    ## 10               EDT          18
    ## 11               EDT          18
    ## 12               EDT          18
    ## 13               EDT          18
    ## 14               EDT          18
    ## 15               EDT          18
    ## 16               EDT          18
    ## 17               EDT          18
    ## 18               EDT          18
    ## 19               EDT          18
    ## 20               EDT          18
    ## 21               EDT          18
    ## 22               EDT          18
    ## 23               EDT          18
    ## 24               EDT          18
    ## 25               EDT          18
    ## 26               EDT          18
    ##    division.name division.nameShort
    ## 1   Metropolitan              Metro
    ## 2   Metropolitan              Metro
    ## 3   Metropolitan              Metro
    ## 4   Metropolitan              Metro
    ## 5   Metropolitan              Metro
    ## 6   Metropolitan              Metro
    ## 7   Metropolitan              Metro
    ## 8   Metropolitan              Metro
    ## 9   Metropolitan              Metro
    ## 10  Metropolitan              Metro
    ## 11  Metropolitan              Metro
    ## 12  Metropolitan              Metro
    ## 13  Metropolitan              Metro
    ## 14  Metropolitan              Metro
    ## 15  Metropolitan              Metro
    ## 16  Metropolitan              Metro
    ## 17  Metropolitan              Metro
    ## 18  Metropolitan              Metro
    ## 19  Metropolitan              Metro
    ## 20  Metropolitan              Metro
    ## 21  Metropolitan              Metro
    ## 22  Metropolitan              Metro
    ## 23  Metropolitan              Metro
    ## 24  Metropolitan              Metro
    ## 25  Metropolitan              Metro
    ## 26  Metropolitan              Metro
    ##           division.link
    ## 1  /api/v1/divisions/18
    ## 2  /api/v1/divisions/18
    ## 3  /api/v1/divisions/18
    ## 4  /api/v1/divisions/18
    ## 5  /api/v1/divisions/18
    ## 6  /api/v1/divisions/18
    ## 7  /api/v1/divisions/18
    ## 8  /api/v1/divisions/18
    ## 9  /api/v1/divisions/18
    ## 10 /api/v1/divisions/18
    ## 11 /api/v1/divisions/18
    ## 12 /api/v1/divisions/18
    ## 13 /api/v1/divisions/18
    ## 14 /api/v1/divisions/18
    ## 15 /api/v1/divisions/18
    ## 16 /api/v1/divisions/18
    ## 17 /api/v1/divisions/18
    ## 18 /api/v1/divisions/18
    ## 19 /api/v1/divisions/18
    ## 20 /api/v1/divisions/18
    ## 21 /api/v1/divisions/18
    ## 22 /api/v1/divisions/18
    ## 23 /api/v1/divisions/18
    ## 24 /api/v1/divisions/18
    ## 25 /api/v1/divisions/18
    ## 26 /api/v1/divisions/18
    ##    division.abbreviation conference.id
    ## 1                      M             6
    ## 2                      M             6
    ## 3                      M             6
    ## 4                      M             6
    ## 5                      M             6
    ## 6                      M             6
    ## 7                      M             6
    ## 8                      M             6
    ## 9                      M             6
    ## 10                     M             6
    ## 11                     M             6
    ## 12                     M             6
    ## 13                     M             6
    ## 14                     M             6
    ## 15                     M             6
    ## 16                     M             6
    ## 17                     M             6
    ## 18                     M             6
    ## 19                     M             6
    ## 20                     M             6
    ## 21                     M             6
    ## 22                     M             6
    ## 23                     M             6
    ## 24                     M             6
    ## 25                     M             6
    ## 26                     M             6
    ##    conference.name
    ## 1          Eastern
    ## 2          Eastern
    ## 3          Eastern
    ## 4          Eastern
    ## 5          Eastern
    ## 6          Eastern
    ## 7          Eastern
    ## 8          Eastern
    ## 9          Eastern
    ## 10         Eastern
    ## 11         Eastern
    ## 12         Eastern
    ## 13         Eastern
    ## 14         Eastern
    ## 15         Eastern
    ## 16         Eastern
    ## 17         Eastern
    ## 18         Eastern
    ## 19         Eastern
    ## 20         Eastern
    ## 21         Eastern
    ## 22         Eastern
    ## 23         Eastern
    ## 24         Eastern
    ## 25         Eastern
    ## 26         Eastern
    ##          conference.link
    ## 1  /api/v1/conferences/6
    ## 2  /api/v1/conferences/6
    ## 3  /api/v1/conferences/6
    ## 4  /api/v1/conferences/6
    ## 5  /api/v1/conferences/6
    ## 6  /api/v1/conferences/6
    ## 7  /api/v1/conferences/6
    ## 8  /api/v1/conferences/6
    ## 9  /api/v1/conferences/6
    ## 10 /api/v1/conferences/6
    ## 11 /api/v1/conferences/6
    ## 12 /api/v1/conferences/6
    ## 13 /api/v1/conferences/6
    ## 14 /api/v1/conferences/6
    ## 15 /api/v1/conferences/6
    ## 16 /api/v1/conferences/6
    ## 17 /api/v1/conferences/6
    ## 18 /api/v1/conferences/6
    ## 19 /api/v1/conferences/6
    ## 20 /api/v1/conferences/6
    ## 21 /api/v1/conferences/6
    ## 22 /api/v1/conferences/6
    ## 23 /api/v1/conferences/6
    ## 24 /api/v1/conferences/6
    ## 25 /api/v1/conferences/6
    ## 26 /api/v1/conferences/6
    ##    franchise.franchiseId
    ## 1                     23
    ## 2                     23
    ## 3                     23
    ## 4                     23
    ## 5                     23
    ## 6                     23
    ## 7                     23
    ## 8                     23
    ## 9                     23
    ## 10                    23
    ## 11                    23
    ## 12                    23
    ## 13                    23
    ## 14                    23
    ## 15                    23
    ## 16                    23
    ## 17                    23
    ## 18                    23
    ## 19                    23
    ## 20                    23
    ## 21                    23
    ## 22                    23
    ## 23                    23
    ## 24                    23
    ## 25                    23
    ## 26                    23
    ##    franchise.teamName
    ## 1              Devils
    ## 2              Devils
    ## 3              Devils
    ## 4              Devils
    ## 5              Devils
    ## 6              Devils
    ## 7              Devils
    ## 8              Devils
    ## 9              Devils
    ## 10             Devils
    ## 11             Devils
    ## 12             Devils
    ## 13             Devils
    ## 14             Devils
    ## 15             Devils
    ## 16             Devils
    ## 17             Devils
    ## 18             Devils
    ## 19             Devils
    ## 20             Devils
    ## 21             Devils
    ## 22             Devils
    ## 23             Devils
    ## 24             Devils
    ## 25             Devils
    ## 26             Devils
    ##           franchise.link
    ## 1  /api/v1/franchises/23
    ## 2  /api/v1/franchises/23
    ## 3  /api/v1/franchises/23
    ## 4  /api/v1/franchises/23
    ## 5  /api/v1/franchises/23
    ## 6  /api/v1/franchises/23
    ## 7  /api/v1/franchises/23
    ## 8  /api/v1/franchises/23
    ## 9  /api/v1/franchises/23
    ## 10 /api/v1/franchises/23
    ## 11 /api/v1/franchises/23
    ## 12 /api/v1/franchises/23
    ## 13 /api/v1/franchises/23
    ## 14 /api/v1/franchises/23
    ## 15 /api/v1/franchises/23
    ## 16 /api/v1/franchises/23
    ## 17 /api/v1/franchises/23
    ## 18 /api/v1/franchises/23
    ## 19 /api/v1/franchises/23
    ## 20 /api/v1/franchises/23
    ## 21 /api/v1/franchises/23
    ## 22 /api/v1/franchises/23
    ## 23 /api/v1/franchises/23
    ## 24 /api/v1/franchises/23
    ## 25 /api/v1/franchises/23
    ## 26 /api/v1/franchises/23
    ##  [ reached 'max' / getOption("max.print") -- omitted 935 rows ]

``` r
teamStats(modifier = "team.roster&season", seasonId = "20182019") 
```

    ##    jerseyNumber person.id
    ## 1             9   8470619
    ## 2            18   8471226
    ## 3            19   8471233
    ## 4             4   8472382
    ## 5             2   8473468
    ## 6            21   8473933
    ## 7            34   8474025
    ## 8            90   8475149
    ## 9            21   8475151
    ## 10           45   8475222
    ## 11           91   8475791
    ## 12           20   8475844
    ## 13           56   8476370
    ## 14           20   8476399
    ## 15           32   8476465
    ## 16           11   8476474
    ## 17           29   8476545
    ## 18           28   8476923
    ## 19            5   8476941
    ## 20            8   8477355
    ## 21           44   8477425
    ## 22            3   8477463
    ## 23           25   8477509
    ## 24           15   8477520
    ## 25           47   8477961
    ## 26           40   8477972
    ##       person.fullName
    ## 1         Brian Boyle
    ## 2       Drew Stafford
    ## 3        Travis Zajac
    ## 4         Andy Greene
    ## 5          Eric Gryba
    ## 6         Ben Lovejoy
    ## 7       Eric Tangradi
    ## 8    Marcus Johansson
    ## 9       Kyle Palmieri
    ## 10       Sami Vatanen
    ## 11        Taylor Hall
    ## 12     Kenny Agostino
    ## 13      Blake Pietila
    ## 14      Blake Coleman
    ## 15        Ryan Murphy
    ## 16      Stefan Noesen
    ## 17     Kurtis Gabriel
    ## 18     Damon Severson
    ## 19     Connor Carrick
    ## 20       Will Butcher
    ## 21         Miles Wood
    ## 22     Steven Santini
    ## 23      Mirco Mueller
    ## 24 Jean-Sebastien Dea
    ## 25   John Quenneville
    ## 26        Josh Jacobs
    ##               person.link
    ## 1  /api/v1/people/8470619
    ## 2  /api/v1/people/8471226
    ## 3  /api/v1/people/8471233
    ## 4  /api/v1/people/8472382
    ## 5  /api/v1/people/8473468
    ## 6  /api/v1/people/8473933
    ## 7  /api/v1/people/8474025
    ## 8  /api/v1/people/8475149
    ## 9  /api/v1/people/8475151
    ## 10 /api/v1/people/8475222
    ## 11 /api/v1/people/8475791
    ## 12 /api/v1/people/8475844
    ## 13 /api/v1/people/8476370
    ## 14 /api/v1/people/8476399
    ## 15 /api/v1/people/8476465
    ## 16 /api/v1/people/8476474
    ## 17 /api/v1/people/8476545
    ## 18 /api/v1/people/8476923
    ## 19 /api/v1/people/8476941
    ## 20 /api/v1/people/8477355
    ## 21 /api/v1/people/8477425
    ## 22 /api/v1/people/8477463
    ## 23 /api/v1/people/8477509
    ## 24 /api/v1/people/8477520
    ## 25 /api/v1/people/8477961
    ## 26 /api/v1/people/8477972
    ##    position.code position.name
    ## 1              C        Center
    ## 2              R    Right Wing
    ## 3              C        Center
    ## 4              D    Defenseman
    ## 5              D    Defenseman
    ## 6              D    Defenseman
    ## 7              L     Left Wing
    ## 8              L     Left Wing
    ## 9              R    Right Wing
    ## 10             D    Defenseman
    ## 11             L     Left Wing
    ## 12             L     Left Wing
    ## 13             L     Left Wing
    ## 14             C        Center
    ## 15             D    Defenseman
    ## 16             R    Right Wing
    ## 17             R    Right Wing
    ## 18             D    Defenseman
    ## 19             D    Defenseman
    ## 20             D    Defenseman
    ## 21             L     Left Wing
    ## 22             D    Defenseman
    ## 23             D    Defenseman
    ## 24             C        Center
    ## 25             C        Center
    ## 26             D    Defenseman
    ##    position.type position.abbreviation
    ## 1        Forward                     C
    ## 2        Forward                    RW
    ## 3        Forward                     C
    ## 4     Defenseman                     D
    ## 5     Defenseman                     D
    ## 6     Defenseman                     D
    ## 7        Forward                    LW
    ## 8        Forward                    LW
    ## 9        Forward                    RW
    ## 10    Defenseman                     D
    ## 11       Forward                    LW
    ## 12       Forward                    LW
    ## 13       Forward                    LW
    ## 14       Forward                     C
    ## 15    Defenseman                     D
    ## 16       Forward                    RW
    ## 17       Forward                    RW
    ## 18    Defenseman                     D
    ## 19    Defenseman                     D
    ## 20    Defenseman                     D
    ## 21       Forward                    LW
    ## 22    Defenseman                     D
    ## 23    Defenseman                     D
    ## 24       Forward                     C
    ## 25       Forward                     C
    ## 26    Defenseman                     D
    ##                 name id
    ## 1  New Jersey Devils  1
    ## 2  New Jersey Devils  1
    ## 3  New Jersey Devils  1
    ## 4  New Jersey Devils  1
    ## 5  New Jersey Devils  1
    ## 6  New Jersey Devils  1
    ## 7  New Jersey Devils  1
    ## 8  New Jersey Devils  1
    ## 9  New Jersey Devils  1
    ## 10 New Jersey Devils  1
    ## 11 New Jersey Devils  1
    ## 12 New Jersey Devils  1
    ## 13 New Jersey Devils  1
    ## 14 New Jersey Devils  1
    ## 15 New Jersey Devils  1
    ## 16 New Jersey Devils  1
    ## 17 New Jersey Devils  1
    ## 18 New Jersey Devils  1
    ## 19 New Jersey Devils  1
    ## 20 New Jersey Devils  1
    ## 21 New Jersey Devils  1
    ## 22 New Jersey Devils  1
    ## 23 New Jersey Devils  1
    ## 24 New Jersey Devils  1
    ## 25 New Jersey Devils  1
    ## 26 New Jersey Devils  1
    ##               link abbreviation
    ## 1  /api/v1/teams/1          NJD
    ## 2  /api/v1/teams/1          NJD
    ## 3  /api/v1/teams/1          NJD
    ## 4  /api/v1/teams/1          NJD
    ## 5  /api/v1/teams/1          NJD
    ## 6  /api/v1/teams/1          NJD
    ## 7  /api/v1/teams/1          NJD
    ## 8  /api/v1/teams/1          NJD
    ## 9  /api/v1/teams/1          NJD
    ## 10 /api/v1/teams/1          NJD
    ## 11 /api/v1/teams/1          NJD
    ## 12 /api/v1/teams/1          NJD
    ## 13 /api/v1/teams/1          NJD
    ## 14 /api/v1/teams/1          NJD
    ## 15 /api/v1/teams/1          NJD
    ## 16 /api/v1/teams/1          NJD
    ## 17 /api/v1/teams/1          NJD
    ## 18 /api/v1/teams/1          NJD
    ## 19 /api/v1/teams/1          NJD
    ## 20 /api/v1/teams/1          NJD
    ## 21 /api/v1/teams/1          NJD
    ## 22 /api/v1/teams/1          NJD
    ## 23 /api/v1/teams/1          NJD
    ## 24 /api/v1/teams/1          NJD
    ## 25 /api/v1/teams/1          NJD
    ## 26 /api/v1/teams/1          NJD
    ##    teamName locationName
    ## 1    Devils   New Jersey
    ## 2    Devils   New Jersey
    ## 3    Devils   New Jersey
    ## 4    Devils   New Jersey
    ## 5    Devils   New Jersey
    ## 6    Devils   New Jersey
    ## 7    Devils   New Jersey
    ## 8    Devils   New Jersey
    ## 9    Devils   New Jersey
    ## 10   Devils   New Jersey
    ## 11   Devils   New Jersey
    ## 12   Devils   New Jersey
    ## 13   Devils   New Jersey
    ## 14   Devils   New Jersey
    ## 15   Devils   New Jersey
    ## 16   Devils   New Jersey
    ## 17   Devils   New Jersey
    ## 18   Devils   New Jersey
    ## 19   Devils   New Jersey
    ## 20   Devils   New Jersey
    ## 21   Devils   New Jersey
    ## 22   Devils   New Jersey
    ## 23   Devils   New Jersey
    ## 24   Devils   New Jersey
    ## 25   Devils   New Jersey
    ## 26   Devils   New Jersey
    ##    firstYearOfPlay  shortName
    ## 1             1982 New Jersey
    ## 2             1982 New Jersey
    ## 3             1982 New Jersey
    ## 4             1982 New Jersey
    ## 5             1982 New Jersey
    ## 6             1982 New Jersey
    ## 7             1982 New Jersey
    ## 8             1982 New Jersey
    ## 9             1982 New Jersey
    ## 10            1982 New Jersey
    ## 11            1982 New Jersey
    ## 12            1982 New Jersey
    ## 13            1982 New Jersey
    ## 14            1982 New Jersey
    ## 15            1982 New Jersey
    ## 16            1982 New Jersey
    ## 17            1982 New Jersey
    ## 18            1982 New Jersey
    ## 19            1982 New Jersey
    ## 20            1982 New Jersey
    ## 21            1982 New Jersey
    ## 22            1982 New Jersey
    ## 23            1982 New Jersey
    ## 24            1982 New Jersey
    ## 25            1982 New Jersey
    ## 26            1982 New Jersey
    ##                    officialSiteUrl
    ## 1  http://www.newjerseydevils.com/
    ## 2  http://www.newjerseydevils.com/
    ## 3  http://www.newjerseydevils.com/
    ## 4  http://www.newjerseydevils.com/
    ## 5  http://www.newjerseydevils.com/
    ## 6  http://www.newjerseydevils.com/
    ## 7  http://www.newjerseydevils.com/
    ## 8  http://www.newjerseydevils.com/
    ## 9  http://www.newjerseydevils.com/
    ## 10 http://www.newjerseydevils.com/
    ## 11 http://www.newjerseydevils.com/
    ## 12 http://www.newjerseydevils.com/
    ## 13 http://www.newjerseydevils.com/
    ## 14 http://www.newjerseydevils.com/
    ## 15 http://www.newjerseydevils.com/
    ## 16 http://www.newjerseydevils.com/
    ## 17 http://www.newjerseydevils.com/
    ## 18 http://www.newjerseydevils.com/
    ## 19 http://www.newjerseydevils.com/
    ## 20 http://www.newjerseydevils.com/
    ## 21 http://www.newjerseydevils.com/
    ## 22 http://www.newjerseydevils.com/
    ## 23 http://www.newjerseydevils.com/
    ## 24 http://www.newjerseydevils.com/
    ## 25 http://www.newjerseydevils.com/
    ## 26 http://www.newjerseydevils.com/
    ##    franchiseId active
    ## 1           23   TRUE
    ## 2           23   TRUE
    ## 3           23   TRUE
    ## 4           23   TRUE
    ## 5           23   TRUE
    ## 6           23   TRUE
    ## 7           23   TRUE
    ## 8           23   TRUE
    ## 9           23   TRUE
    ## 10          23   TRUE
    ## 11          23   TRUE
    ## 12          23   TRUE
    ## 13          23   TRUE
    ## 14          23   TRUE
    ## 15          23   TRUE
    ## 16          23   TRUE
    ## 17          23   TRUE
    ## 18          23   TRUE
    ## 19          23   TRUE
    ## 20          23   TRUE
    ## 21          23   TRUE
    ## 22          23   TRUE
    ## 23          23   TRUE
    ## 24          23   TRUE
    ## 25          23   TRUE
    ## 26          23   TRUE
    ##           venue.name
    ## 1  Prudential Center
    ## 2  Prudential Center
    ## 3  Prudential Center
    ## 4  Prudential Center
    ## 5  Prudential Center
    ## 6  Prudential Center
    ## 7  Prudential Center
    ## 8  Prudential Center
    ## 9  Prudential Center
    ## 10 Prudential Center
    ## 11 Prudential Center
    ## 12 Prudential Center
    ## 13 Prudential Center
    ## 14 Prudential Center
    ## 15 Prudential Center
    ## 16 Prudential Center
    ## 17 Prudential Center
    ## 18 Prudential Center
    ## 19 Prudential Center
    ## 20 Prudential Center
    ## 21 Prudential Center
    ## 22 Prudential Center
    ## 23 Prudential Center
    ## 24 Prudential Center
    ## 25 Prudential Center
    ## 26 Prudential Center
    ##             venue.link venue.city
    ## 1  /api/v1/venues/null     Newark
    ## 2  /api/v1/venues/null     Newark
    ## 3  /api/v1/venues/null     Newark
    ## 4  /api/v1/venues/null     Newark
    ## 5  /api/v1/venues/null     Newark
    ## 6  /api/v1/venues/null     Newark
    ## 7  /api/v1/venues/null     Newark
    ## 8  /api/v1/venues/null     Newark
    ## 9  /api/v1/venues/null     Newark
    ## 10 /api/v1/venues/null     Newark
    ## 11 /api/v1/venues/null     Newark
    ## 12 /api/v1/venues/null     Newark
    ## 13 /api/v1/venues/null     Newark
    ## 14 /api/v1/venues/null     Newark
    ## 15 /api/v1/venues/null     Newark
    ## 16 /api/v1/venues/null     Newark
    ## 17 /api/v1/venues/null     Newark
    ## 18 /api/v1/venues/null     Newark
    ## 19 /api/v1/venues/null     Newark
    ## 20 /api/v1/venues/null     Newark
    ## 21 /api/v1/venues/null     Newark
    ## 22 /api/v1/venues/null     Newark
    ## 23 /api/v1/venues/null     Newark
    ## 24 /api/v1/venues/null     Newark
    ## 25 /api/v1/venues/null     Newark
    ## 26 /api/v1/venues/null     Newark
    ##    venue.id venue.timeZone.id
    ## 1        NA  America/New_York
    ## 2        NA  America/New_York
    ## 3        NA  America/New_York
    ## 4        NA  America/New_York
    ## 5        NA  America/New_York
    ## 6        NA  America/New_York
    ## 7        NA  America/New_York
    ## 8        NA  America/New_York
    ## 9        NA  America/New_York
    ## 10       NA  America/New_York
    ## 11       NA  America/New_York
    ## 12       NA  America/New_York
    ## 13       NA  America/New_York
    ## 14       NA  America/New_York
    ## 15       NA  America/New_York
    ## 16       NA  America/New_York
    ## 17       NA  America/New_York
    ## 18       NA  America/New_York
    ## 19       NA  America/New_York
    ## 20       NA  America/New_York
    ## 21       NA  America/New_York
    ## 22       NA  America/New_York
    ## 23       NA  America/New_York
    ## 24       NA  America/New_York
    ## 25       NA  America/New_York
    ## 26       NA  America/New_York
    ##    venue.timeZone.offset
    ## 1                     -4
    ## 2                     -4
    ## 3                     -4
    ## 4                     -4
    ## 5                     -4
    ## 6                     -4
    ## 7                     -4
    ## 8                     -4
    ## 9                     -4
    ## 10                    -4
    ## 11                    -4
    ## 12                    -4
    ## 13                    -4
    ## 14                    -4
    ## 15                    -4
    ## 16                    -4
    ## 17                    -4
    ## 18                    -4
    ## 19                    -4
    ## 20                    -4
    ## 21                    -4
    ## 22                    -4
    ## 23                    -4
    ## 24                    -4
    ## 25                    -4
    ## 26                    -4
    ##    venue.timeZone.tz division.id
    ## 1                EDT          18
    ## 2                EDT          18
    ## 3                EDT          18
    ## 4                EDT          18
    ## 5                EDT          18
    ## 6                EDT          18
    ## 7                EDT          18
    ## 8                EDT          18
    ## 9                EDT          18
    ## 10               EDT          18
    ## 11               EDT          18
    ## 12               EDT          18
    ## 13               EDT          18
    ## 14               EDT          18
    ## 15               EDT          18
    ## 16               EDT          18
    ## 17               EDT          18
    ## 18               EDT          18
    ## 19               EDT          18
    ## 20               EDT          18
    ## 21               EDT          18
    ## 22               EDT          18
    ## 23               EDT          18
    ## 24               EDT          18
    ## 25               EDT          18
    ## 26               EDT          18
    ##    division.name division.nameShort
    ## 1   Metropolitan              Metro
    ## 2   Metropolitan              Metro
    ## 3   Metropolitan              Metro
    ## 4   Metropolitan              Metro
    ## 5   Metropolitan              Metro
    ## 6   Metropolitan              Metro
    ## 7   Metropolitan              Metro
    ## 8   Metropolitan              Metro
    ## 9   Metropolitan              Metro
    ## 10  Metropolitan              Metro
    ## 11  Metropolitan              Metro
    ## 12  Metropolitan              Metro
    ## 13  Metropolitan              Metro
    ## 14  Metropolitan              Metro
    ## 15  Metropolitan              Metro
    ## 16  Metropolitan              Metro
    ## 17  Metropolitan              Metro
    ## 18  Metropolitan              Metro
    ## 19  Metropolitan              Metro
    ## 20  Metropolitan              Metro
    ## 21  Metropolitan              Metro
    ## 22  Metropolitan              Metro
    ## 23  Metropolitan              Metro
    ## 24  Metropolitan              Metro
    ## 25  Metropolitan              Metro
    ## 26  Metropolitan              Metro
    ##           division.link
    ## 1  /api/v1/divisions/18
    ## 2  /api/v1/divisions/18
    ## 3  /api/v1/divisions/18
    ## 4  /api/v1/divisions/18
    ## 5  /api/v1/divisions/18
    ## 6  /api/v1/divisions/18
    ## 7  /api/v1/divisions/18
    ## 8  /api/v1/divisions/18
    ## 9  /api/v1/divisions/18
    ## 10 /api/v1/divisions/18
    ## 11 /api/v1/divisions/18
    ## 12 /api/v1/divisions/18
    ## 13 /api/v1/divisions/18
    ## 14 /api/v1/divisions/18
    ## 15 /api/v1/divisions/18
    ## 16 /api/v1/divisions/18
    ## 17 /api/v1/divisions/18
    ## 18 /api/v1/divisions/18
    ## 19 /api/v1/divisions/18
    ## 20 /api/v1/divisions/18
    ## 21 /api/v1/divisions/18
    ## 22 /api/v1/divisions/18
    ## 23 /api/v1/divisions/18
    ## 24 /api/v1/divisions/18
    ## 25 /api/v1/divisions/18
    ## 26 /api/v1/divisions/18
    ##    division.abbreviation conference.id
    ## 1                      M             6
    ## 2                      M             6
    ## 3                      M             6
    ## 4                      M             6
    ## 5                      M             6
    ## 6                      M             6
    ## 7                      M             6
    ## 8                      M             6
    ## 9                      M             6
    ## 10                     M             6
    ## 11                     M             6
    ## 12                     M             6
    ## 13                     M             6
    ## 14                     M             6
    ## 15                     M             6
    ## 16                     M             6
    ## 17                     M             6
    ## 18                     M             6
    ## 19                     M             6
    ## 20                     M             6
    ## 21                     M             6
    ## 22                     M             6
    ## 23                     M             6
    ## 24                     M             6
    ## 25                     M             6
    ## 26                     M             6
    ##    conference.name
    ## 1          Eastern
    ## 2          Eastern
    ## 3          Eastern
    ## 4          Eastern
    ## 5          Eastern
    ## 6          Eastern
    ## 7          Eastern
    ## 8          Eastern
    ## 9          Eastern
    ## 10         Eastern
    ## 11         Eastern
    ## 12         Eastern
    ## 13         Eastern
    ## 14         Eastern
    ## 15         Eastern
    ## 16         Eastern
    ## 17         Eastern
    ## 18         Eastern
    ## 19         Eastern
    ## 20         Eastern
    ## 21         Eastern
    ## 22         Eastern
    ## 23         Eastern
    ## 24         Eastern
    ## 25         Eastern
    ## 26         Eastern
    ##          conference.link
    ## 1  /api/v1/conferences/6
    ## 2  /api/v1/conferences/6
    ## 3  /api/v1/conferences/6
    ## 4  /api/v1/conferences/6
    ## 5  /api/v1/conferences/6
    ## 6  /api/v1/conferences/6
    ## 7  /api/v1/conferences/6
    ## 8  /api/v1/conferences/6
    ## 9  /api/v1/conferences/6
    ## 10 /api/v1/conferences/6
    ## 11 /api/v1/conferences/6
    ## 12 /api/v1/conferences/6
    ## 13 /api/v1/conferences/6
    ## 14 /api/v1/conferences/6
    ## 15 /api/v1/conferences/6
    ## 16 /api/v1/conferences/6
    ## 17 /api/v1/conferences/6
    ## 18 /api/v1/conferences/6
    ## 19 /api/v1/conferences/6
    ## 20 /api/v1/conferences/6
    ## 21 /api/v1/conferences/6
    ## 22 /api/v1/conferences/6
    ## 23 /api/v1/conferences/6
    ## 24 /api/v1/conferences/6
    ## 25 /api/v1/conferences/6
    ## 26 /api/v1/conferences/6
    ##    franchise.franchiseId
    ## 1                     23
    ## 2                     23
    ## 3                     23
    ## 4                     23
    ## 5                     23
    ## 6                     23
    ## 7                     23
    ## 8                     23
    ## 9                     23
    ## 10                    23
    ## 11                    23
    ## 12                    23
    ## 13                    23
    ## 14                    23
    ## 15                    23
    ## 16                    23
    ## 17                    23
    ## 18                    23
    ## 19                    23
    ## 20                    23
    ## 21                    23
    ## 22                    23
    ## 23                    23
    ## 24                    23
    ## 25                    23
    ## 26                    23
    ##    franchise.teamName
    ## 1              Devils
    ## 2              Devils
    ## 3              Devils
    ## 4              Devils
    ## 5              Devils
    ## 6              Devils
    ## 7              Devils
    ## 8              Devils
    ## 9              Devils
    ## 10             Devils
    ## 11             Devils
    ## 12             Devils
    ## 13             Devils
    ## 14             Devils
    ## 15             Devils
    ## 16             Devils
    ## 17             Devils
    ## 18             Devils
    ## 19             Devils
    ## 20             Devils
    ## 21             Devils
    ## 22             Devils
    ## 23             Devils
    ## 24             Devils
    ## 25             Devils
    ## 26             Devils
    ##           franchise.link
    ## 1  /api/v1/franchises/23
    ## 2  /api/v1/franchises/23
    ## 3  /api/v1/franchises/23
    ## 4  /api/v1/franchises/23
    ## 5  /api/v1/franchises/23
    ## 6  /api/v1/franchises/23
    ## 7  /api/v1/franchises/23
    ## 8  /api/v1/franchises/23
    ## 9  /api/v1/franchises/23
    ## 10 /api/v1/franchises/23
    ## 11 /api/v1/franchises/23
    ## 12 /api/v1/franchises/23
    ## 13 /api/v1/franchises/23
    ## 14 /api/v1/franchises/23
    ## 15 /api/v1/franchises/23
    ## 16 /api/v1/franchises/23
    ## 17 /api/v1/franchises/23
    ## 18 /api/v1/franchises/23
    ## 19 /api/v1/franchises/23
    ## 20 /api/v1/franchises/23
    ## 21 /api/v1/franchises/23
    ## 22 /api/v1/franchises/23
    ## 23 /api/v1/franchises/23
    ## 24 /api/v1/franchises/23
    ## 25 /api/v1/franchises/23
    ## 26 /api/v1/franchises/23
    ##               roster.link
    ## 1  /api/v1/teams/1/roster
    ## 2  /api/v1/teams/1/roster
    ## 3  /api/v1/teams/1/roster
    ## 4  /api/v1/teams/1/roster
    ## 5  /api/v1/teams/1/roster
    ## 6  /api/v1/teams/1/roster
    ## 7  /api/v1/teams/1/roster
    ## 8  /api/v1/teams/1/roster
    ## 9  /api/v1/teams/1/roster
    ## 10 /api/v1/teams/1/roster
    ## 11 /api/v1/teams/1/roster
    ## 12 /api/v1/teams/1/roster
    ## 13 /api/v1/teams/1/roster
    ## 14 /api/v1/teams/1/roster
    ## 15 /api/v1/teams/1/roster
    ## 16 /api/v1/teams/1/roster
    ## 17 /api/v1/teams/1/roster
    ## 18 /api/v1/teams/1/roster
    ## 19 /api/v1/teams/1/roster
    ## 20 /api/v1/teams/1/roster
    ## 21 /api/v1/teams/1/roster
    ## 22 /api/v1/teams/1/roster
    ## 23 /api/v1/teams/1/roster
    ## 24 /api/v1/teams/1/roster
    ## 25 /api/v1/teams/1/roster
    ## 26 /api/v1/teams/1/roster
    ##  [ reached 'max' / getOption("max.print") -- omitted 1073 rows ]

``` r
getStatsTable(modifier = "teamId", teamIds = "1,2,3")
```

    ## # A tibble: 3 x 29
    ##      id name  link  abbreviation
    ##   <int> <chr> <chr> <chr>       
    ## 1     1 New ~ /api~ NJD         
    ## 2     2 New ~ /api~ NYI         
    ## 3     3 New ~ /api~ NYR         
    ## # ... with 25 more variables:
    ## #   teamName <chr>,
    ## #   locationName <chr>,
    ## #   firstYearOfPlay <chr>,
    ## #   shortName <chr>,
    ## #   officialSiteUrl <chr>,
    ## #   franchiseId <int>, active <lgl>,
    ## #   venue.name <chr>,
    ## #   venue.link <chr>,
    ## #   venue.city <chr>, venue.id <int>,
    ## #   venue.timeZone.id <chr>,
    ## #   venue.timeZone.offset <int>,
    ## #   venue.timeZone.tz <chr>,
    ## #   division.id <int>,
    ## #   division.name <chr>,
    ## #   division.nameShort <chr>,
    ## #   division.link <chr>,
    ## #   division.abbreviation <chr>,
    ## #   conference.id <int>,
    ## #   conference.name <chr>,
    ## #   conference.link <chr>,
    ## #   franchise.franchiseId <int>,
    ## #   franchise.teamName <chr>,
    ## #   franchise.link <chr>
