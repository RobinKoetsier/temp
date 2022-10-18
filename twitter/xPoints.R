library(tidyverse)
library(worldfootballR)
# Only fill in the comp_id
leagues <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/fotmob-leagues/all_leagues.csv")

comp_id = 40
league_matches <- fotmob_get_league_matches(
 league_id = comp_id
) 

#filter out matches in the future
league_matches <- league_matches %>%
  filter(league_matches$status$reason$short == "FT") 

#get matches for those id's
match_details <- fotmob_get_match_details(league_matches$id)

# unnest the shots only needed in older versions of worldfootballR so commented it out) 
# and add team name of team shooting and team conceding (
shots_temp <- match_details %>%
  #unnest(shots) %>%
  mutate(team_name = case_when(team_id == home_team_id ~ home_team,
                               team_id == away_team_id ~ away_team),
         opponent = case_when(team_id == home_team_id ~ away_team,
                              team_id == away_team_id ~ home_team))


xG_table <- shots_temp %>%
  mutate(expected_goals = replace_na(expected_goals,0)) %>% #replace the NA for own goals to 0
  group_by(team_name) %>%
  summarise(xGF = sum(expected_goals)) %>% #xG for
  left_join(shots_temp %>% #join with xG against
              mutate(expected_goals = replace_na(expected_goals,0)) %>%
              group_by(opponent) %>%
              summarise(xGA = sum(expected_goals)),by=c("team_name" = "opponent"))
head(xG_table)

calculateChance<-function(team1,team2,p){
  
  home = 0
  away = 0
  draw = 0
  homeP = 0
  awayP = 0
  drawP = 0
  
  for(i in 1:p){
    matchWinner <- calculateWinner(team1,team2)
    
    if(matchWinner == "home"){
      home <- home+1
      homeP <- homeP+3
    }else if(matchWinner == "away"){
      
      away <- away+1
      awayP <- awayP+3
    }else{
      draw <- draw +1
      awayP <- awayP+1
      homeP <- homeP+1
    }
  }
  
  home = paste0(home/(p/100),"%")
  away = paste0(away/(p/100),"%")
  draw = paste0(draw/(p/100),"%")
  homeP = homeP/p
  awayP = awayP/p
  
  chances <- paste0("Home win: ",home,"% | Draw: ",draw,"% | Away win: ",away,"%")
  game <- data.frame(home,draw,away,homeP,awayP)
  return(game)
}

# function that returns if a shot becomes a goal and counts the goals
testShots<-function(XG){
  Goals = 0
  XG[is.na(XG)] <- 0
  for(i in 1:length(XG)){
    if(runif(1, 0.0, 1.0)<=XG[i]){
      
      Goals <- Goals + 1
    }else{
      
    }
  }
  
  return(Goals)
}  

# function that calculates the winner by comparing the number of goals of the two teams
calculateWinner <- function(home,away){
  HomeGoals = 0
  AwayGoals = 0
  
  HomeGoals <- testShots(home)
  AwayGoals <- testShots(away)
  
  #diffTemp <- (HomeGoals - AwayGoals)
  
  #diff <- append(diff,diffTemp)
  if(HomeGoals > AwayGoals){
    
    return("home")
  }else if(AwayGoals > HomeGoals){
    
    return("away")
  }else{
    
    return("draw")
  }
}

plot_func <- function(df){
  calculateChance(pull(df %>% filter(team_id == home_team_id),expected_goals),
                  pull(df %>% filter(team_id == away_team_id),expected_goals),
                  10000)
}



df <- shots_temp %>%
  group_by(match_id) %>%
  nest() %>%
  mutate(result = map(data, plot_func)) %>%
  ungroup() %>%
  unnest(result)
head(df)
total_df <- df %>%
  select(match_id,homeP,awayP) %>%
  left_join(match_details %>% 
              group_by(match_id,home_team,away_team) %>% 
              nest()) 
# if you unnested the match_details earlier, you can just join by 'match_id'

# and sum all the xPoints per team
xpoints <- total_df %>%
  group_by(home_team) %>%
  summarise(pointsH = sum(homeP)) %>%
  left_join(total_df %>%
              group_by(away_team) %>%
              summarise(pointsA = sum(awayP)),by =c("home_team"="away_team")) %>%
  mutate(xPoints = pointsH + pointsA)
head(xpoints)


safely_from_json <- purrr::safely(jsonlite::fromJSON, otherwise = NULL, quiet = TRUE)
jsonn <- safely_from_json(glue::glue("https://www.fotmob.com/api/leagues?id={comp_id}"))
table <- data.frame(jsonn$result$table$data$table$all)
xptable <- table %>%
  left_join(xpoints, by=c("name" = "home_team")) %>%
  separate(scoresStr, c("GF", "GA"),"-") %>%
  mutate(GF = as.numeric(GF),
         GA = as.numeric(GA),
         GD = GF - GA) %>%
  
  select(idx,id, name, played, wins,draws,losses,GF,GA,GD,pts,xPoints) %>%
  arrange(-xPoints) %>%
  mutate(xRank = c(1:length(table$name))) %>%
  left_join(xG_table, by = c("name" = "team_name")) %>%
  mutate(xGD = xGF - xGA)
xptable <- xptable %>%
  select(idx,id,name,played,wins,draws,losses,GF,GA,GD,pts,xGF,xGA,xGD,xPoints,xRank) %>%
  `colnames<-`(c("RANK","id", "TEAM", "P","W","D","L","GF","GA","GD","PTS","xGF","xGA","xGD","xPTS","xRANK"))

xptable <- xptable %>%
  mutate(id = glue::glue("https://images.fotmob.com/image_resources/logo/teamlogo/{id}.png"))

library(gt)

add_rank_color <- function(col1,col2){
  add_color <- if (col1 < col2) {
    "background:#61B861;"
  } else if (col1>col2) {
    "background:#FC785F;"
  } else if (col1 == col2) {
    "background:#FDD297;"
  }
  div_out <- htmltools::div(
    style = paste(
      "width: 20px;
  height: 20px;
  border: 1px solid rgba(0, 0, 0, 0.03);
  border-radius: 50%;
text-align: center;
  align-item: right;
   margin-left: 15px;
#  color: #000;
  font-size: 13px;
      font-weight:bold;",
      add_color
    ),col1
  )
  
  as.character(div_out) %>% 
    gt::html()
}

library(gtExtras)
xptable %>%
  mutate(
    RANK_temp = RANK,
    RANK = map2(RANK, xRANK, add_rank_color),
    xRANK = map2(xRANK, RANK_temp, add_rank_color)
  ) %>%
  select(-RANK_temp) %>%
  gt() %>%
  gt_img_rows(columns = id, img_source = "web", height = 17) %>%
  cols_label(
    id = " "
  ) %>%
  fmt_number(
    columns = c(xGF,xGA,xGD,xPTS),
    decimals = 1
  ) %>%
  cols_align(
    align = "center",
    columns = c(P:xRANK)
  ) %>%
  tab_style(
    style = list(
      
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
      
    ),
    locations = list(
      cells_body(
        columns = c(P,xGF)
      )
    )
  ) %>%
  tab_spanner(
    label = "LEAGUE TABLE",
    columns = c(
      RANK:PTS
    )
  ) %>%
  tab_spanner(
    label = "EXPECTED TABLE",
    columns = c(
      xGF:xRANK
    )
  )  %>%
  cols_width(
    c(xGF:xPTS) ~ px(60),
    #c(RANK,xRANK) ~ px(30),
    TEAM ~ 150,
    everything() ~ px(50)
  ) %>% tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      cell_borders(sides = "bottom", weight = px(3)),
      cell_text(weight = "bold")
    )
  ) %>% tab_style(
    locations = list(
      cells_body(
        columns = c(PTS,xPTS)
      )
    ),
    style = list(
      cell_text(weight = "bold")
    )
  ) %>%
  opt_table_font(font = "Roboto Mono") %>%
  tab_options(
    row.striping.background_color = "#F6F8FA",
    row.striping.include_table_body = TRUE,
    data_row.padding = px(2),
    table.border.top.style = "hidden",
    #table.border.bottom.style = "hidden",
    table.font.size = "12px"
  ) %>%
  tab_header(md("**LEAGUE TABLE BASED ON EXPECTED POINTS**")) %>%
  tab_source_note(
    source_note = "xPoints calculated by simulating every shot in a match"
  )%>%
  tab_source_note(
    source_note = "Data from Opta via FotMob"
  )




