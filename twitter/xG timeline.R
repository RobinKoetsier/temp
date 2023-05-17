library(ggbraid)
library(tidyverse)
library(ggtext)
TEAM = "Ajax"

with_date <- readxl::read_excel("Export_TDL_NED_2223.xlsx", 
                   sheet = "Shots") %>%
  mutate(season = "22/23") %>%
  select(-date) %>%
  left_join(readxl::read_excel("Export_TDL_NED_2223.xlsx", 
                               sheet = "Wedstrijden") %>%
              mutate(season = "22/23") %>%
              mutate(match = glue::glue("{HomeName} - {AwayName}"),
                     date = as.Date(date,"%d-%m-%Y"))) %>%
  separate(match, c("Home", "Away"), " - ") %>%
  select(date,name, Home,Away,xG, Type_of_play,season) %>%
  mutate(opp = ifelse(name == Home, Away, Home)) %>%
  rbind(with_date <- readxl::read_excel("Export_TDL_NED_2122.xlsx", 
                                        sheet = "Shots") %>%
          mutate(season = "21/22") %>%
          select(-date) %>%
          left_join(readxl::read_excel("Export_TDL_NED_2122.xlsx", 
                                       sheet = "Wedstrijden") %>%
                      mutate(season = "21/22") %>%
                      mutate(match = glue::glue("{HomeName} - {AwayName}"),
                             date = as.Date(date,"%d-%m-%Y"))) %>%
          separate(match, c("Home", "Away"), " - ") %>%
          select(date,name, Home,Away,xG, Type_of_play, season) %>%
          mutate(opp = ifelse(name == Home, Away, Home))) %>%
  rbind(with_date <- readxl::read_excel("Export_TDL_NED_2021.xlsx", 
                                        sheet = "Shots") %>%
          mutate(season = "20/21") %>%
          select(-date) %>%
          left_join(readxl::read_excel("Export_TDL_NED_2021.xlsx", 
                                       sheet = "Wedstrijden") %>%
                      mutate(match = glue::glue("{HomeName} - {AwayName}"),
                             date = as.Date(date,"%d-%m-%Y"))) %>%
          separate(match, c("Home", "Away"), " - ") %>%
          select(date,name, Home,Away,xG, Type_of_play,season) %>%
          mutate(opp = ifelse(name == Home, Away, Home))) %>%
  
  rbind(readxl::read_excel("Export_TDL_NED_1920.xlsx", 
                                        sheet = "Shots") %>%
          mutate(season = "19/20") %>%
          select(-date) %>%
          left_join(readxl::read_excel("Export_TDL_NED_1920.xlsx", 
                                       sheet = "Wedstrijden") %>%
                      mutate(season = "19/20") %>%
                      mutate(match = glue::glue("{HomeName} - {AwayName}"),
                             date = as.Date(date,"%d-%m-%Y"))) %>%
          separate(match, c("Home", "Away"), " - ") %>%
          select(date,name, Home,Away,xG, Type_of_play,season) %>%
          mutate(opp = ifelse(name == Home, Away, Home))) %>%
  rbind(readxl::read_excel("Export_TDL_NED_1819.xlsx", 
                           sheet = "Shots") %>%
          mutate(season = "18/19") %>%
          select(-date) %>%
          left_join(readxl::read_excel("Export_TDL_NED_1819.xlsx", 
                                       sheet = "Wedstrijden") %>%
                      mutate(season = "18/19") %>%
                      mutate(match = glue::glue("{HomeName} - {AwayName}"),
                             date = as.Date(date,"%d-%m-%Y"))) %>%
          separate(match, c("Home", "Away"), " - ") %>%
          select(date,name, Home,Away,xG, Type_of_play,season) %>%
          mutate(opp = ifelse(name == Home, Away, Home))) 



df <- with_date %>%
  filter(Type_of_play != "Penalty") %>%
  filter(name == TEAM) %>%
  group_by(name,date) %>%
  summarise(xG = sum(xG)) %>%
  left_join(with_date %>%
              filter(Type_of_play != "Penalty") %>%
              filter(opp == TEAM) %>%
              group_by(opp,date,season) %>%
              summarise(xGA = sum(xG)),by="date") %>%
  mutate(round = c(1:34,1:24,1:34,1:34,1:32),
         xGA = ifelse(row_number() > 5,replace_na(xGA,0),xGA),
  #mutate(round = c(1:147),
         roll = zoo::rollmean(xG,5, fill=NA,align="right"),
         rollA = zoo::rollmean(xGA,5, fill=NA,align="right")) 

df1 <- 
  df %>%
  group_by(season) %>%
  summarise(xG = mean(xG),
            xGA = mean(xGA),
            diff = xG-xGA) %>%
  mutate(diff = ifelse(season == "18/19", glue::glue("Average xG difference: {round(diff,2)}"),round(diff,2)))

  df %>%
  ggplot() +
  geom_line(aes(round,roll),colour="#69C5D2") +
  ggbraid::geom_braid(aes(round, ymin = roll, ymax = rollA, fill = rollA < roll), alpha = 0.6) +
  geom_line(aes(round,rollA),colour="#D75998") +
  geom_point(aes(round,rollA),colour="#D75998",size=.75,shape=21,fill="#120E41") +
    geom_point(aes(round,roll),colour="#69C5D2",size=.75,shape=21,fill="#120E41") +
  scale_fill_manual(values=c("TRUE" = "#69C5D2","FALSE" = "#D75998"),
                    labels = c("TRUE" = "xG > xGA", "FALSE" ="xG < xGA"))+
  scale_x_continuous(breaks= c(0,10,20,30)) +
    
    geom_text(data = df1, 
              aes(label= diff
                              ,x=2,y=3.2), colour="white", family = "Spartan-Bold",size=2.5,hjust=0)+
    
  facet_grid(.~season,scales = "free_x",space="free") +
  labs(y="xG",
       x="match day",
       fill = "xG Difference",
       title = glue::glue("Non Penalty Expected Goals Timeline {df$name[1]}"),
       subtitle = glue::glue("5 game rolling average of <i style='color:#69C5D2'>Expected Goals</i> and <i style='color:#D75998'>Expected Goals Against</i>"),
       caption = "@RobinWilhelmus") +
  #coord_cartesian(ylim = c(0,4)) +/i
  themes::theme_twitter()

ggsave(glue::glue("timeline_{df$name[1]}.png"),width=12,height=5,device=png)









