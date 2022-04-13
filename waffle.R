library(tidyverse)
library(data.table)
library(hrbrthemes)
library(waffle)
dfxgb <- rbindlist(xGBlist) %>% filter(penalty == "no") %>%
  filter(Goal == 1)  %>% select(PlayerId,TeamId,match,grp,xG,Goal,shot,assist)

dfxgb <- unique(dfxgb) %>% group_by(PlayerId,TeamId) %>% summarise(betrokken = n())

df1 <- sheet %>% select(playername,name,Assists, NP_Goals ,Goals)
df <- left_join(df1,dfxgb,by=c("playername"="PlayerId","name"="TeamId")) %>% 
  mutate(opbouw = betrokken - (Assists + NP_Goals)) %>%
  select(-betrokken)

rank <- RankOld %>% select(Team,GF)
df <- left_join(df,rank,by=c("name"="Team")) %>% select(-NP_Goals)

df

df3 <- top_n(ungroup(df),12, (rowSums(df[c(3:5)])/df$GF))
df3 <- df3 %>% select(playername,name,opbouw,Assists,Goals,GF)
df2 <- df3 %>% select(playername,GF)
df3 <- df3 %>% mutate(GFrest = GF - (opbouw+Assists+Goals)) %>% select(-GF)
df3 <- gather(df3,"variable","value",opbouw:GFrest) 

df3$variable<-gsub("Goals","A",df3$variable)
df3$variable<-gsub("Assists","B",df3$variable)
df3$variable<-gsub("opbouw","C",df3$variable)
df3$variable<-gsub("GFrest","D",df3$variable)




waf <- ungroup(df3) %>% 
  arrange(variable, variable) 
  
waf$playername1 <- factor(waf$playername, levels = c( "Georgios Giakoumakis","Henk Veerman",
                                                      "Joey Veerman","Mike Trésor","Rai Vloet",
                                                      "Steven Berghuis","Vangelis Pavlidis",
                                                      "Danilo", "Donyell Malen",
                                                      
                                                      "Michiel Kramer",
                                                 
                                                      "Dusan Tadic",	
                                                      "Lennerd Daneels"
                                                   ))

  
  ggplot(waf,aes(fill = fct_rev(variable), values = as.numeric(value))) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = NULL, colour = NULL) +
  theme_ipsum_rc(grid="") +
    scale_fill_manual(values=c("A"="#2511D4",
                               "C"="lightblue2",
                               "D"=alpha(NA,1/2),
                              "B"="#776CDF",
                              "black" = alpha("grey",0.1)))+
  theme_enhance_waffle() +
    
geom_waffle(data=df2,aes(values=GF,fill="black"),
            n_rows = 10, n_cols = 10,
            size = 0.5, colour=NA, flip = TRUE,
            make_proportional = TRUE,
            radius = unit(3, "pt")) +

    geom_waffle(
    n_rows = 10, n_cols = 10,
    size = 0.5, aes(colour = variable), flip = TRUE,
    make_proportional = TRUE,
    radius = unit(3, "pt")
  ) + scale_colour_manual(values = c("A"="black",
                                     "B"="black",
                                     "C"="black",
                                     "D"=NA,
                                     "black"=NA))+
    #labs(title=glue::glue("{round((sum(waf$value)-(waf$value[4]))/sum(waf$value)*100,2)}%")) +
    labs(title="In what percentage of the goals are players involved?",
         subtitle = "Involvement by <span style='color:blue'>Goal</span>, <span style='color:#776CDF'>Assist</span> or <span style='color:lightblue2'>Build Up</span>",
         caption = "@RobinWilhelmus")+
    geom_label(aes(x = 8, y = 9, label = f_labels$label),size=6,family= "Spartan-Bold")+
    theme(legend.position = "none",
          plot.title = element_markdown(family= "Spartan-Bold",hjust=0.5,size=25),
          plot.subtitle = element_markdown(hjust=0.5,family= "Spartan-Bold",size=22),
          plot.caption = element_markdown(family= "Spartan-Bold",size=15),
          plot.background = element_rect(fill="#1a1d2c"),
          text=element_text(colour="white"),
          strip.text = element_text(colour="white",size=20)) +
    facet_wrap(~playername1)
    
  ggsave("test2.png",width=13,height=13)
  system("convert test2.png -trim test2.png")
  waf$size_f = factor(waf$playername, levels=c("Henk Veerman","Joey Veerman","Fredrik Midtsjø"))
  f_labels <- data.frame(
                         label = rep(c("71%", "51%","55%",
                                       "54%", "61%","45%",
                                       "56%", "55%","58%",
                                       "48%", "58%","44%"),4))
  
  
  
