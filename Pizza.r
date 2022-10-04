### Getting and preparing the data 

library(worldfootballR)  #for scraping
library(tidyverse)       #for ggplot, dplyr and several other stuff
library(forcats)         #for sorting within ggplot
library(glue)            #easier than paste()

df<-fb_player_scouting_report("https://fbref.com/en/players/282679b4/Mateusz-Klich")

df <- df %>% filter(scouting_period == "Last 365 Days")


#For some players you need to add

#pos_versus = "primary"

df_selected<- data.frame(player_name = "Mateusz Klich",
                         Statistic = c("Pressures (Att 3rd)", 
                                       "% of dribblers tackled",
                                       "Touches (Att 3rd)",
                                       "Carries into Final Third",
                                       "Progressive Passes Rec",
                                       "Crosses"),
                         Per90 = c(4.63,
                                    16,
                                    26.11,
                                    2.55,
                                    7.22,
                                    1.32),
                         Percentile = c(92,
                                        3,
                                        94,
                                        93,
                                        98,
                                        87),
                         stat=c("Defending",
                                "Defending",
                                "Possession",
                                "Possession",
                                "Possession",
                                "Attacking"))


print(df$Statistic)

df_selected <- df[c(2,3,9,10,13,28,29,47,73,107,109,116,118,126,148),]

df_selected <- df_selected %>% 
      mutate(stat=case_when(Statistic == "Non-Penalty Goals"|
                            Statistic == "npxG"|
                            Statistic == "Shots Total"|
                            Statistic == "Assists"|
                            Statistic == "xA"|
                            Statistic == "npxG+xA"|
                            Statistic == "Shot-Creating Actions" ~ "Attacking",
                            Statistic == "Passes Attempted"|
                            Statistic == "Pass Completion %"|
                            Statistic == "Progressive Passes"|
                            Statistic == "Progressive Carries"|
                            Statistic == "Dribbles Completed"|
                            Statistic == "Touches (Att Pen)"|
                            Statistic == "Progressive Passes Rec" ~ "Possession",
                                 TRUE ~ "Defending"))


### Making the chart

ggplot(df_selected,aes(fct_reorder(Statistic,stat),Percentile)) +                       #select the columns to plot and sort it so the types of metric are grouped
  geom_bar(aes(y=100,fill=stat),stat="identity",width=1,colour="white",                 #make the whole pizza first
  alpha=0.5) +                                                                          #change alphe to make it more or less visible
  geom_bar(stat="identity",width=1,aes(fill=stat),colour="white") +                     #insert the values 
  coord_polar() +                                                                       #make it round
  geom_label(aes(label=Per90,fill=stat),size=2,color="white",show.legend = FALSE)+     #add a label for the value. Change 'label=Per.90' to 'label=Percentile' to show the percentiles
 scale_fill_manual(values=c("Possession" = "#D70232",                                   #choose colors to fill the pizza parts
                             "Attacking" = "#1A78CF",
                             "Defending" = "#FF9300")) +                                                              
  scale_y_continuous(limits = c(-10,100))+                                              #create the white part in the middle.   
  labs(fill="",                                                                         #remove legend title
       caption = "Data from StatsBomb via FBref",                                       #credit FBref/StatsBomb
       title=df_selected$Player[1])+                                                    #let the title be te name of the player
 
  theme_minimal() +                                                                     #from here it's only themeing. 
  theme(legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family="Spartan-Light"),                                    #I downloaded this font from Google Fonts. You can use your own font of course
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5,size=6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

temp <- (360/(nrow(df_selected))/2)                             #find the difference in angle between to labels and divide by two.
myAng <- seq(-temp, -360+temp, length.out = nrow(df_selected))  #get the angle for every label
ang<-ifelse(myAng < -90, myAng+180, myAng)                                    #rotate label by 180 in some places for readability
ang<-ifelse(ang < -90, ang+180, ang)                                          #rotate some lables back for readability...

df_selected$Statistic <- gsub(" ","\n",df_selected$Statistic)

ggplot(df_selected,aes(fct_reorder(Statistic,stat),Percentile)) +                       #select the columns to plot and sort it so the types of metric are grouped
  geom_bar(aes(y=100,fill=stat),stat="identity",width=1,colour="white",                 #make the whole pizza first
  alpha=0.5) +                                                                          #change alphe to make it more or less visible
  geom_bar(stat="identity",width=1,aes(fill=stat),colour="white") +                     #insert the values 
  coord_polar() +                                                                       #make it round
  geom_label(aes(label=Per90,fill=stat),size=2,color="white",show.legend = FALSE)+     #add a label for the value. Change 'label=Per.90' to 'label=Percentile' to show the percentiles
 scale_fill_manual(values=c("Possession" = "#D70232",                                   #choose colors to fill the pizza parts
                             "Attacking" = "#1A78CF",
                             "Defending" = "#FF9300")) +                                                              
  scale_y_continuous(limits = c(-10,100))+                                              #create the white part in the middle.   
  labs(fill="",                                                                         #remove legend title
       caption = "Data from StatsBomb via FBref",                                       #credit FBref/StatsBomb
       title=df_selected$Player[1])+                                                    #let the title be te name of the player
 
  theme_minimal() +                                                                     #from here it's only themeing. 
  theme(legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 6, angle = ang),
        text = element_text(family="Spartan-Light"),                                    #I downloaded this font from Google Fonts. You can use your own font of course
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5,size=6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

ggplot(df_selected,aes(fct_reorder(Statistic,stat),Percentile)) +                       #select the columns to plot and sort it so the types of metric are grouped
  geom_bar(aes(y=100,fill=stat),stat="identity",width=1,colour="white",                 #make the whole pizza first
  alpha=0.5) +                                                                          #change alphe to make it more or less visible
  geom_bar(stat="identity",width=1,aes(fill=stat),colour="white") +                     #insert the values 
  coord_polar() +                                                                       #make it round
  geom_label(aes(label=Per90,fill=stat),size=2,color="white",show.legend = FALSE)+      #add a label for the value. Change 'label=Per.90' to 'label=Percentile' to show the percentiles
 scale_fill_manual(values=c("Possession" = "#D70232",                                   #choose colors to fill the pizza parts
                             "Attacking" = "#1A78CF",
                             "Defending" = "#FF9300")) +                                                              
  scale_y_continuous(limits = c(-10,100))+                                              #create the white part in the middle.   
  labs(fill="",   
       caption = "Data from StatsBomb via FBref",     
       #remove legend title
       title=glue("{df_selected$Player[1]} | Leeds United"),
        subtitle = glue::glue("{df_selected$season} | Compared to midfielders Top 5 competitions | stats per 90"))+ #let the title be te name of the player                                                
 
  theme_minimal() +                                                                     #from here it's only themeing. 
  theme(plot.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
        panel.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
         axis.text.x = element_text(size = 6, angle = ang),
        text = element_text(family="Spartan-Light"),                                    #I downloaded this font from Google Fonts. You can use your own font of course
        plot.title = element_markdown(hjust=0.5,family="Spartan-Medium"),
        plot.subtitle = element_text(hjust=0.5,size=8),
        plot.caption = element_text(hjust=0.5,size=6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5,2,2,2)) 


system("convert -trim image.png new_image.png")

ggsave("image.png",bg="#F2F4F5")

### Some other (well known) styles


#### The Athletic/ Tom Worville

ggplot(df_selected,aes(fct_reorder(Statistic,stat),Percentile)) +                       
  geom_bar(aes(y=100),fill="#131313",stat="identity",width=1,colour="#797979",                 
  alpha=0.5,show.legend = FALSE) +      
  
  
  geom_bar(stat="identity",width=1,aes(fill=stat),colour="#F3FEFC",alpha=1) +                     
  coord_polar(clip = "off") +                                                                      
     geom_hline(yintercept=25, colour="#565656",linetype="longdash",alpha=0.5)+
  geom_hline(yintercept=50, colour="#565656",linetype="longdash",alpha=0.5)+
  geom_hline(yintercept=75, colour="#565656",linetype="longdash",alpha=0.5)+ 
 scale_fill_manual(values=c("Possession" = "#1ADA89",                                   
                             "Attacking" = "#0F70BF",
                             "Defending" = "#EC313A")) +                                                        
   geom_label(aes(label=Percentile,fill=stat),size=2,color="white",show.legend = FALSE)+ 
  scale_y_continuous(limits = c(-20,100))+                                              
  labs(fill="",   
       caption = "Data from StatsBomb via FBref\nStyle copied from The Athletic/@worville",     
       #remove legend title
       title=glue("{df_selected$Player[1]} | Leeds United"),
        subtitle = glue::glue("{df_selected$season} | Compared to midfielders Top 5 competitions | stats per 90"))+                                                
  theme_minimal() +                                                                     
  theme(plot.background = element_rect(fill = "#131313",color = "#131313"),
        panel.background = element_rect(fill = "#131313",color = "#131313"),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 6,colour = "#FFFFFF"),
        text = element_text(family="Spartan-Light",colour= "#FEFEFE"),                                   
        plot.title = element_markdown(hjust=0.5,family="Spartan-Medium"),
        plot.subtitle = element_text(hjust=0.5,size=8),
        plot.caption = element_text(hjust=0.5,size=6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5,4,2,4)) 


 

#### Football Slices


ggplot(df_selected,aes(fct_reorder(Statistic,stat),Percentile)) +                      
  geom_bar(aes(y=100),fill="#FAFBFD",stat="identity",width=1,colour="black",                 
  alpha=0.5) +                                                                          
  geom_bar(stat="identity",width=0.95,aes(fill=stat),colour=NA) +                    
  coord_polar(clip = "off") +                                                                       
 
   geom_hline(yintercept=25, colour="#CFD0D2",alpha=1,size=0.1)+
  geom_hline(yintercept=50, colour="#CFD0D2",alpha=1,size=0.1)+
  geom_hline(yintercept=75, colour="#CFD0D2",alpha=1,size=0.1)+ 
   geom_text(aes(label=Per90,fill=stat),size=2,color="black",show.legend = FALSE)+  
 scale_fill_manual(values=c("Possession" = "#F47294",                                   
                             "Attacking" = "#E7D96E",
                             "Defending" = "#8FBFEF")) +                                                              
  scale_y_continuous(limits = c(-10,110))+                                             
  labs(fill="",   
       caption = "Data from StatsBomb via FBref\nStyle copied from @FootballSlices",     
       #remove legend title
       title=glue("{df_selected$Player[1]} | Leeds United"),
        subtitle = glue::glue("{df_selected$season} | Compared to midfielders Top 5 competitions | stats per 90"))+                                               
  theme_minimal() +                                                                  
  theme(plot.background = element_rect(fill = "#FAFBFD",color = "#FAFBFD"),
        panel.background = element_rect(fill = "#FAFBFD",color = "#FAFBFD"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
         axis.text.x = element_text(size = 6),
        text = element_text(family="Spartan-Light"),                                    
        plot.title = element_markdown(hjust=0.5,family="Spartan-Medium"),
        plot.subtitle = element_text(hjust=0.5,size=8),
        plot.caption = element_text(hjust=0.5,size=6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5,2,2,2)) 

 

#### One with no background

ggplot(df_selected,aes(fct_reorder(Statistic,stat),Percentile)) +                      
  geom_bar(aes(y=100),fill="#F2F4F5",stat="identity",width=1,colour="white",                
           alpha=1,linetype="dashed") +                                                                          
  geom_bar(stat="identity",width=1,fill="#D20222",colour="white") +   
  geom_hline(yintercept=25, colour="white",linetype="longdash",alpha=0.5)+
  geom_hline(yintercept=50, colour="white",linetype="longdash",alpha=0.5)+
  geom_hline(yintercept=75, colour="white",linetype="longdash",alpha=0.5)+ 
  geom_hline(yintercept=100, colour="white",alpha=0.5)+ 
  coord_polar() +                                                                     
  geom_label(aes(label=Per90),fill="#D20222",size=2,color="white",show.legend = FALSE)+     
  scale_fill_manual(values=c("Possession" = "#D70232",                                  
                             "Attacking" = "#1A78CF",
                             "Defending" = "#FF9300")) +                                                              
  scale_y_continuous(limits = c(-10,100))+                                              
  labs(fill="",   
       caption = "Data from StatsBomb via FBref",     
       #remove legend title
       title=glue("{df_selected$Player[1]} | Manchester United"),
       subtitle = glue::glue("{df_selected$season} | Compared to midfielders Top 5 competitions | stats per 90"))+                                               
  
  theme_minimal() +                                                                     
  theme(plot.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
        panel.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 6, angle = ang),
        text = element_text(family="Spartan-Light"),                                    
        plot.title = element_markdown(hjust=0.5,family="Spartan-Medium"),
        plot.subtitle = element_text(hjust=0.5,size=8),
        plot.caption = element_text(hjust=0.5,size=6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5,2,2,2)) 


label_data <- df_selected
# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
label_data$id <- seq(1,length(label_data$player_name))
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)



ggplot(df_selected,aes(fct_reorder(Statistic,stat),Percentile)) +                      
  geom_bar(aes(y=100),fill="#0066B2",stat="identity",width=1,colour="#0066B2",                
           alpha=0.4,linetype="dashed") +                                                                          
  geom_bar(stat="identity",width=1,fill="#CC0033",colour="white") +   
  geom_hline(yintercept=25, colour="white",linetype="longdash",alpha=0.5)+
  geom_hline(yintercept=50, colour="white",linetype="longdash",alpha=0.5)+
  geom_hline(yintercept=75, colour="white",linetype="longdash",alpha=0.5)+ 
  geom_hline(yintercept=100, colour="white",alpha=0.5)+ 
  coord_polar() +                                                                     
  geom_label(aes(label=Per90),fill="#CC0033",size=2,color="white",show.legend = FALSE,family="Spartan-Bold")+     
  scale_fill_manual(values=c("Possession" = "#D70232",                                  
                             "Attacking" = "#1A78CF",
                             "Defending" = "#FF9300")) +                                                              
  scale_y_continuous(limits = c(-10,110))+                                              
  labs(fill="",   
       caption = "Data from StatsBomb via FBref",     
       #remove legend title
       title=glue("{df_selected$Player[1]} | Bayern Munich"),
       subtitle = glue::glue("{df_selected$season} | Compared to attackers Top 5 competitions | stats per 90"))+                                               
  geom_text(data=label_data, aes(x=id, y=100+10, label=Statistic, hjust=hjust), 
      color="#0066B2", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )  +
  theme_minimal() +                                                                     
  theme(plot.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
        panel.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
       # axis.text.x = element_text(size = 6, angle = ang),
        axis.text.x = element_blank(),
        text = element_text(family="Spartan-Light"),                                    
        plot.title = element_markdown(hjust=0.5,family="Spartan-Medium"),
        plot.subtitle = element_text(hjust=0.5,size=8),
        plot.caption = element_text(hjust=0.5,size=6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5,2,2,2)) 
