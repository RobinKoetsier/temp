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
       caption = "Data from StatsBomb via FBref",                                        #credit FBref/StatsBomb
       title=df_selected$player_name[1])+                                               #let the title be te name of the player
  
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
