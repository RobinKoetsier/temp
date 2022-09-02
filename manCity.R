ggplot(man_city_shots) +
  annotate_pitch(colour = "#454545",
                 fill = "#191919") +
  geom_point(aes(x = X, y = Y, colour = Result, fill = Result),
             shape = 21,
             size = 3) +
  coord_flip(xlim=c(65,105),
             ylim=c(100,0)) +
  theme_pitch() +
  scale_colour_manual(values = cols, aesthetics = c("colour", "fill"))+
 
  theme(panel.background = element_rect(fill = "#191919"),
        plot.background = element_rect(fill = "#191919"), # everything same background color
        legend.background = element_rect(fill = "#191919"), # including legend box
        legend.key = element_rect(fill = "#191919", color = NA), #including legend key box
        legend.text = element_text(colour="white"), #legend text
        legend.title = element_text(colour="white")) # legend title
