rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}
MakePassReady <- function(DATA){
  
#DATA <- df
DATA$TeamId <- as.character(DATA$TeamId)
DATA$PlayerId <- as.character(DATA$PlayerId)

colnames(DATA)[27] <- "Type"
#colnames(DATA)[26] <- "type.displayName"
data <- dplyr::filter(DATA, Type == "Pass" | Type == "SubstitutionOff")

names(data)[names(data) == "endX"] <- "xend"
names(data)[names(data) == "endY"] <- "yend"

#data <- data %>% select(2,3,22,23,24,7,8)
#data <- na.omit(data)
data$xm <- as.numeric(data$x) * 106
data$xendm <- as.numeric(data$xend) * 106
data$ym <- as.numeric(data$y) * 70.4
data$yendm <- as.numeric(data$yend) * 70.4



data$direction <- ifelse(data$xm<data$xendm,"vooruit",ifelse(data$xm>data$xendm,"achteruit","opzij"))
data$difxmreal <- data$xendm-data$xm
data$difymreal <- data$yendm-data$ym
data$difxm <- abs(data$xendm-data$xm)
data$difym <- abs(data$yendm-data$ym)
data$zone <- ifelse(data$xm<=data$xendm&data$ym<=data$yendm& data$difym>=data$difxm,1,
                    ifelse(data$xm<=data$xendm&data$ym<=data$yendm& data$difym<=data$difxm,2,
                           ifelse(data$xm<=data$xendm&data$ym>=data$yendm&data$difym<=data$difxm,3,
                                  ifelse(data$xm<=data$xendm&data$ym>=data$yendm&data$difym>=data$difxm,4,
                                         ifelse(data$xm>=data$xendm&data$ym>=data$yendm&data$difym>=data$difxm,5,
                                                ifelse(data$xm>=data$xendm&data$ym>=data$yendm&data$difym<=data$difxm,6,
                                                       ifelse(data$xm>=data$xendm&data$ym<=data$yendm&data$difym<=data$difxm,7,
                                                              ifelse(data$xm>=data$xendm&data$ym<=data$yendm&data$difym>=data$difxm,8,"oeps"))))))))

data$border <- rep(500,length(data$x))
data$distance <- sqrt((data$xendm - data$xm)^2 + (data$yendm - data$ym)^2)
data$angle <- atan2(data$difym,data$difxm)
data$angle <- rad2deg(data$angle)
data$angle2 <- as.numeric(ifelse(data$difxmreal == 0 & data$difymreal >0 , 0,
                                 ifelse(data$difxmreal >0 & data$difymreal == 0, 90,
                                        ifelse(data$difxmreal == 0 & data$difymreal < 0 , 180,
                                               ifelse(data$difxmreal <0 & data$difymreal == 0 , 270,
                                                      ifelse(data$difxmreal>0 & data$difymreal>0, 90 - data$angle,
                                                             ifelse(data$difxmreal>0 & data$difymreal<0, 90 + data$angle,
                                                                    ifelse(data$difxmreal<0 & data$difymreal<0, 270 - data$angle,
                                                                           ifelse(data$difxmreal<0 & data$difymreal>0,270 + data$angle, "OEPS")))))))))

data$zone3 <- ifelse(data$angle2 >=345 | data$angle2<15, 1,
                     ifelse(data$angle2>=15 & data$angle2 < 45 , 2 ,
                            ifelse(data$angle2 >=45 & data$angle2 < 75 , 3,
                                   ifelse(data$angle2 >=75 & data$angle2 < 105,4,
                                          ifelse(data$angle2 >=105 & data$angle2 < 135 , 5 ,
                                                 ifelse(data$angle2>=135 & data$angle2 < 165, 6,
                                                        ifelse(data$angle2 >= 165 & data$angle2 < 195,7,
                                                               ifelse(data$angle2 >= 195 & data$angle2 < 225,8,
                                                                      ifelse(data$angle2 >= 225 & data$angle2 < 255,9,
                                                                             ifelse(data$angle2 >= 255 & data$angle2 < 285,10,
                                                                                    ifelse(data$angle2 >= 285 & data$angle2 < 315,11,12)))))))))))

data$passesIntoThird <- ifelse(data$x<66.6&data$xend>66.6,1,0)
data$passesInThird <- ifelse(data$x>66.6&data$xend>66.6,1,0)
data$goingForward  <- ifelse(data$direction=="vooruit",1,0)
data$startToGoal <- sqrt((10600 - data$xm)^2 + (3520 - data$ym)^2)
data$endToGoal <- sqrt((10600 - data$xendm)^2 + (3520 - data$yendm)^2)
data$progress <- data$startToGoal  - data$endToGoal 
data <- data[!is.na(data$zone3),]

return(data)
}

