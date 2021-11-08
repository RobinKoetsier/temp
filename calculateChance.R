calculateChance<-function(team1,team2,p){
  #team1 <- c(0.1,0.2,0.3)
  #team2 <- c(0.9)
  #p<- 10000
  
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
  print(chances)
  
  print(paste("xpHome:",homeP,"xpAway:",awayP))
  game <- data.frame(home,draw,away,homeP,awayP)
  return(game)
}


testShots<-function(XG){
  Goals = 0
  XG[is.na(XG)] <- 0
  for(i in 1:length(XG)){
    if(runif(1, 0.0, 1.0)<=XG[i]){
      
      Goals <- Goals + 1
    }else{
      #  print("miss")
    }
  }
  
  
  return(Goals)
}  

calculateWinner <- function(home,away){
  HomeGoals = 0
  AwayGoals = 0
  
  HomeGoals <- testShots(home)
  AwayGoals <- testShots(away)
 # print(HomeGoals - AwayGoals)
  diffTemp <- (HomeGoals - AwayGoals)
  #print(diffTemp)
  diff <- append(diff,diffTemp)
  if(HomeGoals > AwayGoals){
    # print(paste("Home",HomeGoals,AwayGoals))
    return("home")
  }else if(AwayGoals > HomeGoals){
    # print(paste("Away",HomeGoals,AwayGoals))
    return("away")
  }else{
    #  print(paste("Share of the points!",HomeGoals,AwayGoals))
    return("draw")
  }
}



