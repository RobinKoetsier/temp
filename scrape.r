library(rvest)
tran <- html("https://www.transfermarkt.com/ajax-amsterdam/spielplandatum/verein/610")
e <- tran %>%
  html_nodes(".rechts+ .zentriert") %>%
  html_text()


df <- as.data.frame(cbind(a[1:52],b[1:52],c[1:52],d,e))

df %>% separate(d,"home","away", sep="(")



df
library(stringr)

  df$away <- as.numeric(str_split_fixed(df$e, ":", 2)[,2])
df$home <- as.numeric(str_split_fixed(df$e, ":", 2)[,1])


df$day <- substr(df$V1, start = 1, stop = 2)

df$result <-  ifelse(df$home == df$away, "draw",
                     ifelse(df$V2 == "H" & df$home > df$away, "win",
                   
                    ifelse(df$V2 == "H" & df$home < df$away,"lost",
                           ifelse(df$V2 == "A" & df$home < df$away, "win",
                                  ifelse(df$V2 == "A" & df$home > df$away, "lost","draw")))))

a[1:53]
tran %>% 
  html_nodes("table") %>%
  { .[1]} %>% 
  html_nodes("a") %>% 
  html_attr("href") 
