library(baseballr)
library(RCurl)
library(xml2)
library(rvest)
library(jsonlite)
dfdates <- read.csv('C:/Data/MLB/2018/2018dates.csv')
colnames(dfdates) <- c('Date')
dfdates$Date <- as.character(dfdates$Date)
dfdates$batterpath <- paste('C:/Data/MLB/2018/Batter/',dfdates$Date,'_Batter.csv',sep = '')
dfdates$pitcherpath <- paste('C:/Data/MLB/2018/Pitcher/',dfdates$Date,'_Pitcher.csv',sep = '')
dftest <- dfdates[1,]

for(i in seq_len(nrow(dfdates))){
  #write.csv(daily_batter_bref(dfdates$Date[i], dfdates$Date[i]),dfdates$batterpath[i])
  #write.csv(daily_pitcher_bref(dfdates$Date[i], dfdates$Date[i]),dfdates$pitcherpath[i])
  #row <- dfdates[i,]
  #dfdates$Date[i]
  
  dfpitcher <- daily_pitcher_bref(dfdates$Date[i], dfdates$Date[i])
  dfpitcher$date <- dfdates$Date[i]
  dfpitcher[is.na(dfpitcher)] <- 0
  dfpitcher$QS <- ifelse(dfpitcher$IP >= 6 & dfpitcher$ER <= 3,1,0)
  dfpitcher$pitchingpoints <- (dfpitcher$W * 6) + (dfpitcher$QS * 4) + (dfpitcher$SO * 3) + (dfpitcher$IP * 3) + (dfpitcher$ER * -3)
  dfpitcher$pointsallowed <- (dfpitcher$X1B * 3) + (dfpitcher$X2B * 6) + (dfpitcher$X3B * 9) + (dfpitcher$HR * 12) + (dfpitcher$ER * 3.5) + (dfpitcher$R * 3.2) + (dfpitcher$BB * 3) + (dfpitcher$SB * 6) + (dfpitcher$HBP * 3)
  write.csv(dfpitcher,paste('C:/Data/MLB/2018/Pitcher/',dfdates$Date[i],'_Pitcher.csv',sep = ''))

  dfbatter <- daily_batter_bref(dfdates$Date[i], dfdates$Date[i])
  dfbatter$date <- dfdates$Date[i]
  dfbatter[is.na(dfbatter)] <- 0
  dfbatter$batterpoints <- (dfbatter$X1B * 3) + (dfbatter$X2B * 6) + (dfbatter$X3B * 9) + (dfbatter$HR * 12) + (dfbatter$RBI * 3.5) + (dfbatter$R * 3.2) + (dfbatter$BB * 3) + (dfbatter$SB * 6) + (dfbatter$HBP * 3)
  write.csv(dfbatter,paste('C:/Data/MLB/2018/Batter/',dfdates$Date[i],'_Batter.csv',sep = ''))
  
  #for(i in seq_len(nrow(dfdates))){
  batURL <- paste('https://rotogrinders.com/projected-stats/mlb-hitter.csv?site=fanduel&date=',dfdates$Date[i],sep = '')
  batx <- getURL(batURL)
  batout <- read.csv(textConnection(batx),header = FALSE)
  pitchURL <- paste('https://rotogrinders.com/projected-stats/mlb-pitcher.csv?site=fanduel&date=',dfdates$Date[i],sep = '')
  pitchx <- getURL(pitchURL)
  pitchout <- read.csv(textConnection(pitchx),header = FALSE)
  out <- rbind(pitchout,batout)
  names(out) <- c("player","salary","team","position","opponent","v6","v7","projectedpoints")
  out$position <- ifelse(out$position =='C-1B','C1',as.character(out$position))
  out$date <- dfdates$Date[i]
  write.csv(out,paste('C:/Data/MLB/2018/DailyData/RotoGrinder/',dfdates$Date[i],'_RotoGrinder.csv',sep = ''))
  
  #for(i in seq_len(nrow(dfdates))){
  batURL <- paste('https://rotogrinders.com/projected-stats/mlb-hitter?site=fanduel&date=',dfdates$Date[i],sep = '')
  batx <- read_html(batURL)
  bat <- html_text(html_nodes(batx, "body div > div > section > div > script"))
  bat <- sub('.[^]]+$', '', bat)
  bat <- sub('.[^]]+$', '\\]', bat)
  bat <- sub("^.*?\\[","\\[",bat)
  dfb <- fromJSON(bat, flatten=TRUE)
  dfb <- apply(dfb,2,as.character)
  write.csv(dfb,paste('C:/Data/MLB/2018/DailyData/RotoGrinder/Batter',dfdates$Date[i],'_RotoGrinder.csv',sep = ''))
  
  pitchURL <- paste('https://rotogrinders.com/projected-stats/mlb-pitcher?site=fanduel&date=',dfdates$Date[i],sep = '')
  pitchx <- read_html(pitchURL)
  pitch <- html_text(html_nodes(pitchx, "body div > div > section > div > script"))
  pitch <- sub('.[^]]+$', '', pitch)
  pitch <- sub('.[^]]+$', '\\]', pitch)
  pitch <- sub("^.*?\\[","\\[",pitch)
  dfp <- fromJSON(pitch, flatten=TRUE)
  dfp <- apply(dfp,2,as.character)
  write.csv(dfp,paste('C:/Data/MLB/2018/DailyData/RotoGrinder/Pitcher',dfdates$Date[i],'_RotoGrinder.csv',sep = ''))
}


