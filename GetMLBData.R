library(RCurl)
library(jsonlite)
library(tidyr)
library(baseballr)

#get rotowireprojections
raw_projdata <- getURL("https://www.rotowire.com/daily/tables/optimizer-mlb.php?sport=MLB&site=FanDuel&projections=&type=main&slate=main")
projdata <- fromJSON(raw_projdata)

projdata <- projdata %>%
  separate(weather_desc,c('windind','windspeed','windUnits','winddirection'),' ')
projdata$date <- Sys.Date()
projdata$windspeed <- as.numeric(projdata$windspeed)
projdata$weatherindicator <- ifelse(projdata$windind == 'In','none',  
                          ifelse(projdata$winddirection == 'Out' & projdata$windspeed >= 10, 'strongout',
                           ifelse(projdata$winddirection == 'Out' & projdata$windspeed < 10, 'mildout',
                            ifelse(projdata$winddirection == 'In' & projdata$windspeed >= 10, 'strongin',
                             ifelse(projdata$winddirection == 'In' & projdata$windspeed < 10, 'mildin',
                              ifelse(projdata$winddirection %in% c('R-L','L-R')  & projdata$windspeed < 10, 'mildcross',
                               ifelse(projdata$winddirection %in% c('R-L','L-R')  & projdata$windspeed >= 10, 'strongcross',
                                      'none')))))))
write.csv(projdata, paste('C:/Data/MLB/2018/DailyData/RotoWire/',Sys.Date(),'_Projected.csv',sep = ''))

#getrotogrinder projections
batURL <- paste('https://rotogrinders.com/projected-stats/mlb-hitter.csv?site=fanduel&date=',Sys.Date(),sep = '')
batx <- getURL(batURL)
batout <- read.csv(textConnection(batx),header = FALSE)
pitchURL <- paste('https://rotogrinders.com/projected-stats/mlb-pitcher.csv?site=fanduel&date=',Sys.Date(),sep = '')
pitchx <- getURL(pitchURL)
pitchout <- read.csv(textConnection(pitchx),header = FALSE)
out <- rbind(pitchout,batout)
names(out) <- c("player","salary","team","position","opponent","v6","v7","projectedpoints")
out$position <- ifelse(out$position =='C-1B','C1',as.character(out$position))
out$date <- Sys.Date()
write.csv(out,paste('C:/Data/MLB/2018/DailyData/RotoGrinder/',Sys.Date(),'_RotoGrinder.csv',sep = ''))
write.csv(out,'C:/R/Shiny/MLBPrediction/Data/latest_RotoGrinder.csv')

yesterday <- Sys.Date()-1

dfpitcher <- daily_pitcher_bref(yesterday, yesterday)
dfpitcher$date <- yesterday
dfpitcher[is.na(dfpitcher)] <- 0
dfpitcher$QS <- ifelse(dfpitcher$IP >= 6 & dfpitcher$ER <= 3,1,0)
dfpitcher$pitchingpoints <- (dfpitcher$W * 6) + (dfpitcher$QS * 4) + (dfpitcher$SO * 3) + (dfpitcher$IP * 3) + (dfpitcher$ER * -3)
dfpitcher$pointsallowed <- (dfpitcher$X1B * 3) + (dfpitcher$X2B * 6) + (dfpitcher$X3B * 9) + (dfpitcher$HR * 12) + (dfpitcher$ER * 3.5) + (dfpitcher$R * 3.2) + (dfpitcher$BB * 3) + (dfpitcher$SB * 6) + (dfpitcher$HBP * 3)
write.csv(dfpitcher,paste('C:/Data/MLB/2018/Pitcher/',yesterday,'_Pitcher.csv',sep = ''))
          
dfbatter <- daily_batter_bref(yesterday, yesterday)
dfbatter$date <- yesterday
dfbatter[is.na(dfbatter)] <- 0
dfbatter$batterpoints <- (dfbatter$X1B * 3) + (dfbatter$X2B * 6) + (dfbatter$X3B * 9) + (dfbatter$HR * 12) + (dfbatter$RBI * 3.5) + (dfbatter$R * 3.2) + (dfbatter$BB * 3) + (dfbatter$SB * 6) + (dfbatter$HBP * 3)
write.csv(dfbatter,paste('C:/Data/MLB/2018/Batter/',yesterday,'_Batter.csv',sep = ''))

#deploy shinyapps
rsconnect::setAccountInfo(name='zwag20', token='5C33E7024D85F7A73E3A25097F451684', secret='IJSyNgayoL1ZD1WDKnka0y/ezgjdN/SM5TqIzKHc')
library(rsconnect)
deployApp("C:/R/Shiny/MLBPrediction",launch.browser = FALSE)
Y
