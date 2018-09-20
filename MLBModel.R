library(dplyr)
library(randomForest)
library(rpart)

setwd("C:/Data/MLB/2018/Pitcher/")

file_list <- list.files()

for (file in file_list){
  
  # if the merged dataset does exist, append to it
  if (exists("pitchers")){
    temp_dataset <-read.table(file, header=TRUE, sep=",",stringsAsFactors = FALSE)
    pitchers<-rbind(pitchers, temp_dataset)
    rm(temp_dataset)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("pitchers")){
    pitchers <- read.table(file, header=TRUE, sep=",",stringsAsFactors = FALSE)
  }
  
}

setwd("C:/Data/MLB/2018/Batter/")

file_list <- list.files()

for (file in file_list){
  
  # if the merged dataset does exist, append to it
  if (exists("batters")){
    temp_dataset <-read.table(file, header=TRUE, sep=",",stringsAsFactors = FALSE)
    batters<-rbind(batters, temp_dataset)
    rm(temp_dataset)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("batters")){
    batters <- read.table(file, header=TRUE, sep=",",stringsAsFactors = FALSE)
  }

  
}

setwd("C:/Data/MLB/2018/DailyData/RotoWire/")

file_list <- list.files()

for (file in file_list){
  
  # if the merged dataset does exist, append to it
  if (exists("projected")){
    temp_dataset <-read.table(file, header=TRUE, sep=",",stringsAsFactors = FALSE)
    projected<-rbind(projected, temp_dataset)
    rm(temp_dataset)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("projected")){
    projected <- read.table(file, header=TRUE, sep=",",stringsAsFactors = FALSE)
  }
  
}

projected[projected == "-" ] <- NA
projected <- type.convert(projected)
projected$date <- as.Date(as.character(projected$date))
projectedpitcher <- projected[projected$position == 'P',]
projectedbatter <- projected[projected$position != 'P',]

pitcherprediction <- projectedpitcher %>% filter(date == max(date))
pitchertraining <- projectedpitcher %>% filter(date != max(date))
batterprediction <- projectedbatter %>% filter(date == max(date))
battertraining <- projectedbatter %>% filter(date != max(date))

names(batters)[names(batters) == 'Name'] <- 'player'
batters$date <- as.Date(batters$date)
#batters <- batters %>% filter(PA >= 3)
batters <- batters[,c("player","date","batterpoints")]
maxpoints <- (2 * sd(batters$batterpoints[batters$batterpoints!=0])) + mean(batters$batterpoints[batters$batterpoints!=0])
#batters$batterpoints <- ifelse(batters$batterpoints > maxpoints,maxpoints,batters$batterpoints)


training <- merge(batters,battertraining,by=c("date","player"))
training$weatherindicator <- as.factor(training$weatherindicator)
batterprediction$weatherindicator <- as.factor(batterprediction$weatherindicator)
batterprediction <- batterprediction %>% filter(injury == '')
levels(batterprediction$weatherindicator) <- levels(training$weatherindicator)

model <- randomForest(formula = batterpoints ~ opp_pitcher_era +
                        vs_pitcher_avg + vs_pitcher_ops + vs_pitcher_fpts +
                        vs_pitcher_hand_avg + vs_pitcher_hand_ops + vs_pitcher_hand_fpts +
                        game_location_avg + game_location_ops +
                        game_location_fpts + season_to_date_hits +
                        season_to_date_avg + season_to_date_ops +
                        season_to_date_fpts + last_seven_games_hits + last_seven_games_avg +
                        last_seven_games_ops + last_seven_games_fpts +
                        weatherindicator + over_under + opp_pitcher_sb +
                        proj_points
                      ,data = training)
pred <- predict(model,batterprediction)
batterprediction$projectedpoints <- pred

# model <- rpart(formula = batterpoints ~ trend_score + opp_pitcher_era +
#                         vs_pitcher_avg + vs_pitcher_ops + vs_pitcher_fpts +
#                         vs_pitcher_hand_avg + vs_pitcher_hand_ops + vs_pitcher_hand_fpts + game_location_avg +
#                         game_location_ops +
#                         game_location_fpts + season_to_date_hits +
#                         season_to_date_avg + season_to_date_ops +
#                         season_to_date_fpts + last_seven_games_hits + last_seven_games_avg +
#                         last_seven_games_ops + last_seven_games_fpts +
#                         weatherindicator + over_under + opp_pitcher_era + opp_pitcher_sb + proj_points,
#                       data = training)
# pred <- predict(model,batterprediction)
# batterprediction$projectedpoints <- pred


batterprediction$P <- ifelse(batterprediction$position == 'P',1,0)
batterprediction$C1 <- ifelse(batterprediction$position == 'C1',1,0)
batterprediction$'2B' <- ifelse(batterprediction$position == '2B',1,0)
batterprediction$SS <- ifelse(batterprediction$position == 'SS',1,0)
batterprediction$'3B' <- ifelse(batterprediction$position == '3B',1,0)
batterprediction$OF <- ifelse(batterprediction$position == 'OF',1,0)
batterprediction$Util <- 1
batterprediction$lb <- 0
batterprediction$ub <- 1
batterprediction$type <- 'B'
pitcherprediction$P <- ifelse(pitcherprediction$position == 'P',1,0)
pitcherprediction$C1 <- ifelse(pitcherprediction$position == 'C1',1,0)
pitcherprediction$'2B' <- ifelse(pitcherprediction$position == '2B',1,0)
pitcherprediction$SS <- ifelse(pitcherprediction$position == 'SS',1,0)
pitcherprediction$'3B' <- ifelse(pitcherprediction$position == '3B',1,0)
pitcherprediction$OF <- ifelse(pitcherprediction$position == 'OF',1,0)
pitcherprediction$Util <- 0
pitcherprediction$lb <- 0
pitcherprediction$ub <- 1
pitcherprediction$type <- 'B'
names(pitcherprediction)[names(pitcherprediction) == 'proj_points'] <- 'projectedpoints'




library(ROI)
library(slam)
library(ROI.plugin.glpk)

factor2char <- function(df){
  i <- sapply(df, is.factor)
  df[i] <- lapply(df[i], as.character)
  return(df)
}

getBounds_matrix <- function(obj){
  li <- 1:NROW(obj)
  ui <- 1:NROW(obj)
  lb <- ifelse(obj$lb, obj$lb, 0)
  ub <- ifelse(obj$ub, obj$ub, Inf)
  nobj <- max(li, ui)
  bounds <- V_bound(li = li, ui = ui, lb = lb, ub = ub, nobj = nobj)
  if ((length(bounds$lower$ind) == 0) && (length(bounds$upper$ind) == 0)){
    bounds <- NULL
  }
  bounds
}

objcols <- c('player','projectedpoints','lb','ub','type')
obj <- rbind(
  subset(batterprediction, select = objcols), 
  subset(pitcherprediction, select = objcols)
)
names(obj)[names(obj) == 'player'] <- 'variable'
names(obj)[names(obj) == 'projectedpoints'] <- 'coefficient'
concols <- c('player','salary','P','C1','2B','SS','3B','OF','Util')
con <- rbind(
  subset(batterprediction, select = concols), 
  subset(pitcherprediction, select = concols)
)
names(con)[names(con) == 'player'] <- 'variable'
dir <- data.frame(dir=c('<=','==','>=','>=','>=','>=','>=','=='), rhs=c(35000,1,1,1,1,1,3,9))

con <- factor2char(con)
obj <- factor2char(obj)
matCols <- !(names(con) %in% c("variable"))
matRows <- !(con$variable %in% c("dir", "description", "rhs"))

A <- t(sapply(con[matRows, matCols, drop = F], as.numeric))
colnames(A) <- con$variable[matRows]
B <- cbind(
  constraints = setdiff(colnames(con), 'variable'),
  dir
)
B <- factor2char(B)
con[is.na(con)] <- 0
con <- as.simple_triplet_matrix(con)


objective <- as.vector(obj$coefficient)
constraints <- L_constraint(
  L = A,
  dir = B$dir,
  rhs = B$rhs
)

op_obj <- OP(
  objective = objective,
  constraints = constraints,
  types = obj$type,
  bounds = getBounds_matrix(obj),
  maximum = TRUE
)

sol <- ROI_solve(op_obj,solver = 'glpk')

solution <- data.frame(sol$solution,player = obj$variable, points =obj$coefficient)

solcols <- c('player','team', 'position')
dfsol <- rbind(
  subset(batterprediction, select = solcols), 
  subset(pitcherprediction, select = solcols)
)
solution <- merge(solution, dfsol, by = c("player"))
solution[solution$sol.solution == 1,]
print(model)
