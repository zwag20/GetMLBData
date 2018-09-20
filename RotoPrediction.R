rm(list=ls())
cat("\014") 

library(ROI)
library(slam)
library(ROI.plugin.glpk)
library(shiny)
library(shinythemes)

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

getLineup <- function(players = NULL,n = 0){
projected <- read.csv('C:/Data/MLB/2018/DailyData/RotoWire/2018-09-08_Projected.csv')
#projected <- read.csv(url('https://rotogrinders.com/projected-stats/mlb-hitter.csv?site=fanduel'))
projected[projected == "-" ] <- NA
projected <- type.convert(projected)
projected$date <- as.Date(as.character(projected$date))
projectedpitcher <- projected[projected$position == 'P',]
projectedbatter <- projected[projected$position != 'P',]

batterprediction <- projectedbatter
pitcherprediction <- projectedpitcher

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
names(batterprediction)[names(batterprediction) == 'proj_points'] <- 'projectedpoints'


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
dir <- data.frame(dir=c('<=','==','>=','>=','>=','>=','>=','=='), rhs=c(35000,1,1,1,1,1,3,8)) #need to add here also
adddir <- dir[rep(2,n),]
dir <- rbind(dir,adddir)

con <- factor2char(con)
con[ ,players] <- 0 #this will be the input list 
obj <- factor2char(obj)
matCols <- !(names(con) %in% c("variable"))
matRows <- !(con$variable %in% c("dir", "description", "rhs"))

A <- t(sapply(con[matRows, matCols, drop = F], as.numeric))
colnames(A) <- con$variable[matRows]
A[outer(rownames(A), colnames(A), "==")] <- 1
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
solcols <- c('player','team', 'position','salary')
dfsol <- rbind(
  subset(batterprediction, select = solcols), 
  subset(pitcherprediction, select = solcols)
)
solution <- merge(solution, dfsol, by = c("player"))
solution <- solution[solution$sol.solution == 1,]
solution
#sum(solution$points)
}

getLineup()