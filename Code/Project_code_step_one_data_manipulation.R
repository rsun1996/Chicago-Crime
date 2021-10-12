#########################################################################
### This code is for data manipulation part of the Chicago crime data ###
#########################################################################

################################
### Set up working directory ###
################################
mydir <- "C:/Users/peter/Documents/UTSA/Data Mining/Project-One-Chicago-Crime"
datadir <- paste(mydir,"/Data",sep = "")
output <- paste(mydir,"/Output",sep = "")

#######################
### Loading library ###
#######################
library(lubridate)
library(stringr)
library(dplyr)
library(caret)
library(MASS)

################################
### Read Chiacago crimes csv ###
################################
Chicago_crimes <- read.csv(paste(datadir,"/Crimes_-_2001_to_Present.csv",sep = ""))

###################################
### Keep only 34 and 60 for now ###
###################################
Crimes <- NULL
Crimes <- Chicago_crimes[which(Chicago_crimes$Community.Area == 34 | Chicago_crimes$Community.Area ==60 ),]
write.csv(Crimes,paste(output,"/Data/Chicago_crimes_34_60.csv",sep = ""),row.names = FALSE)
Crimes <- read.csv(paste(output,"/Data/Chicago_crimes_34_60.csv",sep = ""))

##############################
### format datetime column ###
##############################
  ########################################################
  ### Split original Date column into Date, time, AMPM ###
  ########################################################
  Crimes$Date_new <- str_split_fixed(Crimes$Date," ",3)[,1]
  Crimes$Time <- str_split_fixed(Crimes$Date," ",3)[,2]
  Crimes$AMPM <- str_split_fixed(Crimes$Date," ",3)[,3]
  Crimes$AMPM <- tolower(Crimes$AMPM) 
  
  ####################################################
  ### Split Date_new column into year, month, date ###
  ####################################################
  Crimes$Month <- str_split_fixed(Crimes$Date_new,"/",3)[,1]
  Crimes$Day <- str_split_fixed(Crimes$Date_new,"/",3)[,2]
  Crimes$Year <- str_split_fixed(Crimes$Date_new,"/",3)[,3]
  
  ####################################
  ### Create a new Datetime column ###
  ####################################
  Crimes$DateTime <- paste(Crimes$Year,"-",Crimes$Month,"-",
                           Crimes$Day," ",Crimes$Time," ",
                           Crimes$AMPM,sep = "")
  Crimes$DateTime <- as.POSIXct(Crimes$DateTime,tz = Sys.timezone(location = TRUE),
                                format = "%Y-%m-%d %H:%M:%S %p")

#################################################
### Define day/night column as the following: ###
###     Day: 6 - 18 (1)                       ###
###     night: Otherwise (0)                  ###
#################################################
Crimes$Hour <- hour(Crimes$DateTime)
Crimes$DayNight <- ifelse(Crimes$Hour <= 18 & Crimes$Hour >= 6,1,0)

######################
### Aggregate data ###
######################
Crimes$Date <- as.Date(Crimes$DateTime)
Crimes_aggregate <- aggregate(ID ~ Date + DayNight,data = Crimes,FUN = length)
colnames(Crimes_aggregate) <- c("Date","DayNight","Count")

###############################################################
### Create a complete data set that will include crime data ###
### and game data                                           ###
###############################################################
  Crimes_complete <- NULL
  targetDate <- seq.Date(from=as.Date("2001-01-01"), to=as.Date("2021-10-01"), by="day")
  DayNight <- rep(1,length(targetDate))
  Crimes_complete <- data.frame(targetDate,DayNight)
  colnames(Crimes_complete) <- c("Date","DayNight")
  Crimes_complete_night <- data.frame(targetDate,rep(0,length(targetDate)))
  colnames(Crimes_complete_night) <- names(Crimes_complete)
  Crimes_complete <- rbind(Crimes_complete,Crimes_complete_night)
  Crimes_complete <- left_join(Crimes_complete, Crimes_aggregate, 
                               by = c("Date","DayNight"))
  Crimes_complete[is.na(Crimes_complete)] <- 0
  
  ###################################
  ### Create the column week days ###
  ###################################
  Crimes_complete$WeekDay <- weekdays(Crimes_complete$Date)
  
  #####################################################
  ### Read game data and merge with Crimes_complete ###
  #####################################################
  load(paste(mydir,"/Data/chw_game_results.RData",sep = ""))
  all_games$Date <- format(as.POSIXct(all_games$Date,format='%A, %b %d'), format='%b %d')
  all_games$Date <- paste(all_games$Year,all_games$Date)
  all_games$Date <- as.Date(format(as.POSIXct(all_games$Date,format='%Y %b %d'),'%Y-%m-%d'))
  all_games$DayNight <- ifelse(all_games$D.N == "D",1,0)
  Crimes_complete <- left_join(Crimes_complete,all_games,
                               by = c("Date","DayNight"))
  
  ##############################
  ### column game day or not ###
  ### 1: game; 0 otherwise   ###
  ##############################
  Crimes_complete$Game <- ifelse(!is.na(Crimes_complete$W.L),1,0)
  Crimes_complete <-Crimes_complete[,c("Date","Count","DayNight",
                                       "Game","WeekDay")]
  Crimes_complete$WeekDay <- as.factor(Crimes_complete$WeekDay)
  
  ##########################
  ### Run the regression ###
  ##########################
  Crimes_complete_no_weird_time <- Crimes_complete[which(!year(Crimes_complete$Date) %in% c(2001,2020,2021)),]
  model.lm <- lm(Count ~  Game + WeekDay + DayNight,
                 data =Crimes_complete_no_weird_time)
  summary(model.lm)
  
  step.model <- stepAIC(model.lm, direction = "both", 
                        trace = FALSE)
  summary(step.model)
  



