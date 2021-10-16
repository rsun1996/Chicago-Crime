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
Crimes <- Chicago_crimes[which(Chicago_crimes$Community.Area %in% c(34,60,37,61,59,31,28,33,35,38)),]
write.csv(Crimes,paste(output,"/Data/Chicago_crimes_all_target_area.csv",sep = ""),row.names = FALSE)
Crimes <- read.csv(paste(output,"/Data/Chicago_crimes_all_target_area.csv",sep = ""))

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
Crimes$Hour <- lubridate::hour(Crimes$DateTime)
Crimes$DayNight <- ifelse(Crimes$Hour <= 18 & Crimes$Hour >= 6,1,0)

#########################################
### Add new variable: community area: ###
### 34 & 60: 1                        ###
### Others: 0                         ###
#########################################
Crimes$Area <- ifelse(Crimes$Community.Area %in% c(34,60),1,0)

######################
### Aggregate data ###
######################
Crimes$Date <- as.Date(Crimes$DateTime)
Crimes_aggregate <- aggregate(ID ~ Date + DayNight + Area,data = Crimes,FUN = length)
colnames(Crimes_aggregate) <- c("Date","DayNight","Area","Count")

###############################################################
### Create a complete data set that will include crime data ###
### and game data                                           ###
###############################################################
Crimes_complete <- NULL
targetDate <- seq.Date(from=as.Date("2001-01-01"), to=as.Date("2021-10-01"), by="day")
DayNight <- c(0,1)
Area <- c(0,1)
for(i.DayNight in 1:length(DayNight)){
  for(i.Area in 1:length(Area)){
      i.Crimes_complete <- NULL
      i.Crimes_complete <- data.frame(targetDate,rep(Area[i.Area],length(targetDate)),
                                      rep(DayNight[i.DayNight],length(targetDate)))
      colnames(i.Crimes_complete) <- c("Date","Area","DayNight")
      Crimes_complete <- rbind(Crimes_complete,i.Crimes_complete)}
}
Crimes_complete <- left_join(Crimes_complete, Crimes_aggregate, 
                              by = c("Date","DayNight","Area"))
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
                               by = c("Date"))
  
##############################
### column game day or not ###
### 1: game; 0 otherwise   ###
##############################
Crimes_complete$Game <- ifelse(!is.na(Crimes_complete$W.L),1,0)
Crimes_complete <-Crimes_complete[,c("Date","Count","DayNight.x",
                                       "Game","WeekDay","Area")]
Crimes_complete_no_weird_time <- Crimes_complete[which(!lubridate::year(Crimes_complete$Date) %in% c(2001,2020,2021)),]
Crimes_complete_no_weird_time <- Crimes_complete_no_weird_time[which(!duplicated(Crimes_complete_no_weird_time)),]
  
#########################################
### Change data set: Weekday and game ###
#########################################
Crimes_complete_no_weird_time$WeekDay <- ifelse(Crimes_complete_no_weird_time$WeekDay %in% c("Monday","Tuesday",
                                                                                             "Wednesday","Thursday",
                                                                                             "Friday"),1,0)
colnames(Crimes_complete_no_weird_time) <- c("Date","Count","DayNight",
                                             "Game","Weekday","Area")
#write.csv(Crimes_complete_no_weird_time,paste(output,"/Data/Crimes_complete_no_weird_time.csv",sep = ""))
Crimes_complete_no_weird_time$Game <- as.factor(Crimes_complete_no_weird_time$Game)
Crimes_complete_no_weird_time$Weekday <- as.factor(Crimes_complete_no_weird_time$Weekday)
Crimes_complete_no_weird_time$DayNight <- as.factor(Crimes_complete_no_weird_time$DayNight)
  
##########################
### Run the regression ###
##########################
model.lm <- glm(Count ~  Game + Weekday + DayNight + DayNight*Game + Weekday*Game + Weekday*DayNight 
               + Weekday*DayNight*Game,
              data =Crimes_complete_no_weird_time,
              family = "poisson")
summary(model.lm)
anova(model.lm)
  
##################################
### Including Interaction term ###
##################################
model.lm.interact <- lm(Count ~  Game + WeekDay + DayNight + DayNight*Game +
                   Game*WeekDay + WeekDay*DayNight,
                 data =Crimes_complete_no_weird_time)
summary(model.lm.interact)
step.model <- stepAIC(model.lm, direction = "both", 
                        trace = FALSE)
summary(step.model)
  



