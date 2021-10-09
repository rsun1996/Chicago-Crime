library(rvest) 
library(plyr)
library(dplyr)
library(textreadr)

### Reading in Crimes data set ################################################
Crimes <- read.csv("C:\\Users\\Sudo\\Desktop\\Crimes_2001_to_Present.csv")
###############################################################################

names(Crimes)

#Dropping unnecessary columns
Crimes <- Crimes[-c(1:2,4:5,7:8,11,13,15:17,19:22)] #Year is kept here on purpose in case it has a use on its own in the regression analysis later

# Splitting Date column into Date and Time
Crimes$Date_2 <- sapply(strsplit(as.character(Crimes$Date), " "), "[", 1)
Crimes$Time <- sapply(strsplit(as.character(Crimes$Date), " "), "[", 2)

#Deleting old date column
Crimes <- Crimes[-c(1)]

#Renaming new date column
names(Crimes)[7] <- 'Date'

#Reordering columns
Crimes <- Crimes[,c(7:8,1:6)]



########################### Web Scrapping of White Sox Games ###########################

#Get years
years <- c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
           "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

#Fill with the following loop. Data frame for all games.
all_games <- data.frame(Date=character(), WL=character(), DN=character(),
                        Attendance=integer(), Year=integer())

for(i in 1:length(years)){
  
  #Game schedule url
  scheduleurl <- paste0("https://www.baseball-reference.com/teams/CHW/",years[i],"-schedule-scores.shtml")
  
  #Read schedule url
  schedulehtml <- read_html(scheduleurl)
  
  #Schedule table from website
  schedule_results <- html_nodes(schedulehtml,"#team_schedule")
  
  #Create a table
  schedule_table <- html_table(schedule_results)
  
  #Schedule results into a data frame
  schedule_data <- data.frame(schedule_table)
  schedule_data <- schedule_data %>% dplyr::rename(Home_Away = 5)
  
  #Add year to each game
  schedule_data$Year <- years[i]
  
  #Keep Date, Home/Away, Opponent, Win/Loss, D/N, Attendance, and Year
  schedule_data <- schedule_data[, c(2,5,6,7,18,19,23)]
  
  #Make 'Attendance' and 'Year' numeric
  schedule_data$Attendance <- as.numeric(gsub(",","",schedule_data$Attendance))
  schedule_data$Year <- as.numeric(schedule_data$Year)
  
  #Remove column headers in data
  schedule_data <- schedule_data[which(schedule_data$Opp != "Opp"),]
  
  #Remove away games
  schedule_data <- schedule_data[which(schedule_data$Home_Away != "@"),]
  
  #Remove Home/Away column
  schedule_data <- schedule_data[, c(1,3,4,5,6,7)]
  
  #Putting all the data into all_games
  all_games <- rbind(all_games, schedule_data)
}

#save(all_games, file = "chw_game_results.RData")
#load(file = "chw_game_results.RData")

### All_games (White Sox games) data set ###########################
Games <- all_games
###############################################################################


###NOTES:######################################################################
#Chicago is organized into 77 community areas.

table(Crimes$Community.Area) #There are a total of 77 community areas in the Crimes data set.

#Guaranteed Rate Field (White Sox Stadium) is located in the Bridgeport neighborhood (COMMUNITY AREA 60).

#There are 43331 total crime reports in community area 60 specifically 
###############################################################################

#Matching Crimes data set and Games data set (by Date)

#Date-time conversion of Games date column: %A, %b %d TO %b %d (leaving out 'day of the week,')
Games$Date <- format(as.POSIXct(Games$Date,format='%A, %b %d'), format='%b %d')

#Concatenating columns involving dates (grouping month, day, and year into one column)
Games$Date_grouped <- paste(Games$Date, Games$Year)

# Date-time conversion of Games date column: %b %d %Y TO %m/%d/%Y
Games$Date_grouped <- format(as.POSIXct(Games$Date_grouped,format='%b %d %Y'), format='%m/%d/%Y')

#Deleting old date column
names(Games)
Games <- Games[-(1)]

#Renaming new date column
names(Games)[6] <- 'Date'

#Sorting columns so date is first
Games <- Games[, c(6, 1:5)]

#Now dates from Crimes and Games match#



#Inner Join (By matching dates - 1.7million)
# test <- merge(Crimes, Games, by = 'Date')

#Left Join with dplyr 
Crimes_Games <- left_join(Crimes, Games, by = c("Date"))


#Adding day of week column
Crimes_Games$Day <- format(as.POSIXct(Crimes_Games$Date,format='%m/%d/%Y'), format='%A')


#Addition of new column, game day, which is 1 for TRUE (Date was a game day) and 0 for FALSE (Date was not a game day).

Crimes_Games$Gameday[Crimes_Games$Attendance >= 0] <- 1
Crimes_Games$Gameday[is.na(Crimes_Games$Attendance)] <- 0


#Attempt at aggregating (Counting) of crimes according to individual dates
#dff <- aggregate(x ~ Year + Month, data = df1, sum)



