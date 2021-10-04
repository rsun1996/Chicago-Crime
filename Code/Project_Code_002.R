library("dplyr")

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


### Reading in all_games (White Sox games) data set ###########################
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


#Not sure what this is
# dfff <- aggregate(x ~ Year + Month, data = df1, sum)



