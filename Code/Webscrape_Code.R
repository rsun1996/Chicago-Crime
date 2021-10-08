library(rvest) 
library(plyr)
library(dplyr)
library(textreadr)

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
