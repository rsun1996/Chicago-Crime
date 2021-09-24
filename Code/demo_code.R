### set up working directory
mydir <- "C:/Users/peter/Documents/UTSA/Data Mining/Project-One-Chicago-Crime"
datadir <- paste(mydir,"/Data",sep = "")
output <- paste(mydir,"/Output",sep = "")

### Read Chiacago crimes csv
Chicago_crimes <- read.csv(paste(datadir,"/Crimes_-_2001_to_Present.csv",sep = ""))