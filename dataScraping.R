# R packages
library(tidyverse)
library(rvest)

# scraping and cleaning function
RussData <- function(Year){ # takes in the season end year
  RussURL <- paste("https://www.basketball-reference.com/players/w/westbru01/gamelog/", Year, "/", sep = "") # gets url
  RussHTML <- read_html(RussURL) # grab all content from the raw html file
  RussStats <- html_table(RussHTML, fill = TRUE)[[8]] # parse html code into data tables, use index to get our desired table
  
  names(RussStats)[6] <- "side"  # rename columns
  names(RussStats)[8] <- "result"
  RussStats <- filter(RussStats, GS == "1") # filter out games he didn't play
  RussStats[RussStats == "CHO"] <- "CHA" # correct abbreviation for Charlotte
  
  # Western Conference teams
  WestTeams <- c("LAL", "UTA", "LAC", "DEN", "DAL", "HOU", "OKC", "MEM", "SAS", "PHO", "POR", "NOP", "SAC", "MIN", "GSW")
  
  # make sure all the stats are of numeric type
  for (i in c(11:ncol(RussStats))){
    RussStats[,i] <- as.numeric(RussStats[,i])
  }
  
  # transforming the table
  RussStats <- RussStats %>% 
    separate(MP, into = c("Min", "Sec")) %>%  # separate the minutes variable (min:sec)
    mutate(Season = paste(Year - 1, "-", Year, sep = ""), # create season variable
           Side = ifelse(side == "@", "Away", "Home"), # which side OKC is
           Result = ifelse(grepl("W", result), "Win", "Loss"), # game result for OKC
           OppConf = ifelse(Opp %in% WestTeams, "West", "East"), # opponent's conference
           Minutes = round(as.integer(Min) + as.integer(Sec)/60, 2), # playing time in minutes
           TripDbl = ifelse(PTS >= 10 & AST >= 10 & TRB >= 10, "Yes", "No")) %>%  # triple-double
    select(Season, Result, Side, Opp, OppConf, Minutes, FG, FGA, `FG%`, `3P`, `3PA`, `3P%`, FT, FTA, `FT%`, ORB, DRB, TRB, PTS, AST, TripDbl, STL, BLK, TOV, PF, `+/-`)
}

# combine the seasons into one table
RussStats <- RussData(2017)  # data from first year, initialize full data 
for (Year in 2018:2019) {
  RussStats <- RussStats %>% 
    full_join(RussData(Year)) # full_join to combine all the tables
}
