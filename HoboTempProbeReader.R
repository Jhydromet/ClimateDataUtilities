library(lubridate)
library(tidyverse)
library(plotly)

# You LUCKY DEVIL! Jeremy Morris Made you this script to read in all of the Hobo temperature logger data from the G: drive.

# This script opens every .csv file contained in the Raw Data folders in "G:\ResearchLogNet\Nechako Basin Research\Water Temperature Research\Field Work\ST Sites & Data"

# It then reads in the .csv (There are two different table structures depending on whether the file is downloaded from a phone or a PC), appends the site name (extracted from the site folder), formats the dates, takes a daily avg temperature, and appends all of the data from all sites into a longform dataframe.

# Do NOT Modify this script. As of 30-11-2021 it works properly to read all ST data. Any errors encountered while running this script likely are due to improper data formatting, or incorrect .csv files being present in the data folders.


# Name all the site folders

folder <- list.dirs(path = "G:/ResearchLogNet/Nechako Basin Research/Water Temperature Research/Field Work/ST Sites & Data",recursive = F)


# Find all the Raw .csv data files

file <- dir(paste0(folder,"/Data/Raw/"),"*.csv", full.names = T)

# file <- "G:/ResearchLogNet/Nechako Basin Research/Water Temperature Research/Field Work/ST Sites & Data/Whitesail Creek  Above Tahtsa Lake/Data/Raw/Whitesail Creek  2021-08-12.csv"
# file <- "G:/ResearchLogNet/Nechako Basin Research/Water Temperature Research/Field Work/ST Sites & Data/Endako River/Data/Raw/Endako River20893574 2021-08-23.csv"


# TWO CHALLENGES:
# 1. PARSE OUT WHICH DATA HAS WHAT TIMEZONE
# 2. CORRECT THE TIMESTAMPS TO PDT AKA GMT-07:00

# Datetime headers in the datasets:
# Date Time, GMT-0700
# Date Time, GMT-08:00 e.g. "corrected = datetime + hour(1)"
# Date Time, GMT+00:00 e.g. "corrected = datetime - hour(8)"


# Define a function to do everything


filegrabbr <- function(file){
  
  print(file)
  
  site <- gsub(".*ST Sites & Data/(.+)/Data.*", "\\1", file)
  
  dat <- read_csv(file)
  
  if(grepl(x = colnames(dat), pattern = "Serial", fixed = TRUE)){
    dat <- read_csv(file, skip = 2) %>% 
      mutate(if_any(.cols = contains("-08:00"))) %>% # to find -08:00 
      select(datetime = contains("Date"),
             temp = contains("Temp"))
    
    
    if(typeof(dat$datetime) == "character"){
      dat <- dat %>% 
        mutate(datetime = mdy_hms(datetime,truncated = 1))
    }
    
      dat <- dat %>% 
        mutate(site = site)
  }
  
  
  
  else{
    dat <- read_csv(file, skip = 1,  local = locale(encoding = "latin1")) %>% 
      select(datetime = contains("Date"),
             temp = contains("Temp")) %>% 
      mutate(datetime = mdy_hms(datetime,truncated = 1),
             site = site)
      
  }
  print(dat)
}

# Run the function, bind all data row-wise

dat <- file %>% 
  map(filegrabbr) %>% 
  reduce(bind_rows)

p <- dat %>% 
  ggplot()+
  geom_line(aes(datetime,temp,colour = site), size = .75)+
  labs(x = "Date", y = "Daily Mean Water Temperature °C")

p

#ggsave(plot = p, filename = "C:/Users/jmorris/Documents/Streamtemp.png", device = "png", dpi = 150)

#write_csv(dat, "G:\\ResearchLogNet\\Database Demo\\WhitesailST.csv")
ggplotly(p)

