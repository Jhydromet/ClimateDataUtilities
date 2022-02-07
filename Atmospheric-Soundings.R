# install.packages("RadioSonde")
#install.packages("plotly")
library(tidyverse)
library(RadioSonde)
library(lubridate)
library(plotly)


files <- dir(path = "TRARE_Field_Data", pattern = "(08).*\\TSPOTINT.txt$", recursive = T,full.names = T)

#files <- "TRARE_Field_Data/HuckleberryMine/08HM001/08HM001_20210921/08HM001_20210921_00_TSPOTINT.txt"


compile_soundings <- function(files) {
  launch.date <- str_split(files, pattern = "_",simplify = T) [5] 
  
  hd <- read_fwf(files,n_max = 1)
  
  d <- read_fwf(files, skip = 3, guess_max = 10000) %>% 
    mutate(X2 = paste(as.character(X2),X3)) %>% 
    select(!X3)
  
  d <- setNames(d,as.character(hd))
  
  dat <- d %>% 
    mutate(datetime = ymd_hms(paste(UTC_Date,UTC_Time)),
           launch.date = launch.date) %>% 
    select(datetime, !UTC_Date & !UTC_Time) %>% 
    rename(press = "Press",
           temp = "Temp",
           dewpt = "DP",
           rhum = "RelHum",
           wspd = "WSpeed",
           dir = "WDirn",
           dz = "Ascent",
           alt = "Alt_MSL",.
           )
}


dat <- files %>% 
  map(compile_soundings) %>% 
  reduce(rbind)

p <- dat %>% 
  ggplot()+
  geom_line(aes(FltTime, alt, colour = temp), size = 2)+
  scale_colour_viridis_c()+
  facet_grid(rows = vars(launch.date))

p



#plotsonde(dat)
#RadioSonde::PWV(dat)
#getAnywhere(PWV)
#getAnywhere(RadioSonde)

