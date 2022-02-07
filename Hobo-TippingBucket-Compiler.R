library(tidyverse)
library(lubridate)
library(plotly)
library(ggplot2)
#install.packages("patchwork")
library(patchwork)

setwd("Z:/TRARE")

files <- dir(path = "TRARE_DATA", pattern = "(06).*\\.csv$", recursive = T,full.names = T)

files <- files[! files %in% c('TRARE_DATA/Kemano/04KM001/20211006_TRARE_Parsivel1_1.csv',
                              'TRARE_DATA/Kemano/04KM001/20211106_TRARE_Parsivel1_1.csv')]

# set dates for inset
start.ref.date <- ymd_hms("2021-09-20 00:00:00 UTC")
end.ref.date <- ymd_hms("2021-09-23 00:00:00 UTC")


# files <- paste0("TRARE_Field_Data/","MountSweeney/06MS002/06MS002_20210910.csv")



compile.hobo <- function(files) {
  site <- str_split(files, pattern = "/",simplify = T)[3]
  
  colnums <- read_csv(files, skip = 2, n_max = 1)
  
  if( ncol(colnums) == 4){
  
  dat <- read_csv(files, skip = 2, col_names = c("record","datetime","temp","rain")) %>% 
    mutate(datetime = mdy_hms(datetime),
           site = site) %>% 
    fill(rain) %>% 
    select(!record)
  
  }  else {
    
  dat <- read_csv(files, skip = 2, col_names = c("record","datetime","rain")) %>% 
    mutate(datetime = mdy_hms(datetime),
           site = site,
           temp = NA) %>% 
    fill(rain) %>% 
    select(!record)}
  
}


hobo.dat <- files %>% 
  map(compile.hobo) %>% # apply function to all filenames
  reduce(bind_rows) %>%  # then bind it all together into one data frame
  group_by(site) %>% 
  mutate(rain.event = lead(rain) - rain, # create non accumulated column
         rain.event = replace(rain.event, rain.event <0, 0), # fix negatives from last step
         HOBO_Rain = cumsum(rain.event))%>% # re accumulate precip
  mutate(datetime = ceiling_date(datetime, "5 min")) # round up datetimes to 5 min intervals

# create complete time sequence
cor.dates <- tibble(datetime = seq.POSIXt(from = head(hobo.dat$datetime,n = 1), 
                                           to = tail(hobo.dat$datetime,n = 1), by = "5 mins"))

# remove duplicate timestamps from data file
hobo.dat <- subset(hobo.dat, hobo.dat$datetime != lead(hobo.dat$datetime))


# join the complete time sequence to the main data file
hobo.dat <- full_join(hobo.dat, cor.dates, by = "join") %>% 
  arrange(datetime) %>% 
  #fill(HOBO_Rain) #fill gaps in precip using previous values

# plot for duration of TRARE

p1 <- hobo.dat %>%
  ggplot()+
  geom_line(aes(datetime, HOBO_Rain, colour = site), size=0.75)+
  labs(y = "Rain (mm)", x = "Date", title = "Precipitation in the Nechako Watershed")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_discrete(name="Site")

ggplotly(p1)

# create df and plot for specific reference dates

# set precip to zero at ref date
ref.date.precip <- hobo.dat %>%
  group_by(site) %>% 
  filter(datetime == start.ref.date) %>%
  distinct() %>% 
  select(site,ref.precip = HOBO_Rain)


hobo.ref.dat <- full_join(ref.date.precip,hobo.dat)

hobo.ref.dat <- hobo.ref.dat %>% 
  filter(between(datetime,start.ref.date,end.ref.date)) %>% 
  mutate(Rain = HOBO_Rain - ref.precip)


p <- hobo.ref.dat %>% 
  ggplot()+
  geom_line(aes(datetime,Rain, colour = site), size=0.25)+
  #labs(y = "Rain (mm)", x = "Date")#+
  scale_color_discrete(name="Site")+
  theme(legend.position = "None", axis.title = element_blank())

# create plot for SAB slides

p3 <- hobo.ref.dat %>%
  ggplot()+
  geom_line(aes(datetime,Rain,colour = site), size=0.75)+
  labs(y = "Precipitation (mm)", x = "Date", title = "Cumulative Tipping Bucket Precipitation")+
  scale_color_discrete(name="Site")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("precipSAB.png", width = 5.4, height = 5.7, dpi = 300)

ggplotly(p3)

# inset plots

p2 <- p1 +
  inset_element(p = p,
                left = 0.02,
                bottom = 0.4,
                right = 0.5,
                top = 0.98)
p2


ggsave("AKplot.png", dpi = 300)

