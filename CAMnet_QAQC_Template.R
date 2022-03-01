library(tidyverse)
library(lubridate)
library(readxl)
library(plotly)
library(zoo) # for roll_mean function
library(maptools) # for sunriset function

# $$$ - search for these dollar signs for sections of the script that must be adapted to new sites

# another tip: use Ctrl+Shift+C to comment out/in entire blocks of code

# $$$

timezone <- "Etc/GMT+7" # This means GMT-7 (PDT)


# Scour data directory for all file names for data reader function

# $$$ 
 
sitename <- "TatukLake"

d <- dir("Campbell",full.names = T)

folder <- d[str_detect(d,pattern = sitename)]

files <- list.files(path = folder,recursive = T,  pattern = "*.xlsx", full.names = T)

sitefile <- str_split(files[1],"/", simplify = T)[2]


# The Campbell data reader Function ---------------------------------------------

site_compiler <- function(files) {
  
  dat <- read_excel(files)
  meta <- dat[1:2,] # Set the number of columns the metadata makes up
  dat <- dat %>% 
    slice(-(1:2)) %>% 
    mutate(Timestamp = as.POSIXct(as.numeric(Timestamp) * (60*60*24),
                                  origin="1899-12-30 08:00:00")) %>%
    mutate(across(!Timestamp, as.numeric)) 
  
  thelist <- list(dat,meta) # Output both meta and data
  names(thelist) <- c("dat","meta")
  return(thelist)
  
}



# Running the excel compiler ----------------------------------------------
# extracting metadata and station data

alloutput <- map(files, site_compiler)

meta <- map_dfr(alloutput[1], "meta") %>% # Extract and rename metadata row
  select(any_of(c("Timestamp", "BattV", "AirTC","AirTC_Avg", "AirTC_2","RH", "RH_2", "Pressure", "BP_mbar","Rain_Tot", "WS_Avg", "WS_Std", "WS_2_Avg", "WS_2_Std", "WindDir", "WindDir_Avg", "WindDir_2_Avg", "DBTCDT", "T107","T107_1", "T107_2", "T107_3", "T109","T109_1","T109_2","T109_3","T109_C","T109_C_1","T109_C_2","T109_C_3", "SlrkJ_Tot", "SlrW_Avg","Solar_Avg", "Solar_Std","Solar_Tot", "SBT_C_Avg", "TT_C_Avg","VW", "PA","VW_1", "PA_1", "VW_2", "PA_2", "VW_3", "PA_3"))) %>% 
  select(!any_of(c("WindDir_Avg","WindDir_2_Avg"))) %>% 
  mutate(across(.cols = everything(), as.character)) %>% # set the metadata columns all to characters (not the dat)
  rename(Pressure = "BP_mbar", # use this rename section when needed
         Solar_W = "SlrW_Avg",
         Solar_kJ_Tot = "SlrkJ_Tot") %>% 
  rename_with(~str_remove(string = ., pattern = "_Avg")) %>%  #drop _avg from all cols 
  rename_with(~str_remove(string = ., pattern = "_C")) #drop _avg from all cols  

# MUST REMOVE WindDir_Avg! This is an incorrect measurement and should be ignored

# If encounter BP_mbar, use rename(Pressure = "BP_mbar")

  
dat <- map(alloutput, "dat") %>% # Extract data and compile
  reduce(full_join) %>% 
  select(any_of(c("Timestamp", "BattV", "AirTC","AirTC_Avg", "AirTC_2","RH", "RH_2", "Pressure", "BP_mbar","Rain_Tot", "WS_Avg", "WS_Std", "WS_2_Avg", "WS_2_Std", "WindDir", "WindDir_Avg", "WindDir_2_Avg", "DBTCDT", "T107","T107_1", "T107_2", "T107_3", "T109","T109_1","T109_2","T109_3","T109_C","T109_C_1","T109_C_2","T109_C_3", "SlrkJ_Tot", "SlrW_Avg","Solar_Avg", "Solar_Std","Solar_Tot", "SBT_C_Avg", "TT_C_Avg","VW", "PA","VW_1", "PA_1", "VW_2", "PA_2", "VW_3", "PA_3"))) %>% 
  select(!any_of(c("WindDir_Avg","WindDir_2_Avg")))  %>%
  rename(Pressure = "BP_mbar", # use this rename section when needed
         Solar_W = "SlrW_Avg",
         Solar_kJ_Tot = "SlrkJ_Tot") %>% 
  rename_with(~str_remove(string = ., pattern = "_Avg")) %>%  #drop _avg from all cols 
  rename_with(~str_remove(string = ., pattern = "_C")) #drop _avg from all cols  


# MUST REMOVE WindDir_Avg! This is an incorrect measurement and should be ignored

# Timestamp Corrections ---------------------------------------------------


start_date <- head(dat$Timestamp,n=1) # extract start and end timestamps from the data
end_date <- tail(dat$Timestamp,n=1)

times <- tibble(Timestamp = seq(from = start_date, to = end_date, by = "15 min")) # create complete timesequence (eg no missing dates)

timecor <- full_join(times,tibble(Timestamp = dat$Timestamp)) # join the complete timesequence to the dataset

dat.cor <- full_join(times,dat) %>% 
  mutate(across(.cols = !Timestamp,~ifelse(is.na(.), NaN, .))) # change all NA to NAN (NAN is standard from our loggers)

tz(dat.cor$Timestamp) <- timezone



# Solar data tester -------------------------------------------------------

# calculate sunlight hours using astronomical dawn and dusk (sun is 18degrees below horizon)

# $$$ 

loc <- read_csv("CAMnet_Sites.csv") %>% # get lat long from camnet map data (csv exported from qgis)
  filter(Name == "Tatuk Lake")
crds =  matrix(c(loc$Lon,loc$Lat), nrow = 1)

# find dawn and dusk

dawn <- unique(crepuscule(crds, proj4string=CRS("+proj=longlat +datum=WGS84"), dat.cor$Timestamp+ days(1), direction = "dawn", POSIXct.out = TRUE, solarDep = 6))

dusk <- unique(crepuscule(crds, proj4string=CRS("+proj=longlat +datum=WGS84"), dat.cor$Timestamp, direction = "dusk", POSIXct.out = TRUE, solarDep = 6))

# create time intervals to represent nighttime each day
nighttime <- as.list(interval(dusk$time, dawn$time, tzone = timezone))


# Flagging section --------------------------------------------------------
# These are the QAQC flag rules. Each case_when function holds logical statements that determine whether the data Fails "F" or Passes "P"

# Most standard sensors except for soil temperature and soil moisture are flagged

cor <- dat.cor  %>%
  mutate(AirTC_flag     = case_when(!between(AirTC, -50, 50) ~ "F", # typical T sensor range
                                    AirTC == NaN ~ "F", # flag any NaNs
                                    TRUE ~ "P"),
         RH_flag        = case_when(!between(RH, 0, 100) ~ "F", # RH between 0 and 100%
                                    RH == NaN ~ "F",
                                    TRUE ~ "P"),
         Pressure_flag  = case_when(!between(Pressure, 700, 1050) ~ "F", # typical airpressure range
                                    Pressure == NaN ~ "F",
                                    TRUE ~ "P"),
         WS_flag        = case_when(!between(WS, 0, 20) ~ "F", # Determine reasonable upper limit from data
                                     WS == NaN ~ "F",
                                    rollmean(WS,k = 48, align = "left", fill = NA) == rollmean(lead(WS, 48), k = 48, align = "left", fill = NA)
                                    & rollmean(AirTC, k = 96, align = "right", fill = NA) <= 0 
                                    & abs(lead(WS)-WS) == 0 ~ "F",
                                    rollmean(WS,k = 48, align = "right", fill = NA) == rollmean(lag(WS, 48), k = 48, align = "right", fill = NA) 
                                    & rollmean(AirTC, k = 96, align = "right", fill = NA) <= 0 
                                    & abs(lead(WS)-WS) == 0  ~ "F",
                                     TRUE ~ "P"),
         WindDir_flag   = case_when(!between(WindDir, 0, 360) ~ "F", # Wind direction between 0 and 360
                                    WS_flag == "F" ~ "F",
                                    WindDir == NaN ~ "F",
                                    TRUE ~ "P"),
         Rain_Tot_flag  = case_when(!between(Rain_Tot, 0, 40) ~ "F", # Determine reasonable max rain event from data
                                    Rain_Tot == NaN ~ "F",
                                    TRUE ~ "P"),
         DBTCDT_flag    = case_when(abs(lead(DBTCDT)-DBTCDT) > 10 ~ "F", # flag large changes between timestamps
                                    abs(lag(DBTCDT)-DBTCDT) > 10 ~ "F",
                                    !between(DBTCDT, -10, 110) ~ "F", # determine reasonable max min range from data
                                    DBTCDT == NaN ~ "F",
                                    TRUE ~ "P"),
         T107_flag     = case_when(!between(T107, -50, 50) ~ "F", # typical T sensor range. If any additional T107s, add them here with their siginifier (eg. T107_2, or T107_3)
                                   T107 == NaN ~ "F", # flag any NaNs
                                   TRUE ~ "P"),
         T109_flag     = case_when(!between(T109, -50, 50) ~ "F", 
                                   T109 == NaN ~ "F", # flag any NaNs
                                   TRUE ~ "P"),
         T109_2_flag     = case_when(!between(T109_2, -50, 50) ~ "F", 
                                     T109_2 == NaN ~ "F", # flag any NaNs
                                     TRUE ~ "P"),
         T109_3_flag     = case_when(!between(T109_3, -50, 50) ~ "F", 
                                     T109_3 == NaN ~ "F", # flag any NaNs
                                     TRUE ~ "P"),
         Solar_flag = case_when(Solar != 0 && Timestamp %within% nighttime ~ "F", # if solar values recorded at nighttime (above code chunk)
                                    Solar == NaN ~ "F",
                                    TRUE ~ "P")
           )



# Functional Plotting Section ---------------------------------------------
# reorient the data to longform with flag column

flags <- cor %>% 
  select(Timestamp, contains("flag")) %>% 
  pivot_longer(!Timestamp, values_to = "flag")

flags$name <- str_remove_all(flags$name, pattern = "_flag")

flag.dat <- cor %>% 
  select(Timestamp, !contains("flag")) %>% 
   pivot_longer(!Timestamp)

flag.dat <- full_join(flag.dat,flags) %>% 
  mutate(flag = replace_na(flag, "N"))

# Plot All with flags

# p <- flag.dat %>%
#   ggplot()+
#   geom_line(data = flag.dat ,aes(Timestamp,value, colour = name))+
#   geom_point(data = filter(flag.dat, flag == "F"), aes(Timestamp, value))+
#   facet_grid(rows = vars(name),scales = "free")
# 
# p

# interactive plot of single variable

plotParameter = "WS"

p <- flag.dat %>%
  ggplot()+
  geom_line(data = filter(flag.dat, name == plotParameter) ,aes(Timestamp,value, colour = name))+
  geom_point(data = filter(flag.dat, flag == "F" & name == plotParameter), aes(Timestamp, value))

ggplotly(p)

# Plot only passing data

# p <- flag.dat %>%
#   filter(flag == "P") %>% 
#   ggplot()+
#   geom_line(aes(Timestamp,value, colour = name))+
#   facet_grid(rows = vars(name),scales = "free")
# 
# p

# interactive plot - Probably really slow
# ggplotly(p)

# save the plot with all data and flags included.

ggsave(p, path = paste0("For Submission/", sitefile), filename = paste0(sitename,".png"), device = "png", height = 12, units = "in")




# Add metadata back in, print csv -----------------------------------------


cor.fin <- cor %>% 
  mutate(across(.cols = everything(), as.character)) %>%
  add_row(meta, .before = 1) %>% 
  rename(SnowDepth = "DBTCDT")

write_csv(cor.fin, paste0("For Submission/", sitefile,"/",sitefile,".csv"))
