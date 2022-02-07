library(tidyverse)
library(lubridate)
library(readxl)
library(plotly)
library(maptools) # for sunriset function


sitename <- "LowerCastleCreek"

d <- dir("Campbell",full.names = T)

folder <- d[str_detect(d,pattern = sitename)]

files <- list.files(path = folder,recursive = T,  pattern = "*.xlsx", full.names = T)

sitefile <- str_split(files[1],"/", simplify = T)[2]


# files <- "Campbell/7.LowerCastleCreekGlacier/LowerCastleCreek_2017.xlsx"

# The Campbell data reader Function ---------------------------------------------

site_compiler <- function(files) {
  
  dat <- read_excel(files)
  meta <- dat[1,]
  dat <- dat %>% 
    slice(-(1:2)) %>% 
    mutate(Timestamp = as.POSIXct(as.numeric(Timestamp) * (60*60*24),
                                  origin="1899-12-30 08:00:00")) %>%
    mutate(across(!Timestamp, as.numeric)) 
  
  thelist <- list(dat,meta) # Output both meta and data
  names(thelist) <- c("dat","meta")
  return(thelist)
  
}

# Variable Names
#  "Timestamp"     "Record"        "BattV"         "AirTC"         "RH"           
#"AirTC_2"       "RH_2"          "DT"            "TCDT"          "DBTCDT"       
#"WS_Avg"        "WS_Std"        "WindDir_Avg"   "WindDir_Std"   "WS_2_Avg"     
#"WS_2_Std"      "WindDir_2_Avg" "WindDir_2_Std" "T107"          "T107_2"       
#"T107_3"        "Solar_Avg"     "Solar_Std"     "Solar_Tot"     "Pressure"     
#"SBT_C_Avg"     "TT_C_Avg"      "Rain_Tot"   


# Run the function

alloutput <- map(files, site_compiler)

meta <- map_dfr(alloutput[1], "meta") %>% # Extract and rename metadata row
  select(any_of(c("Timestamp", "BattV", "AirTC", "AirTC_2","RH", "RH_2", "Pressure", "Rain_Tot", "WS_Avg", "WS_Std", "WS_2_Avg", "WS_2_Std", "WindDir_Avg", "WindDir_2_Avg", "DBTCDT", "T107", "T107_2", "T107_3", "T109", "Solar_Avg", "Solar_Std", "SBT_C_Avg", "TT_C_Avg"))) %>% 
  rename(WindDir = "WindDir_Avg",
         WindDir_2 = "WindDir_2_Avg")
  
dat <- map(alloutput, "dat") %>% # Extract data and compile
  reduce(full_join) %>% 
  select(any_of(c("Timestamp", "BattV", "AirTC", "AirTC_2","RH", "RH_2", "Pressure", "Rain_Tot", "WS_Avg", "WS_Std", "WS_2_Avg", "WS_2_Std", "WindDir_Avg", "WindDir_2_Avg", "DBTCDT", "T107", "T107_2", "T107_3", "T109", "Solar_Avg", "Solar_Std", "SBT_C_Avg", "TT_C_Avg", "file")))

# Correct timestamps

start_date <- head(dat$Timestamp,n=1)
end_date <- tail(dat$Timestamp,n=1)

times <- tibble(Timestamp = seq(from = start_date, to = end_date, by = "15 min"))

timecor <- full_join(times,tibble(Timestamp = dat$Timestamp))

dat.cor <- full_join(times,dat) %>% 
  rename(WindDir = "WindDir_Avg",
         WindDir_2 = "WindDir_2_Avg")

tz(dat.cor$Timestamp) <- "Etc/GMT+7"

# calculate sunlight hours using astronomical dawn and dusk (sun is 18degrees below horizon)

loc <- read_csv("CAMnet_Sites.csv") %>% 
  filter(Name == "Lower Castle")
crds =  matrix(c(loc$Lon,loc$Lat), nrow = 1)

dawn <- crepuscule(crds, dat.cor$Timestamp, direction = "dawn", POSIXct.out = TRUE, solarDep = 18)
dusk <- crepuscule(crds, dat.cor$Timestamp, direction = "dusk", POSIXct.out = TRUE, solarDep = 18)

sun <- interval(dawn$time, dusk$time, tzone = "Etc/GMT+7") # remember this actually means GMT-7

# Minor data corrections, select variables for publication
# !! These are general functions that can be applied to most stations.


cor <- dat.cor  %>%
  mutate(DBTCDT_flag    = case_when(abs(lead(DBTCDT)-DBTCDT) > 10 ~ "F",
                                     !between(DBTCDT, -10, 280) ~ "F",
                                     DBTCDT == NA ~ "F",
                                     TRUE ~ "G"),
         WS_Avg_flag        = case_when(!between(WS_Avg, 0, 20) ~ "F",
                                     WS_Avg == NA ~ "F",       
                                     TRUE ~ "G"),
         WS_2_Avg_flag      = case_when(!between(WS_2_Avg, 0, 20) ~ "F",
                                    WS_2_Avg == NA ~ "F",
                                    TRUE ~ "G"),
         WindDir_flag   = case_when(!between(WindDir, 0, 360) ~ "F",
                                    WindDir == NA ~ "F",
                                    TRUE ~ "G"),
         WindDir_2_flag = case_when(!between(WindDir_2, 0, 360) ~ "F",
                                    WindDir_2 == NA ~ "F",
                                    TRUE ~ "G"),
         Pressure_flag  = case_when(!between(Pressure, 700, 1050) ~ "F",
                                    Pressure == NA ~ "F",
                                    TRUE ~ "G"),
         RH_flag        = case_when(!between(RH, 0, 100) ~ "F",
                                    RH == NA ~ "F",
                                   TRUE ~ "G"),
         RH_2_flag      = case_when(!between(RH_2, 0, 100) ~ "F",
                                    RH_2 == NA ~ "F",
                                    TRUE ~ "G"),
         AirTC_flag     = case_when(!between(AirTC, -50, 50) ~ "F",
                                    AirTC == NA ~ "F",
                                    TRUE ~ "G"),
         AirTC_2_flag   = case_when(!between(AirTC_2, -50, 50) ~ "F",
                                    AirTC_2 == NA ~ "F",
                                    TRUE ~ "G"),
         Rain_Tot_flag  = case_when(!between(Rain_Tot, 0, 40) ~ "F",
                                    Rain_Tot == NA ~ "F",
                                    TRUE ~ "G"),
         Solar_flag = case_when(!(Timestamp %within% sun) & Solar_Avg != 0 ~ "F",
                                    TRUE ~ "G")
           )

flags <- cor %>% 
  select(Timestamp, contains("flag")) %>% 
  pivot_longer(!Timestamp, values_to = "flag")


flags$name <- str_remove_all(flags$name, pattern = "_flag")

flag.dat <- cor %>% 
  select(Timestamp, !contains("flag")) %>% 
  pivot_longer(!Timestamp)

flag.dat <- full_join(flags,flag.dat)         


# Plot All

p <- flag.dat %>%
  ggplot()+
  geom_line(data = filter(flag.dat,name == "Solar_Avg") ,aes(Timestamp,value, colour = name))+
  geom_point(data = filter(flag.dat, flag == "F" & name == "Solar_Avg"), aes(Timestamp, value))+
  facet_grid(rows = vars(name),scales = "free")

p

ggplotly(p)


ggsave(p, path = paste0("For Submission/", sitefile), filename = paste0(sitename,".png"), device = "png")

# Final Output

cor.fin <- cor %>% 
  mutate(across(.cols = everything(), as.character)) %>% 
  add_row(meta, .before = 1) %>% 
  rename(SnowDepth = "DBTCDT")

write_csv(cor.fin, paste0("For Submission/", sitefile,"/",sitefile,".csv"))
