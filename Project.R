# DS 340H
# Capstone Project
# Yildiz Derinkok
# Fall 2024

setwd("/Users/ozgecankocak/Downloads/Fall 24/DS 340H/Project")
library(lubridate)
library(dplyr)
library(tidyr)


############## Create data frame ##############

# Load data
df_act <- read.delim("atusact_0323.dat", sep=",")
df_resp <- read.delim("atusresp_0323.dat", sep=",")
df_ros <- read.delim("atusrost_0323.dat", sep=",")
df_cps <- read.delim("atuscps_0323.dat", sep=",")

# Get variables of interest from roster file ???????????????? (rerun df_ros efore running this code)
#df_household <- df_ros %>%
#  select(TUCASEID, TULINENO, TEAGE) %>%
#  group_by(TUCASEID, TULINENO) %>%
#  spread(TULINENO, TEAGE) %>%
#  rowwise() %>%
#  mutate(Over65 = if_else(any(across(everything()) > 64), 1, 0)) %>%
#  mutate(Under13 = if_else(any(across(everything()) < 14), 1, 0))

#df_ros <- merge(df_ros, df_household, by="TUCASEID") #add the new vars to df_ros


# Create Data frames with variables of interest
df_act <- df_act[c("TUCASEID", "TEWHERE", "TUACTDUR24", "TUSTARTTIM", "TUSTOPTIME", "TUCUMDUR24")]
df_resp <- df_resp[c("TUCASEID", "TUDIARYDATE", "TELFS", "TRDTOCC1", "TROHHCHILD", "TESCHENR")]
df_cps <- df_cps[df_cps$TULINENO == 1,] #only get interview respondents
df_cps <- df_cps[c("TUCASEID", "TULINENO", "PEMARITL", "GESTFIPS", "GEREG", "HRNUMHOU")]
#length(unique(df_cps$TUCASEID)) #cps data has more households than act, resp, and ros (they have 245139)
df_ros <- df_ros[df_ros$TULINENO == 1,] #only get interview respondents
df_ros <- df_ros[c("TUCASEID", "TEAGE", "TESEX")]

# Merge data frames
df_merge1 <- merge(df_act, df_resp, by="TUCASEID") 
df_merge2 <- merge(df_merge1, df_ros, by="TUCASEID")
df <- merge(df_merge2, df_cps, by="TUCASEID")


############## Data Cleaning ###############

# Clean date variables
df$TUSTARTTIM <- hms(df$TUSTARTTIM)
df$TUSTOPTIME <- hms(df$TUSTOPTIME)
df$TUDIARYDATE <- ymd(df$TUDIARYDATE)

# Only keep activities where people are outside the home
df <- df[df$TEWHERE != 1,] #1: at home or yard
df <- df[df$TEWHERE != -1,] #-1: where info not collected

# Calculate total time spent outside for each person
df <- df %>%
  group_by(TUCASEID) %>%
  mutate(TotalMins = sum(TUACTDUR24))

df$TotalHrs = df$TotalMins / 60


### Add new Variables ###

#Year, year-month, month
df$Year <- format(df$TUDIARYDATE, "%Y") #Variable for year
df$YearMo <- ym(format(df$TUDIARYDATE, "%Y-%m")) #Variable for year-month
df$Month <- as.numeric(format(df$TUDIARYDATE, "%m"))

#Day variable: subtract the first day from all the days (as.Date, specify origin)
df$Days <- days(df$TUDIARYDATE) - days(as.Date("2002-12-31"))
df$Days <- day(df$Days)

#Day^2 variable
df$Days2 <- (df$Days)^2

#Indicator for before and after Covid (march 13, 2020)
df$PostCovid <- if_else(df$TUDIARYDATE > "2020-03-13", 1, 0)

#Seasonal indicator
df$Season <- "winter"
df$Season[df$Month>=3 & df$Month<6] <- "spring"
df$Season[df$Month>=6 & df$Month<9] <- "summer"
df$Season[df$Month>=9 & df$Month<12] <- "fall"

#Day of the week
df$DayofWeek <- wday(df$TUDIARYDATE, label = TRUE)
df$Weekend <- if_else((df$DayofWeek == "Sat" | df$DayofWeek == "Sun"), 1, 0)

#Holiday Indicator ?



### Create Subsets ###

# Get dataset for each person
d <- df %>%
  select (-c(TEWHERE, TUACTDUR24, TUSTARTTIM, TUSTOPTIME, TUCUMDUR24)) %>%
  distinct()
#length(unique(df$TUCASEID)) # 208715 #check if there is a row in d for each unique case id


# Dataset with median time by date
d_date <- d %>%
  group_by(TUDIARYDATE) %>%
  mutate(MedMins = median(TotalMins)) %>%
  mutate(MedHrs = median(TotalHrs)) %>%
  select(TUDIARYDATE, MedMins, MedHrs) %>% 
  distinct()

# Dataset with median time by YearMo
d_yearmo <- d %>%
  group_by(YearMo) %>%
  mutate(MedMins = median(TotalMins)) %>%
  mutate(MedHrs = median(TotalHrs)) %>%
  select(YearMo, MedMins, MedHrs) %>% 
  distinct()

# Dataset with median time by Year
d_year <- d %>%
  group_by(Year) %>%
  mutate(MedMins = median(TotalMins)) %>%
  mutate(MedHrs = median(TotalHrs)) %>%
  select(Year, MedMins, MedHrs) %>% 
  distinct()



############## Visualizations ################

### Just date and median time ###
## By TUDIARYDATE
plot(d_date$TUDIARYDATE[d_date$TUDIARYDATE < "2003-12-01"], d_date$MedHrs[d_date$TUDIARYDATE < "2003-12-01"], 
     main = "Med. Hours Spent Outside by Date", xlab = "Date (2003-2004)", ylab = "Median hours spent outside")

plot(d_date$TUDIARYDATE, d_date$MedHrs, 
     main = "Med. Hours Spent Outside by Date", xlab = "Date (2003-2023)", ylab = "Median hours spent outside")

plot(d_date$TUDIARYDATE[d_date$TUDIARYDATE < "2022-01-01" & d_date$TUDIARYDATE > "2020-01-01"], 
     d_date$MedHrs[d_date$TUDIARYDATE < "2022-01-01" & d_date$TUDIARYDATE > "2020-01-01"], 
     main = "Med. Hours Spent Outside by Date", xlab = "Date (2020-2022)", ylab = "Median hours spent outside")


## By YearMo
plot(d_yearmo$YearMo, d_yearmo$MedHrs, main = "Med. Hours Spent Outside by Date", 
     xlab = "Date (2003-2023)", ylab = "Median hours spent outside")


## By Year
plot(d_year$Year, d_year$MedHrs, main = "Med. Hours Spent Outside by Date", 
     xlab = "Date (2003-2023)", ylab = "Median hours spent outside")


### Add other variables ###



############## Models ###############


summary(lm(TotalHrs ~ Days + Days2 + PostCovid + Weekend + Season, data = d))












