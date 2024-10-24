# DS 340H
# Capstone Project
# Yildiz Derinkok
# Fall 2024

setwd("/Users/ozgecankocak/Downloads/Fall 24/DS 340H/Project")
library(lubridate)
library(dplyr)

####### Data Cleaning ########

# Create Data frames with variables of interest
df_act <- read.delim("atusact_0323.dat", sep=",")
df_resp <- read.delim("atusresp_0323.dat", sep=",")

df_act <- df_act[c("TUCASEID", "TEWHERE", "TUACTDUR24", "TUSTARTTIM", "TUSTOPTIME", "TUCUMDUR24")]
df_resp <- df_resp[c("TUCASEID", "TUDIARYDATE", "TELFS", "TRDTOCC1", "TROHHCHILD", "TESCHENR")]

df <- merge(df_act, df_resp, by="TUCASEID") #merge dataframes


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
  select(TUCASEID, TEWHERE, TUDIARYDATE, TUACTDUR24, TUSTARTTIM, TUSTOPTIME, TUCUMDUR24, TELFS, TRDTOCC1, TROHHCHILD, TESCHENR) %>%
  mutate(TotalMins = sum(TUACTDUR24))

df$TotalHrs = df$TotalMins / 60


# Add variables for week and month and by year.
df$Year <- format(df$TUDIARYDATE, "%Y")
df$YearMo <- ym(format(df$TUDIARYDATE, "%Y-%m"))


# Get dataset for each person
d <- df %>%
  select(TUCASEID, TUDIARYDATE, Year, YearMo, TotalMins, TotalHrs) %>% 
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



####### Visualizations #########

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



