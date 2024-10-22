# DS 340H
# Capstone Project
# Yildiz Derinkok
# Fall 2024

setwd("/Users/ozgecankocak/Downloads/Fall 24/DS 340H")
library(lubridate)
library(dplyr)

### Create Data frames with variables of interest
df_act <- read.delim("atusact_0323.dat", sep=",")
df_resp <- read.delim("atusresp_0323.dat", sep=",")

df_act <- df_act[c("TUCASEID", "TEWHERE", "TUCUMDUR", "TUSTARTTIM", "TUSTOPTIME")]
df_resp <- df_resp[c("TUCASEID", "TUDIARYDATE", "TELFS", "TRDTOCC1", "TROHHCHILD", "TESCHENR")]

df <- merge(df_act, df_resp, by="TUCASEID") #merge dataframes



# Clean date variables
df$TUSTARTTIM <- hms(df$TUSTARTTIM)
df$TUSTOPTIME <- hms(df$TUSTOPTIME)
df$TUDIARYDATE <- ymd(df$TUDIARYDATE)

# Make each person's day 24hrs


# Only keep activities where people are outside the home
#df <- df[df$TEWHERE != 1,] #1: at home or yard
#df <- df[df$TEWHERE != -1,] #-1: where info not collected

# Calculate total time spent outside for each person
d <- df %>%
  group_by(TUCASEID) %>%
  select(TUCASEID, TUCUMDUR, TUSTARTTIM, TUSTOPTIME) %>%
  mutate(TotalTime = sum(TUCUMDUR))

# Calculate median time spent outside for each date









