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

# Get variables of interest from roster file
df_ros$O65 <- df_ros$TEAGE > 64
df_ros$U13 <- df_ros$TEAGE < 14
df_household <- df_ros %>%
  group_by(TUCASEID) %>%
  summarize(O651 = mean(O65), U131 = mean(U13))

df_household$HOver65 <- df_household$O651 > 0
df_household$HUnder13 <- df_household$U131 > 0

df_ros <- merge(df_ros, df_household, by="TUCASEID") #add the new vars to df_ros


# Create Data frames with variables of interest
df_act <- df_act[c("TUCASEID", "TEWHERE", "TUACTDUR24", "TUSTARTTIM", "TUSTOPTIME", "TUCUMDUR24")]
df_resp <- df_resp[c("TUCASEID", "TUDIARYDATE", "TELFS", "TRDTOCC1", "TROHHCHILD", "TESCHENR")]
df_cps <- df_cps[df_cps$TULINENO == 1,] #only get interview respondents
df_cps <- df_cps[c("TUCASEID", "TULINENO", "PEMARITL", "GESTFIPS", "GEREG", "HRNUMHOU", "HEFAMINC", "HUFAMINC", "PTDTRACE", "HRYEAR4")]
#length(unique(df_cps$TUCASEID)) #cps data has more households than act, resp, and ros (they have 245139)
df_ros <- df_ros[df_ros$TULINENO == 1,] #only get interview respondents
df_ros <- df_ros[c("TUCASEID", "TEAGE", "TESEX", "HOver65", "HUnder13")]

# Merge data frames
df_merge1 <- merge(df_act, df_resp, by="TUCASEID") 
df_merge2 <- merge(df_merge1, df_ros, by="TUCASEID")
df <- merge(df_merge2, df_cps, by="TUCASEID")


############## Data Cleaning ###############

# Clean date variables
df$TUSTARTTIM <- hms(df$TUSTARTTIM)
df$TUSTOPTIME <- hms(df$TUSTOPTIME)
df$TUDIARYDATE <- ymd(df$TUDIARYDATE)

# Only keep activities where people are outside the home    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#table(df$TEWHERE)
df <- df[df$TEWHERE != 1,] #1: at home or yard
df <- df[df$TEWHERE != -1,] #-1: where info not collected
df <- df[df$TEWHERE != -2,] #-2: don't know
df <- df[df$TEWHERE != -3,] #-3: refused

# Calculate total time spent outside for each person
df <- df %>%
  group_by(TUCASEID) %>%
  mutate(TotalMins = sum(TUACTDUR24))

df$TotalHrs = df$TotalMins / 60


# Clean categorical vars
df$JobCat <- as.factor(df$TRDTOCC1)
df$HUnder18 <- df$TROHHCHILD == 1
df$School <- df$TESCHENR == 1
df$TESEX <- ifelse(df$TESEX == 1, "Male", "Female")
df$EmployStat <- as.factor(df$TELFS)
df$Married <- df$PEMARITL == 1
df$Race <- as.factor(df$PTDTRACE)
df$HIncome <- ifelse(df$HRYEAR4 < 2010, df$HUFAMINC, df$HEFAMINC)
df$HIncome <- as.factor(df$HIncome)

# Clean location vars
df$Region <- as.factor(df$GEREG)
df$State <- as.factor(df$GESTFIPS)



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
df$DayofWeek <- factor(df$DayofWeek, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
df$Weekend <- if_else((df$DayofWeek == "Sat" | df$DayofWeek == "Sun"), 1, 0)




### Create Subsets ###

# Get dataset for each person
d <- df %>%
  select (-c(TEWHERE, TUACTDUR24, TUSTARTTIM, TUSTOPTIME, TUCUMDUR24, TULINENO, TotalMins, 
             TELFS, TRDTOCC1, TROHHCHILD, TESCHENR, PEMARITL, GEREG, GESTFIPS, PTDTRACE, 
             HRYEAR4, HEFAMINC, HUFAMINC)) %>%
  distinct()
#length(unique(df$TUCASEID)) # 208715 #check if there is a row in d for each unique case id


# Dataset with median time by date
d_date <- d %>%
  group_by(TUDIARYDATE) %>%
  #mutate(MedMins = median(TotalMins)) %>%
  mutate(MedHrs = median(TotalHrs)) %>%
  select(TUDIARYDATE, MedHrs, Season, DayofWeek, Weekend, PostCovid, Days) %>% 
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
plot(d_date$TUDIARYDATE, d_date$MedHrs, main = "Med. Hours Spent Outside by Date", 
     xlab = "Date (2003-2023)", ylab = "Median hours spent outside")

#2003 - 2004
plot(d_date$TUDIARYDATE[d_date$TUDIARYDATE < "2003-12-01"], d_date$MedHrs[d_date$TUDIARYDATE < "2003-12-01"], 
     main = "Med. Hours Spent Outside by Date", xlab = "Date (2003-2004)", ylab = "Median hours spent outside")

#2020 - 2022
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
plot(d_date$TUDIARYDATE, d_date$MedHrs, col = as.factor(d_date$Season),
     main = "Med. Hours Spent Outside by Date", xlab = "Date (2003-2023)", ylab = "Median hours spent outside")

#cols = c("pink", "black")
plot(d_date$TUDIARYDATE, d_date$MedHrs, col = as.factor(d_date$Weekend), #col = cols[as.factor(d_date$Weekend)]
     main = "Med. Hours Spent Outside by Date", xlab = "Date (2003-2023)", ylab = "Median hours spent outside")

lm(formula = MedHrs ~ Weekend * PostCovid, data = d_date)



############## Models ###############

### Model with just time vars
summary(lm(TotalHrs ~ PostCovid, data = d)) 
#worried abt independence assumption, bc residuals not independent of each other.
#don't show this model, bc of independence issue. 

summary(lm(TotalHrs ~ Days + Days2 + PostCovid + DayofWeek + Season + Year + Month, data = d))
#when you add more time variables, independence assumption is changing. more vars related to time is better

lm.time <- lm(TotalHrs ~ Days + Days2 + PostCovid + DayofWeek + Season + Year + Month + Days*PostCovid + Days2*PostCovid, data = d)
summary(lm.time)


### Model with all variables
lm.full <- lm(TotalHrs ~ TEAGE + TESEX + HOver65 + HUnder13 + HRNUMHOU + JobCat + 
             HUnder18 + School + EmployStat + Married + Race + HIncome + Region + 
             State + Year + Month + Days + Days2 + PostCovid + Season + DayofWeek +
             Weekend, data = d) # 22 predictors
summary(lm.full)

#Add interactions for Covid for all vars
lm.int <- lm(TotalHrs ~ TEAGE*PostCovid + TESEX*PostCovid + HOver65*PostCovid + 
             HUnder13*PostCovid + HRNUMHOU*PostCovid + JobCat*PostCovid + 
             HUnder18*PostCovid + School*PostCovid + EmployStat*PostCovid + 
             Married*PostCovid + Race*PostCovid + HIncome*PostCovid + Region*PostCovid + 
             State*PostCovid + Year*PostCovid + Month*PostCovid + Days*PostCovid + 
             Days2*PostCovid + PostCovid + Season*PostCovid + DayofWeek*PostCovid +
             Weekend*PostCovid, data = d) 
summary(lm.int)


### Variable selection by BIC
#Selection for no interaction model
step(lm(TotalHrs ~ TEAGE + TESEX + HOver65 + HUnder13 + HRNUMHOU + JobCat + 
          HUnder18 + School + EmployStat + Married + Race + HIncome + Region + 
          State + Year + Month + Days + Days2 + PostCovid + Season + DayofWeek +
          Weekend, data = d), direction="both", k= log(n)) 

lm.full2 <- lm(formula = TotalHrs ~ TEAGE + TESEX + HOver65 + HUnder13 + 
            JobCat + HUnder18 + School + EmployStat + Married + HIncome + 
            Year + Month + Days + Days2 + PostCovid + Season + DayofWeek + 
            Region, data = d) # 18 vars - HRNUMHOU, Race, State, Weekend
summary(lm.full2)
#Days still here? State and race removed?  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#Selection for interaction model
step(lm(TotalHrs ~ TEAGE*PostCovid + TESEX*PostCovid + HOver65*PostCovid + 
             HUnder13*PostCovid + HRNUMHOU*PostCovid + JobCat*PostCovid + 
             HUnder18*PostCovid + School*PostCovid + EmployStat*PostCovid + 
             Married*PostCovid + Race*PostCovid + HIncome*PostCovid + Region*PostCovid + 
             State*PostCovid + Year*PostCovid + Month*PostCovid + Days*PostCovid + 
             Days2*PostCovid + PostCovid*PostCovid + Season*PostCovid + DayofWeek*PostCovid +
             Weekend*PostCovid, data = d), direction="both", k= log(n)) 


lm.int2 <- lm(formula = TotalHrs ~ TEAGE + PostCovid + TESEX + HOver65 + 
              HUnder13 + HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
              Married + HIncome + Month + Days + Days2 + Season + DayofWeek + 
              Region + TEAGE:PostCovid + PostCovid:TESEX + PostCovid:HRNUMHOU + 
              PostCovid:JobCat + PostCovid:School + PostCovid:Month + PostCovid:Days + 
              PostCovid:Days2 + PostCovid:DayofWeek + PostCovid:Region, 
              data = d) # - Race, State, Year, Weekend, HOver65:PostCovid, HUnder13:PostCovid, 
              # HUnder18*PostCovid, EmployStat*PostCovid, Married*PostCovid, Season*PostCovid
summary(lm.int2)


### Model Evaluation
BIC(lm.time) #adj R2 = 0.05706
BIC(lm.full) #adj R2 = 0.2169
BIC(lm.int) #adj R2 = 0.2197 
BIC(lm.full2) #adj R2 = 0.2166
BIC(lm.int2) #best #adj R2 = 0.219

# K-fold Cross Validation  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!!!!!!!!!!!!




### Final Model vs Final model without covid   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




#### Draw the Models

##Create new datasets for predictions
# 46 yo white woman in a 3 person household with no kids or elders in the home. 
# married, employed, works in management operations
newdata <- d
newdata$TEAGE <- mean(d$TEAGE)
newdata$TESEX <- "Female"
newdata$HOver65 <- FALSE
newdata$HUnder13 <- FALSE
newdata$HRNUMHOU <- 3
newdata$JobCat <- as.factor(1) #Management Occupations
newdata$HUnder18 <- FALSE
newdata$School <- FALSE
newdata$EmployStat <- as.factor(1) #employed
newdata$Married <- TRUE
newdata$Race <- as.factor(1) #white
newdata$HIncome <- as.factor(14)
newdata$State <- as.factor(6)
newdata$Region <- as.factor(4)
newdata <- subset(newdata, select = -c(TUCASEID, TotalHrs)) #drop caseid, totalhrs
newdata <- newdata %>%
  distinct() #just keep one row for one day

#Same, but unemployed
newdata2 <- newdata
newdata2$EmployStat <- as.factor(2)
newdata2$JobCat <- as.factor(-1)

#Same as 1, but there is an elder in the home
newdata3 <- newdata
newdata3$HOver65 <- TRUE

#Same as 1, but person is 65



## Plot Time model
# do i make a spline? the lines are really ugly  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
plot(newdata$Days, predict(lm.time, newdata = newdata))

plot(d_date$Days, d_date$MedHrs)
points(newdata$Days, predict(lm.time, newdata = newdata), type = "l", col = "lightblue")


## Plot final model
plot(newdata$Days, predict(lm.int2, newdata = newdata))
plot(newdata$Days, predict(lm.int2, newdata = newdata), type = "l")

plot(d_date$Days, d_date$MedHrs, xlab = "Days since January 1, 2023", ylab = "Hours spent outside")
points(newdata$Days, predict(lm.int2, newdata = newdata), col = "lightblue")
points(newdata$Days, predict(lm.int2, newdata = newdata2), col = "red")




#figure out why weekday is weird. what is ordered factor? try reg with just that var
#race? 










