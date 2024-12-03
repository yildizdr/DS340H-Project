# DS 340H
# Capstone Project
# Yildiz Derinkok
# Fall 2024

setwd("/Users/ozgecankocak/Downloads/Fall 24/DS 340H/Project")
library(lubridate)
library(dplyr)
library(tidyr)
library(caret)

############################### Create data frame ###############################

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


############################### Data Cleaning ##################################

# Clean date variables
df$TUSTARTTIM <- hms(df$TUSTARTTIM)
df$TUSTOPTIME <- hms(df$TUSTOPTIME)
df$TUDIARYDATE <- ymd(df$TUDIARYDATE)

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
df$DayofWeek <- as.character(df$DayofWeek)
df$DayofWeek <- as.factor(df$DayofWeek)
df$Weekend <- if_else((df$DayofWeek == "Sat" | df$DayofWeek == "Sun"), 1, 0)


### Only keep activities where people are outside the home
#table(df$TEWHERE)
miss <- df[df$TEWHERE == -1 | df$TEWHERE == -2 | df$TEWHERE == -3,] #dataset of just missing values
df <- df[df$TEWHERE != 1,] #1: at home or yard
df <- df[df$TEWHERE != -1,] #-1: where info not collected
df <- df[df$TEWHERE != -2,] #-2: don't know
df <- df[df$TEWHERE != -3,] #-3: refused

# Calculate total time spent outside for each person
df <- df %>%
  group_by(TUCASEID) %>%
  mutate(TotalMins = sum(TUACTDUR24))

df$TotalHrs = df$TotalMins / 60



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



### Explore Missing TEWHERE values
#Create dataset of just missing WHERE values (-1, -2, -3)
miss <- miss %>%
  select (-c(TEWHERE, TUACTDUR24, TUSTARTTIM, TUSTOPTIME, TUCUMDUR24, TULINENO, 
             TELFS, TRDTOCC1, TROHHCHILD, TESCHENR, PEMARITL, GEREG, GESTFIPS, PTDTRACE, 
             HRYEAR4, HEFAMINC, HUFAMINC)) %>%
  distinct()

# Compare distribution of missing values to that of non-missing
summary(miss$TEAGE)
summary(d$TEAGE)

length(miss$TESEX[miss$TESEX == "Male"])/length(miss$TESEX[miss$TESEX == "Female"])
length(d$TESEX[d$TESEX == "Male"])/length(d$TESEX[d$TESEX == "Female"])

mean(miss$HRNUMHOU)
mean(d$HRNUMHOU)

mean(as.numeric(miss$HIncome))
mean(as.numeric(d$HIncome))

length(miss$School[miss$School == TRUE])/length(miss$School)
length(d$School[d$School == TRUE])/length(d$School)

length(miss$Married[miss$Married == TRUE])/length(miss$Married)
length(d$Married[d$Married == TRUE])/length(d$Married)

length(miss$EmployStat[miss$EmployStat == 2])/length(miss$EmployStat) #proportion unemployed
length(d$EmployStat[d$EmployStat == 2])/length(d$EmployStat)

length(miss$Weekend[miss$Weekend == 1])/length(miss$Weekend) #proportion weekend days
length(d$Weekend[d$Weekend == 1])/length(d$Weekend)




############################## Visualizations #################################

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




################################## Models #####################################

### Model with just time vars
summary(lm(TotalHrs ~ PostCovid, data = d)) 
#worried abt independence assumption, bc residuals not independent of each other.
#don't show this model, bc of independence issue. 

summary(lm(TotalHrs ~ Days + Days2 + PostCovid + DayofWeek + Season + Year + Month, data = d))
#when you add more time variables, independence assumption is changing. more vars related to time is better

lm.time <- lm(TotalHrs ~ Days + Days2 + PostCovid + DayofWeek + Weekend + Season + Year + Month + Days*PostCovid + Days2*PostCovid, data = d)
summary(lm.time) #including weekend doesn't change anything


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
          Weekend, data = d), direction="both", k= log(nrow(d))) 

lm.full2 <- lm(formula = TotalHrs ~ TEAGE + TESEX + HOver65 + HUnder13 + 
            JobCat + HUnder18 + School + EmployStat + Married + HIncome + 
            Month + Days + Days2 + PostCovid + Season + DayofWeek + 
            Region, data = d) # 17 vars - HRNUMHOU, Race, State, Weekend, Year
summary(lm.full2)


#Selection for interaction model
step(lm(TotalHrs ~ TEAGE*PostCovid + TESEX*PostCovid + HOver65*PostCovid + 
             HUnder13*PostCovid + HRNUMHOU*PostCovid + JobCat*PostCovid + 
             HUnder18*PostCovid + School*PostCovid + EmployStat*PostCovid + 
             Married*PostCovid + Race*PostCovid + HIncome*PostCovid + Region*PostCovid + 
             State*PostCovid + Year*PostCovid + Month*PostCovid + Days*PostCovid + 
             Days2*PostCovid + PostCovid + Season*PostCovid + DayofWeek*PostCovid +
             Weekend*PostCovid, data = d), direction="both", k= log(nrow(d))) 


lm.int2 <- lm(formula = TotalHrs ~ TEAGE + PostCovid + TESEX + HOver65 + 
                HUnder13 + HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                Married + HIncome + Month + Days + Days2 + Season + DayofWeek + 
                Region + TEAGE:PostCovid + PostCovid:HRNUMHOU + PostCovid:JobCat + 
                PostCovid:Days + PostCovid:Days2 + PostCovid:DayofWeek, data = d)
# Not included: Race, State, Year, Weekend, HOver65:PostCovid, HUnder13:PostCovid, 
# HUnder18*PostCovid, EmployStat*PostCovid, Married*PostCovid, Season*PostCovid, PostCovid:TESEX
# PostCovid:School, PostCovid:Month, PostCovid:Region
summary(lm.int2)


###Final model but without any covid variables
lm.nocovid <- lm(formula = TotalHrs ~ TEAGE + TESEX + HOver65 + HUnder13 + 
                   HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                   Married + HIncome + Month + Days + Days2 + Season + DayofWeek + 
                   Region, data = d)
summary(lm.nocovid)


### Model Evaluation
BIC(lm.time) #adj R2 = 0.05706
BIC(lm.full) #adj R2 = 0.2169
BIC(lm.int) #adj R2 = 0.2197 
BIC(lm.full2) #adj R2 = 0.2159
BIC(lm.int2) #best #adj R2 = 0.2188
BIC(lm.nocovid)

# K-fold Cross Validation 

control <- trainControl(method="cv", number=10)
model1 <- train(TotalHrs ~ Days + Days2 + PostCovid + DayofWeek + Weekend + Season + 
                Year + Month + Days*PostCovid + Days2*PostCovid, 
                data = d, method="lm", trControl=control)
model1 #lm.time

model2 <- train(TotalHrs ~ TEAGE*PostCovid + TESEX*PostCovid + HOver65*PostCovid + 
                  HUnder13*PostCovid + HRNUMHOU*PostCovid + JobCat*PostCovid + 
                  HUnder18*PostCovid + School*PostCovid + EmployStat*PostCovid + 
                  Married*PostCovid + Race*PostCovid + HIncome*PostCovid + Region*PostCovid + 
                  State*PostCovid + Year*PostCovid + Month*PostCovid + Days*PostCovid + 
                  Days2*PostCovid + PostCovid + Season*PostCovid + DayofWeek*PostCovid +
                  Weekend*PostCovid, data = d, method="lm", trControl=control)
model2 #lm.int

model3 <- train(TotalHrs ~ TEAGE + PostCovid + TESEX + HOver65 + 
                  HUnder13 + HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                  Married + HIncome + Month + Days + Days2 + Season + DayofWeek + 
                  Region + TEAGE:PostCovid + PostCovid:HRNUMHOU + PostCovid:JobCat + 
                  PostCovid:Days + PostCovid:Days2 + PostCovid:DayofWeek, 
                  data = d, method="lm", trControl=control)
model3 #lm.int2

model4 <- train(TotalHrs ~ TEAGE + TESEX + HOver65 + HUnder13 + 
                HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                Married + HIncome + Month + Days + Days2 + Season + DayofWeek + 
                Region, data = d, method="lm", trControl=control)
model4 #lm.nocovid




########################## Hypothesis Testing ###############################
### Test Significance of Categorical Variables
#Season
lm.noseason <- lm(formula = TotalHrs ~ TEAGE + PostCovid + TESEX + HOver65 + 
                HUnder13 + HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                Married + HIncome + Month + Days + Days2 + DayofWeek + 
                Region + TEAGE:PostCovid + PostCovid:HRNUMHOU + PostCovid:JobCat + 
                PostCovid:Days + PostCovid:Days2 + PostCovid:DayofWeek, data = d)
anova(lm.int2, lm.noseason) #significant
lm.nocovseason <- lm(formula = TotalHrs ~ TEAGE + TESEX + HOver65 + HUnder13 + 
      HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                   Married + HIncome + Month + Days + Days2 + DayofWeek + 
                   Region, data = d)
anova(lm.nocovid, lm.nocovseason) #sig

#Day of Week
lm.noweekday <- lm(formula = TotalHrs ~ TEAGE + PostCovid + TESEX + HOver65 + 
                HUnder13 + HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                Married + HIncome + Month + Days + Days2 + Season + 
                Region + TEAGE:PostCovid + PostCovid:HRNUMHOU + PostCovid:JobCat + 
                PostCovid:Days + PostCovid:Days2, data = d)
anova(lm.int2, lm.noweekday) #sig
lm.nocovweekday <- lm(formula = TotalHrs ~ TEAGE + TESEX + HOver65 + HUnder13 + 
                   HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                   Married + HIncome + Month + Days + Days2 + Season + 
                   Region, data = d)
anova(lm.nocovid, lm.nocovweekday) #sig

#Region
lm.noregion <- lm(formula = TotalHrs ~ TEAGE + PostCovid + TESEX + HOver65 + 
                HUnder13 + HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                Married + HIncome + Month + Days + Days2 + Season + DayofWeek + 
                TEAGE:PostCovid + PostCovid:HRNUMHOU + PostCovid:JobCat + 
                PostCovid:Days + PostCovid:Days2 + PostCovid:DayofWeek, data = d)
anova(lm.int2, lm.noregion) #sig
lm.nocovregion <- lm(formula = TotalHrs ~ TEAGE + TESEX + HOver65 + HUnder13 + 
                   HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                   Married + HIncome + Month + Days + Days2 + Season + DayofWeek, data = d)
anova(lm.nocovid, lm.nocovregion) #sig

#Employment Status
lm.noemployment <- lm(formula = TotalHrs ~ TEAGE + PostCovid + TESEX + HOver65 + 
                HUnder13 + HRNUMHOU + JobCat + HUnder18 + School + 
                Married + HIncome + Month + Days + Days2 + Season + DayofWeek + 
                Region + TEAGE:PostCovid + PostCovid:HRNUMHOU + PostCovid:JobCat + 
                PostCovid:Days + PostCovid:Days2 + PostCovid:DayofWeek, data = d)
anova(lm.int2, lm.noemployment) #sig
lm.nocovemployment <- lm(formula = TotalHrs ~ TEAGE + TESEX + HOver65 + HUnder13 + 
                   HRNUMHOU + JobCat + HUnder18 + School + 
                   Married + HIncome + Month + Days + Days2 + Season + DayofWeek + 
                   Region, data = d) #sig
anova(lm.nocovid, lm.nocovemployment)

#Job Category
lm.nojobcat <- lm(formula = TotalHrs ~ TEAGE + PostCovid + TESEX + HOver65 + 
                HUnder13 + HRNUMHOU + HUnder18 + School + EmployStat + 
                Married + HIncome + Month + Days + Days2 + Season + DayofWeek + 
                Region + TEAGE:PostCovid + PostCovid:HRNUMHOU  + 
                PostCovid:Days + PostCovid:Days2 + PostCovid:DayofWeek, data = d)
anova(lm.int2, lm.nojobcat) #sig
lm.nocovjobcat <- lm(formula = TotalHrs ~ TEAGE + TESEX + HOver65 + HUnder13 + 
                   HRNUMHOU + HUnder18 + School + EmployStat + 
                   Married + HIncome + Month + Days + Days2 + Season + DayofWeek + 
                   Region, data = d)
anova(lm.nocovid, lm.nocovjobcat) #sig

#Household Income
lm.nohinc <- lm(formula = TotalHrs ~ TEAGE + PostCovid + TESEX + HOver65 + 
                  HUnder13 + HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                  Married + Month + Days + Days2 + Season + DayofWeek + 
                  Region + TEAGE:PostCovid + PostCovid:HRNUMHOU + PostCovid:JobCat + 
                  PostCovid:Days + PostCovid:Days2 + PostCovid:DayofWeek, data = d)
anova(lm.int2, lm.nohinc) #sig
lm.nocovhinc <- lm(formula = TotalHrs ~ TEAGE + TESEX + HOver65 + HUnder13 + 
                     HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                     Married + Month + Days + Days2 + Season + DayofWeek + 
                     Region, data = d)
anova(lm.nocovid, lm.nocovhinc) #sig

#Residual plot
scatter.smooth(residuals(lm.int2)~predict(lm.int2), xlab= "Y.hat", ylab= "Residuals")



### Final Model vs Final model without covid 
anova(lm.int2, lm.nocovid) 
#Ho: reduced model is correct. Ha: need the complex model. 
#Pvalue = 0 means we can reject the simpler model


### Hypothesis Test for 2019 vs 2023
#could ask model what it would predict if there wasn't covid. what the previous trend would predict.
#refit model up until march 2020, make predictions for now. 
#can omit mar 2020-nov 23: see if covid predictor is significant.

d_precovid <- d[d$TUDIARYDATE < "2020-03-01" | d$TUDIARYDATE > "2023-11-30",]

lm.precovid2 <- lm(formula = TotalHrs ~ TEAGE + PostCovid + TESEX + HOver65 + 
                HUnder13 + HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                Married + HIncome + Month + Days + Days2 + Season + DayofWeek + 
                Region + TEAGE:PostCovid + PostCovid:HRNUMHOU + PostCovid:JobCat + 
                PostCovid:Days + PostCovid:Days2 + PostCovid:DayofWeek, data = d_precovid)
lm.precovid <- lm(formula = TotalHrs ~ TEAGE + TESEX + HOver65 + 
                    HUnder13 + HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                    Married + HIncome + Month + Days + Days2 + Season + DayofWeek + 
                    Region, data = d_precovid)
summary(lm.precovid)

plot(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata), type = "l", ylim = c(0, 19))
#points(newdata$TUDIARYDATE, predict(lm.nocovid, newdata = newdata), type = "l", ylim = c(0, 19))
points(newdata$TUDIARYDATE, predict(lm.precovid, newdata = newdata), type = "l", ylim = c(0, 19), lty = 2)


plot(newdata$TUDIARYDATE, predict(lm.precovid2, newdata = newdata), type = "l", ylim = c(0, 19), lty = 2)
#plot(newdata$TUDIARYDATE[newdata$TUDIARYDATE > "2023-11-30"], predict(lm.precovid, newdata = newdata[newdata$TUDIARYDATE > "2023-11-30",]), type = "l", ylim = c(0, 19), lty = 2)




############## Draw Model Predictions ###############

### Create new datasets for predictions ###

#Person 1: married 46 yo woman in a 3 person household, no kids, no over 65, income = 14, region= west
#employed, works in management operations
newdata <- d
newdata <- subset(newdata, select = -c(TUCASEID, TotalHrs, Year, Weekend, State, Race)) #drop rows that are not included in final model
newdata$TEAGE <- mean(d$TEAGE) #46.8
newdata$TESEX <- "Female"
newdata$HOver65 <- FALSE
newdata$HUnder13 <- FALSE
newdata$HRNUMHOU <- 3
newdata$JobCat <- as.factor(1) #Management Occupations
newdata$HUnder18 <- FALSE
newdata$School <- FALSE
newdata$EmployStat <- as.factor(1) #employed
newdata$Married <- TRUE
newdata$HIncome <- as.factor(14) #$75,000 to $99,999
newdata$Region <- as.factor(4) #west

newdata <- newdata %>%
  distinct() #just keep one row for one day
newdata <- newdata[newdata$DayofWeek == 'Wed',] #only include Wednesdays

#Person 1, but (fem) unemployed
newdata.funemploy <- newdata
newdata.funemploy$EmployStat <- as.factor(2)
newdata.funemploy$JobCat <- as.factor(-1)

#Person 1, but male (employed)
newdata.male <- newdata
newdata.male$TESEX <- "Male"

#Person 1, but male unemployed
newdata.munemploy <- newdata.funemploy
newdata.munemploy$TESEX <- "Male"


#Person 1 dif ages
newdata.15 <- newdata
newdata.15$TEAGE <- 15
newdata.25 <- newdata
newdata.25$TEAGE <- 25
newdata.35 <- newdata
newdata.35$TEAGE <- 35
newdata.45 <- newdata
newdata.45$TEAGE <- 45
newdata.55 <- newdata
newdata.55$TEAGE <- 55
newdata.65 <- newdata
newdata.65$TEAGE <- 65
newdata.75 <- newdata
newdata.75$TEAGE <- 75
newdata.85 <- newdata
newdata.85$TEAGE <- 85


#Person 1 dif regions
newdata.nwest <- newdata
newdata.nwest$Region <- as.factor(1) #northwest
newdata.mwest <- newdata
newdata.mwest$Region <- as.factor(2) #midwest
newdata.south <- newdata
newdata.south$Region <- as.factor(3) #south


#Person 1 dif job categories
newdata.j17 <- newdata
newdata.j17$JobCat <- as.factor(17) #Office and administrative support occupations
newdata.j16 <- newdata
newdata.j16$JobCat <- as.factor(16) #Sales and related occupations
newdata.j8 <- newdata
newdata.j8$JobCat <- as.factor(8) #Educational instruction and library occupations
newdata.j10 <- newdata
newdata.j10$JobCat <- as.factor(10) #Healthcare practitioner and technical occupations
newdata.j2 <- newdata
newdata.j2$JobCat <- as.factor(2) #Business and financial operations occupations
newdata.j19 <- newdata
newdata.j19$JobCat <- as.factor(19) #Construction and extraction occupations


### Plot final model on new datasets ###

#Plot on all data points (test if there's a skip)
plot(d_date$TUDIARYDATE[d_date$TUDIARYDATE < "2020-06-01" & d_date$TUDIARYDATE > "2020-03-01"], d_date$MedHrs[d_date$TUDIARYDATE < "2020-06-01" & d_date$TUDIARYDATE > "2020-03-01"], xlab = "Days since January 1, 2023", ylab = "Hours spent outside")
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata), col = "lightblue", type = "l")
#points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.unemployed), col = "red", type = "l")


### Prediction Interval for Person 1
pred.interval <- predict(lm.int2, newdata = newdata, interval = "prediction", se = TRUE, level = 0.9)

plot(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata), type = "l", ylim = c(0, 19), 
     xlab = "Year", ylab = "Hours spent outside", main = "Hours spent outside for an average person, with prediction interval")
polygon(x = c(newdata$TUDIARYDATE, rev(newdata$TUDIARYDATE)),
        y = c(pred.interval$fit[,2], rev(pred.interval$fit[,3])),
        col =  adjustcolor("dodgerblue", alpha.f = 0.20), border = NA)


### Plot for Unemployment and Sex
#Employed
par(las=1)
plot(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata), col = "paleturquoise3", type = "l", 
     ylim = c(0, 14), lwd = 1, xlab = "Year", ylab = "Hours spent outside", 
     main = "Hours spent outside for Employed vs Unemployed & Female vs Male")
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.male), col = "paleturquoise3", type = "l", lty = 2, lwd = 1)
#Unemployed
points(newdata.funemploy$TUDIARYDATE, predict(lm.int2, newdata = newdata.funemploy), col = "salmon", type = "l", ylim = c(0, 14), lwd = 1)
points(newdata.munemploy$TUDIARYDATE, predict(lm.int2, newdata = newdata.munemploy), col = "salmon", type = "l", lty = 2, lwd = 1)
legend(x="topright", legend=c("Employed","Unemployed", "Female", "Male"), 
       col=c("paleturquoise3","salmon", "black", "black"), 
       lwd=1, lty=c(1,1,1,2), cex = 0.5)


### Plot for Unemployment and Sex TRIAL !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
lm.trial <- lm(formula = TotalHrs ~ TEAGE + PostCovid + TESEX + HOver65 + 
                HUnder13 + HRNUMHOU + JobCat + HUnder18 + School + EmployStat + 
                Married + HIncome + Month + Days + Days2 + Season + DayofWeek + 
                Region + TEAGE:PostCovid + PostCovid:HRNUMHOU + PostCovid:JobCat + 
                PostCovid:Days + PostCovid:Days2 + PostCovid:DayofWeek + 
              PostCovid:EmployStat, data = d)
#Employed
par(las=1)
plot(newdata$TUDIARYDATE, predict(lm.trial, newdata = newdata), col = "paleturquoise3", type = "l", 
     ylim = c(0, 14), lwd = 1, xlab = "Year", ylab = "Hours spent outside", 
     main = "Hours spent outside for Employed vs Unemplyed 
     and Female vs Male, Since 2003")
points(newdata$TUDIARYDATE, predict(lm.trial, newdata = newdata.male), col = "paleturquoise3", type = "l", lty = 2, lwd = 1)
#Unemployed
points(newdata.funemploy$TUDIARYDATE, predict(lm.trial, newdata = newdata.funemploy), col = "salmon", type = "l", ylim = c(0, 14), lwd = 1)
points(newdata.munemploy$TUDIARYDATE, predict(lm.trial, newdata = newdata.munemploy), col = "salmon", type = "l", lty = 2, lwd = 1)
legend(x="topright", legend=c("Employed","Unemployed", "Female", "Male"), 
       col=c("paleturquoise3","salmon", "black", "black"), 
       lwd=1, lty=c(1,1,1,2), cex = 0.5)



### Plot for Age
#every 10 years
par(las=1)
plot(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.15), col = "#e641b6", type = "l", 
     ylim = c(0, 14), lwd = 1, xlab = "Year", ylab = "Hours spent outside", 
     main = "Hours spent outside for Different Ages")
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.25), col = "#e50000", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.35), col = "#ec894d", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.45), col = "#f0cc2e", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.55), col = "#3ea908", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.65), col = "#41b6e6", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.75), col = "#0057e5", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.85), col = "#003b5c", type = "l", lwd = 1)
legend(x="topright", legend=c("15 y.o","25 y.o", "35 y.o", "45 y.o", "55 y.o", "65 y.o", "75 y.o", "85 y.o"), 
       col=c("#e641b6","#e50000", "#ec894d", "#f0cc2e", "#3ea908", "#41b6e6", "#0057e5", "#003b5c"), 
       lwd=1, lty=c(1,1,1,1,1,1,1,1), cex = 0.5)



### Plot for Region
par(las=1)
plot(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.nwest), col = "#540d6e", type = "l", 
     ylim = c(0, 14), lwd = 1, xlab = "Year", ylab = "Hours spent outside", 
     main = "Hours spent outside for Different Regions")
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.mwest), col = "#d8315b", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.south), col = "#ffd23f", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata), col = "#3e92cc", type = "l", lwd = 1)
legend(x="topright", legend=c("Northwest","Midwest", "South", "West"), 
       col=c("#540d6e","#d8315b", "#ffd23f", "#3e92cc"), 
       lwd=1, lty=c(1,1,1,1), cex = 0.5)



### Plot for Job Category
#most common 7 + Unemployed
par(las=1)
plot(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata), col = "#e641b6", type = "l", 
     ylim = c(0, 14), lwd = 1, xlab = "Year", ylab = "Hours spent outside", 
     main = "Hours spent outside for Different Job Categories")
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.j17), col = "#e50000", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.j16), col = "#ec894d", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.j8), col = "#f0cc2e", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.j10), col = "#3ea908", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.j2), col = "#41b6e6", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.j19), col = "#0057e5", type = "l", lwd = 1)
points(newdata$TUDIARYDATE, predict(lm.int2, newdata = newdata.funemploy), col = "black", type = "l", lwd = 1)
legend(x="topleft", legend=c("Management", "Office","Sales", "Education", "Healthcare", "Business", "Construction", "Unemployed"), 
       col=c("#e641b6","#e50000", "#ec894d", "#f0cc2e", "#3ea908", "#41b6e6", "#0057e5", "black"), 
       lwd=1, lty=c(1,1,1,1,1,1,1, 1), cex = 0.5)





