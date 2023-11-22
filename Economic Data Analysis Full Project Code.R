---
  title: "The Relationship Between Economic Vulnerability And Measurements of Standards of Living: A Case Study of Nigeria"
author: "Omari Mwakuluzo Kassim"
date: "2023-05-04"
---
 
## Setup of working directory and loading libraries
setwd("~/University Folder/2nd Year/Spring Term/Data Analysis/Project/New Files")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(outliers)
library(magrittr)


## Loading of All Data Sets ####
income <- read.csv("sect13_income.csv", sep = ",", header = TRUE)
housing <- read.csv("sect14_housing.csv", sep = ",", header = TRUE)
economicshock <- read.csv("sect16_shocks.csv", sep = ",", header = TRUE)
education <- read.csv("sect2_education.csv", sep = ",", header = TRUE)
health <- read.csv("sect3_health.csv", sep = ",", header = TRUE)
assets <- read.csv("sect10_assets.csv", sep = ",", header = TRUE)
enterprise<- read.csv("sect9a_enterprise.csv", sep= ",", header= TRUE)

## Data Munging (including selecting appropriate variables from the raw data sets, filtering, and removing N/A and Null values ####
income1<- subset(income, select = 
                   c(hhid, zone_id, state_id, sector, lga, ea_code, s13q02))

housing1<- subset(housing, select = 
                    c(hhid, zone, state, sector, lga, ea, s14q04a))

economicshock1<- subset(economicshock, select = 
                          c(hhid, zone, state, sector, lga, ea, s16q03, s16q01))

education1<- subset(education, select = 
                      c(hhid, zone, state, sector, lga, ea, s02q08,s02q15))

health1<- subset(health, select = 
                   c(hhid, zone, state, sector, lga, ea, s03q07a))

assets1<- subset(assets, select = 
                   c(hhid, zone, state, sector, lga, ea, s10q02))

enterprise1<- subset(enterprise, select= 
                       c(hhid, zone, state, sector, lga, ea, s09q01__1))

assets2<- assets1 %>%
  group_by(hhid)%>%
  summarize(total_assets = ifelse(all(is.na(s10q02)), NA, sum(s10q02,
                                                              na.rm = TRUE)))
income2 <- income1 %>%
  group_by(hhid) %>%
  summarize(total_income = ifelse(all(is.na(s13q02)), NA, sum(s13q02,
                                                              na.rm = TRUE)))
education2<- education1 %>%
  group_by(hhid) %>% 
  summarize(education_level = ifelse(all(is.na(s02q08)), NA, max(s02q08,
                                                                 na.rm = TRUE)))
health2<- health1 %>%
  group_by(hhid) %>% 
  summarize(total_injury = ifelse(all(is.na(s03q07a)), NA, sum(s03q07a,
                                                               na.rm = TRUE)))
economicshock1 <- economicshock1 %>%
  mutate(s16q01 = ifelse(s16q01 == 1, 0, s16q01))

economicshock1 <- economicshock1 %>%
  mutate(s16q01 = ifelse(s16q01 == 2, 1, s16q01))

economicshock2 <- economicshock1 %>%
  group_by(hhid) %>% 
  summarize(total_shock = ifelse(all(is.na(s16q03)), NA, sum(s16q03,
                                                             na.rm = TRUE)))
enterprise2<- enterprise1 %>% 
  group_by(hhid) %>% 
  summarize(business= ifelse(all(is.na(s09q01__1)), NA, sum(s09q01__1,
                                                            na.rm = TRUE)))
income3 <- income2 %>%
  na.omit(total_income)

## Merging data sets of selected variables to form a single dataset to be used ####
merged <- left_join(income3, housing1, by = c("hhid")) %>% 
  left_join(education2, by = c("hhid")) %>% 
  left_join(health2, by = c("hhid")) %>% 
  left_join(economicshock2, by = c("hhid")) %>% 
  left_join(assets2, by = c("hhid")) %>% 
  left_join(enterprise2, by= c("hhid"))

merged1<- merged %>% 
  filter(!is.na(education_level))
merged2<- merged1 %>% 
  filter(!is.na(total_shock))
merged3<- merged2 %>% 
  filter(!is.na(total_injury))
merged4<- merged3 %>% 
  filter(!is.na(s14q04a))
merged4<- merged4 %>% 
  filter(!is.na(total_injury))

names(merged4)[names(merged4)== "s14q04a"]<- "rent_val"

## Transforming Categorical variables (such as levels of Education) into factor variables ####
fact_edu<- merged4$education_level
Fact_Edu <- factor(fact_edu)
merged4 <- cbind(merged4, Fact_Edu)

#levels(merged4$Fact_Edu)
levels(merged4$Fact_Edu) <- c("None", "FSLC", "MSLC", "JSSC", "SSS", "AL", "NCE",
                              "BA", "MA", "PHD", "Other",  "VOCC", "VOCD")
#levels(merged4$Fact_Edu)
merged4$Dummy.FSLC<- ifelse(merged4$Fact_Edu== "FSLC", 1, 0)
merged4$Dummy.MSLC<- ifelse(merged4$Fact_Edu== "MSLC", 1, 0)
merged4$Dummy.JSSC<- ifelse(merged4$Fact_Edu== "JSSC", 1, 0)
merged4$Dummy.SSS<- ifelse(merged4$Fact_Edu== "SSS", 1, 0)
merged4$Dummy.AL<- ifelse(merged4$Fact_Edu== "AL", 1, 0)
merged4$Dummy.NCE<- ifelse(merged4$Fact_Edu== "NCE", 1, 0)
merged4$Dummy.BA<- ifelse(merged4$Fact_Edu== "BA", 1, 0)
merged4$Dummy.MA<- ifelse(merged4$Fact_Edu== "MA", 1, 0)
merged4$Dummy.PHD<- ifelse(merged4$Fact_Edu== "PHD", 1, 0)
merged4$Dummy.Other<- ifelse(merged4$Fact_Edu== "Other", 1, 0)
merged4$Dummy.VOCC<- ifelse(merged4$Fact_Edu== "VOCC", 1, 0)
merged4$Dummy.VOCD<- ifelse(merged4$Fact_Edu== "VOCD", 1, 0)

# Descriptive Statistics and Visualization of data distribution for the different variables ####
library(Hmisc)

### Income ####
describe(merged4$total_income)
hist(merged4$total_income, xlab = "Monthly Income", 
     ylab = "Number of Households",
     main = "Household Distribution of Income Levels",
     col= "brown", breaks = 80)

### Education ####
describe(merged4$Fact_Edu)
plot(merged4$Fact_Edu, xlab = "Education Level/Qualification", 
     ylab = "Number of Individuals",
     main = "Houshold Distribution of Education Levels",
     col= "brown")

### Health ####
describe(merged4$total_injury)
hist(merged4$total_injury, col = "brown",
     xlab = "Number of Days suffered in injuries",
     ylab = "Number of individuals",
     main = "Household Distribution of Days suffered in injury", breaks = 80)

### Number of Owned Assets ####
describe(merged4$total_assets)
hist(merged4$total_assets, col = "brown",
     xlab = "Number of Assets owned by households",
     ylab = "Number of households",
     main = "Household Distribution of Number of Assets Owned", breaks = 80)

### Housing ####
describe(merged4$rent_val)
hist(merged4$rent_val, col = "brown",
     xlab = "Predicted rental values of households",
     ylab = "Number of households",
     main = "Household distribution of predicted rental values", breaks = 80)


## Scaling of appropriate variables (such as Income) and removal of outliers from the data ####
# Scaling income
merged4$total_income <- merged4$total_income/1000

# Removing outliers for income
total_income_1 <- ifelse(merged4$total_income>= 1500, NA, merged4$total_income)

# Scaling of Predicted rental Values
merged4$rent_val <- merged4$rent_val/1000

# Removing outliers for predicted rental values
rent_val1 <- ifelse(merged4$rent_val>= 500, NA, merged4$rent_val)

# Removal of outliers for Days SUffered in Injury
total_injury_1 <- ifelse(merged4$total_injury>= 97, NA, merged4$total_injury)

# Removal of outliers for Number of Assets Owned
total_assets_1 <- ifelse(merged4$total_assets>= 100, NA, merged4$total_assets)


## Visualization of the data Distribution after the removal of outliers and Scaling of Income ####
### Income ####
hist(total_income_1,
     xlab = "Monthly Income (Thousand Naira)", 
     ylab = "Number of Households",
     main = "Household Distribution of Income Levels",
     col= "brown", breaks = 80)

### Predicted rental Values ####
hist(rent_val1,
     xlab = "Predicted rental value (Thousand Naira)", 
     ylab = "Number of Households",
     main = "Household Distribution of Predicted rental values",
     col= "brown", breaks = 80)

### Health ####
hist(total_injury_1,
     xlab = "NUmber of days suffered in injury", 
     ylab = "Number of Households",
     main = "Household Distribution of Days suffered in Injury",
     col= "brown", breaks =80)

### Number of Assets Owned ####
hist(total_assets_1,
     xlab = "Number of Assets owned by households",
     ylab = "Number of households",
     main = "Household Distribution of Number of Assets Owned",
     col= "brown", breaks =80)

## Merging and Creating a new table with the transformed variables and removing N/A values
merged5 <- cbind(merged4, rent_val1, total_assets_1, total_income_1, total_injury_1)
merged6 <- na.omit(merged5)

# Runnning Regressions and Regression Analysis #####

### Regressing and plotting the Number of economic shocks on income ####
shock_income <- lm(merged6$total_shock ~ merged6$total_income_1)
summary(shock_income)
plot(merged6$total_income_1, merged6$total_shock,
     main = "Effect of household income on number of economic shocks",
     xlab = "Monthly Income (Thousand Naira)",
     ylab = "Number of economic shocks", 
     col = "brown", pch=20) + abline(shock_income, lwd = 2, col = "blue")

### Regressing the Number of economic shocks on Health ####
shock_inj <- lm(merged6$total_shock ~ merged6$total_injury_1)
summary(shock_inj)
plot(merged6$total_injury_1, merged6$total_shock,
     xlab = "NUmber of days suffered in injury", 
     ylab = "Number of Economic Shocks",
     main = "Effect of number of health on number of economic shocks",
     pch=20, col = "brown") + abline(shock_inj, lwd = 2, col = "blue")

### Regressing the Number of economic shocks on Number of assets owned ####
shock_asset <- lm(merged6$total_shock ~ merged6$total_assets_1)
summary(shock_asset)
plot(merged6$total_assets_1, merged6$total_shock,
     xlab = "Number of Assets owned by households",
     ylab = "Number of Economic Shocks",
     main = "Effect of Number of Assets Owned on Number of Economic Shocks",
     col = "brown", pch=20) + abline(shock_asset, lwd = 2, col = "blue")

### Regressing the Number of economic shocks on housing conditions (predicted rental value) ####
shock_rent <- lm(merged6$total_shock ~ merged6$rent_val1)
summary(shock_rent)
plot(merged6$rent_val1, merged6$total_shock,
     xlab = "Predicted rental value (Thousand Naira)", 
     ylab = "Number of Economic Shocks",
     main = "Effect of predicted rental value on the number of economic shocks",
     col = "brown", pch=20) + abline(shock_rent, lwd = 2, col = "blue")

### Regressing the Number of economic shocks on Education Level ####
shock_Fact_Edu<- lm(merged6$total_shock~ merged6$Fact_Edu)
summary(shock_Fact_Edu)


# Running Multiple linear regressions ####  

## Multiple regressio model for main specification ####
multi_1 <- lm(merged6$total_shock~ merged6$total_assets_1 +
                merged6$total_income_1 + merged6$rent_val1 + 
                merged6$Fact_Edu + merged6$total_injury_1)
summary(multi_1)

### Checking for multicollinearity in regression 1 usinf the VIF} ####
vif(multi_1)

### Checking for homoscedasticity assumption in the generated model ####
plot(multi_1, which=1, pch = 20, lwd = 2)

## Multiple Regression Model for the Alternative specification ####  
### Manipulating additional variables of the model (factoring and merging to the data table) ####
fact_Bus <- merged6$business
fact_Bus <- factor(fact_Bus)
merged6 <- cbind (merged6, fact_Bus)
levels(merged6$fact_Bus)
levels(merged6$fact_Bus) <- c("1"="0", "2"="1")
merged6$fact_Bus<- relevel(factor(merged6$fact_Bus), ref= "0")

# Renaming the categories to Yes and No
levels(merged6$fact_Bus) <- c("0"="Yes", "1"="No")

### Distribution of the additional variable and regression model with Economic Shocks ####
plot(merged6$fact_Bus, xlab = "Owning a Business",
     ylab = "Number of Individuals", 
     main = "Effect of owning a business on Economic shocks", col = "brown")
shock_fact_Bus<- lm(merged6$total_shock~merged6$fact_Bus)
summary(shock_fact_Bus)

### Multiple Regression Model for the Alternative Specification} ####
multi_2 <- lm(merged6$total_shock~ merged6$total_assets_1 +
                merged6$total_income_1 + merged6$rent_val1 + 
                merged6$Fact_Edu + merged6$total_injury_1 +
                merged6$fact_Bus)
summary(multi_2)

### Linearizing the variables of the alternative model ####
linear_multi2 <- lm(merged6$total_shock ~ log(merged6$total_assets_1) +
                      log(merged6$total_income_1) + 
                      log(merged6$rent_val1) + 
                      merged6$Fact_Edu + 
                      log(merged6$total_injury_1) + 
                      merged6$fact_Bus)
summary(linear_multi2)

### Examining multicollinearity and homoscedasticity assumptions in the linearized alternative model ####
vif(linear_multi2)

plot(linear_multi2, which=1, pch = 20, lwd = 2)


