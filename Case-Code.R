# Problem Set 1 - Sirisha Gadepalli, Ayush Ghildyal, Garima Singhal

# Read the claims data from github  
claims <- read.csv("https://raw.githubusercontent.com/reifjulian/illinois-wellness-data/master/data/csv/claims.csv")
# The first six rows of the dataset 
head(claims)
# Names of the columns in the dataset 
names(claims)
# Structure of the dataset, variable description
str(claims)

# Question 4 - Pre Randomization (i.e. prior to August 2016)
"We run linear regression on 4 outcome variables for the pre-randomization period (i.e. prior to August 2016)"

# Linear Regression on Average Monthly Spending -> pre-randomization
lm_spend_0715_0716 <- lm(claims$spend_0715_0716 ~ as.factor(treat), data = claims)
summary(lm_spend_0715_0716)

# Linear Regression on Average Monthly Spending Office Care -> pre-randomization
lm_spendOff_0715_0716 <- lm(claims$spendOff_0715_0716 ~ as.factor(treat), data = claims)
summary(lm_spendOff_0715_0716)

# Linear Regression on Average Monthly Spending Hospital Care -> pre-randomization
lm_spendHosp_0715_0716 <- lm(claims$spendHosp_0715_0716 ~ as.factor(treat), data = claims)
summary(lm_spendHosp_0715_0716)

# Linear Regression on Average Monthly Spending RX -> pre-randomization
lm_spendRx_0715_0716 <- lm(claims$spendRx_0715_0716 ~ as.factor(treat), data = claims)
summary(lm_spendRx_0715_0716)

# Question 4 - Group means for each spending 
library(dplyr)

# Group means for Average Monthly Spending -> pre-randomization
# We remove the NA values in the data with na.rm = TRUE command
claims %>%
  group_by(treat) %>%
  summarise(mean = mean(spend_0715_0716, na.rm = TRUE))

# Group means for Average Monthly Spending Office Care-> pre-randomization
# We remove the NA values in the data with na.rm = TRUE command
claims %>%
  group_by(treat) %>%
  summarise(mean = mean(spendOff_0715_0716, na.rm = TRUE))

# Group means for Average Monthly Spending Hospital Care-> pre-randomization
# We remove the NA values in the data with na.rm = TRUE command
claims %>%
  group_by(treat) %>%
  summarise(mean = mean(spendHosp_0715_0716, na.rm = TRUE))

# Group means for Average Monthly Spending RX -> pre-randomization
# We remove the NA values in the data with na.rm = TRUE command
claims %>%
  group_by(treat) %>%
  summarise(mean = mean(spendRx_0715_0716, na.rm = TRUE))

# Question 5 - A year following randomization (i.e. 2016 - 2017) for treatment and control groups 
"We run linear regression on 4 outcome variables for the year following randomization(i.e. 2016 - 2017)
with the demographics and without the demographics between treatment and control groups.The demographics 
we consider are sex(male/female), race(white/nonwhite), middle-age group(37-49/not 37-49), and oldest age group (50+/not 50+)"

# Linear Regression on Average Monthly Spending with demographics
lm_spend_0816_0717_with_demographic <- lm(claims$spend_0816_0717 ~ as.factor(treat)+as.factor(male)+
                                       as.factor(age50)+as.factor(age37_49)+as.factor(white), data=claims)
summary(lm_spend_0816_0717_with_demographic)

# Linear Regression on Average Monthly Spending without demographics
lm_spend_0816_0717_without_demographic  <- lm(claims$spend_0816_0717 ~ as.factor(treat), data=claims)
summary(lm_spend_0816_0717_without_demographic)

# Linear Regression on Average Monthly Spending Office Care with demographics
lm_spendOff_0816_0717_with_demographic <- lm(claims$spendOff_0816_0717 ~ as.factor(treat)+as.factor(male)+
                                          as.factor(age50)+as.factor(age37_49)+as.factor(white), data=claims)
summary(lm_spendOff_0816_0717_with_demographic)

# Linear Regression on Average Monthly Spending Office Care without demographics
lm_spendOff_0816_0717_without_demographic <- lm(claims$spendOff_0816_0717 ~ as.factor(treat), data=claims)
summary(lm_spendOff_0816_0717_without_demographic)

# Linear Regression on Average Monthly Spending Hospital Care with demographics
lm_spendHosp_0816_0717_with_demographic <- lm(claims$spendHosp_0816_0717 ~ as.factor(treat)+as.factor(male)+
                                           as.factor(age50)+as.factor(age37_49)+as.factor(white), data=claims)
summary(lm_spendHosp_0816_0717_with_demographic)

# Linear Regression on Average Monthly Spending Hospital Care without demographics
lm_spendHosp_0816_0717_without_demographic <- lm(claims$spendHosp_0816_0717 ~ as.factor(treat), data=claims)
summary(lm_spendHosp_0816_0717_without_demographic)

# Linear Regression on Average Monthly Spending RX with demographics
lm_spendRx_0816_0717_with_demographic <- lm(claims$spendRx_0816_0717 ~ as.factor(treat)+as.factor(male)+
                                         as.factor(age50)+as.factor(age37_49)+as.factor(white), data=claims)
summary(lm_spendRx_0816_0717_with_demographic)

# Linear Regression on Average Monthly Spending RX without demographics
lm_spendRx_0816_0717_without_demographic <- lm(claims$spendRx_0816_0717 ~ as.factor(treat), data=claims)
summary(lm_spendRx_0816_0717_without_demographic)

# Question 6 - A year following randomization (i.e. 2016 - 2017) for participants and non-participants
"We run linear regression on 4 outcome variables for the year following randomization(i.e. 2016 - 2017)
with the demographics and without the demographics between participants and non-participants. The demographics
we consider are sex(male/female), race(white/nonwhite), middle-age group(37-49/not 37-49), and oldest age group (50+/not 50+)"

# Linear Regression on Average Monthly Spending with demographics
lm_spend_0816_0717_with_demographic <- lm(claims$spend_0816_0717 ~ as.factor(hra_c_yr1)+as.factor(male)+
                                            as.factor(age50)+as.factor(age37_49)+as.factor(white), data=claims)
summary(lm_spend_0816_0717_with_demographic)

# Linear Regression on Average Monthly Spending without demographics
lm_spend_0816_0717_without_demographic  <- lm(claims$spend_0816_0717 ~ as.factor(hra_c_yr1), data=claims)
summary(lm_spend_0816_0717_without_demographic)

# Linear Regression on Average Monthly Spending Office Care with demographics
lm_spendOff_0816_0717_with_demographic <- lm(claims$spendOff_0816_0717 ~ as.factor(hra_c_yr1)+as.factor(male)+
                                               as.factor(age50)+as.factor(age37_49)+as.factor(white), data=claims)
summary(lm_spendOff_0816_0717_with_demographic)

# Linear Regression on Average Monthly Spending Office Care without demographics
lm_spendOff_0816_0717_without_demographic <- lm(claims$spendOff_0816_0717 ~ as.factor(hra_c_yr1), data=claims)
summary(lm_spendOff_0816_0717_without_demographic)

# Linear Regression on Average Monthly Spending Hospital Care with demographics
lm_spendHosp_0816_0717_with_demographic <- lm(claims$spendHosp_0816_0717 ~ as.factor(hra_c_yr1)+as.factor(male)+
                                                as.factor(age50)+as.factor(age37_49)+as.factor(white), data=claims)
summary(lm_spendHosp_0816_0717_with_demographic)

# Linear Regression on Average Monthly Spending Hospital Care without demographics
lm_spendHosp_0816_0717_without_demographic <- lm(claims$spendHosp_0816_0717 ~ as.factor(hra_c_yr1), data=claims)
summary(lm_spendHosp_0816_0717_without_demographic)

# Linear Regression on Average Monthly Spending RX with demographics
lm_spendRx_0816_0717_with_demographic <- lm(claims$spendRx_0816_0717 ~ as.factor(hra_c_yr1)+as.factor(male)+
                                              as.factor(age50)+as.factor(age37_49)+as.factor(white), data=claims)
summary(lm_spendRx_0816_0717_with_demographic)

# Linear Regression on Average Monthly Spending RX without demographics
lm_spendRx_0816_0717_without_demographic <- lm(claims$spendRx_0816_0717 ~ as.factor(hra_c_yr1), data=claims)
summary(lm_spendRx_0816_0717_without_demographic)

