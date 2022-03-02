# Team Members - Garima Singhal, Sirisha Gadepalli, Ayush Ghildyal
# Setting to working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Implement and RD
# Load libraries
library("tidyverse")

# Load data 
mortality <- haven::read_dta("mortality.dta")
names(mortality)

# Above MDA indicator
mortality <- mortality %>%
  mutate(post = agemo_mda > 0)

# Calculate rates of death (ROD) in units of deaths per 100,000 person-years
mortality <- mortality %>%
  mutate(rod_any = cod_any * 100000 / (pop/12),
         rod_MVA = cod_MVA * 100000 / (pop/12))

# Part A
mortality_aboveMLDA <- mortality %>%
  filter(agemo_mda >= 1 & agemo_mda <= 24) %>%
  mutate(rod_any = cod_any * 100000 / (pop/12))
summary(mortality_aboveMLDA)
  
mortality_belowMLDA <- mortality %>%
  filter(agemo_mda >= -24 & agemo_mda <= -1) %>%
  mutate(rod_any = cod_any * 100000 / (pop/12))
summary(mortality_belowMLDA)

# Part B
# Scatter plot of ROD between any cause of death 
# and cause of death due to motor vehicles accidents
filter(mortality, between(agemo_mda, -24, 24)) %>%
  ggplot(aes(x = agemo_mda)) + 
  geom_point(aes(y = rod_any), color = "black", shape = "square") +
  geom_point(aes(y = rod_MVA), color = "blue") +
  geom_vline(xintercept = 0, linetype = "solid", color = "red", size = 1) +
  labs(y= "rate of death : rod_any vs rod_MVA", x = "agemo_mda")

# Part C
# "Donut" Non-Parametric RD
# Cause of death any

# Cause of death any with bandwidth of 6 months
rd6_any <- lm(rod_any ~ post,
              data = mortality,
              subset = between(agemo_mda, -6, 6) & agemo_mda != 0)
broom::tidy(rd6_any)

# Cause of death any with bandwidth of 12 months
rd12_any <- lm(rod_any ~ post,
               data = mortality,
               subset = between(agemo_mda, -12, 12) & agemo_mda != 0)
broom::tidy(rd12_any)

# Cause of death any with bandwidth of 24 months
rd24_any <- lm(rod_any ~ post,
               data = mortality,
               subset = between(agemo_mda, -24, 24) & agemo_mda != 0)
broom::tidy(rd24_any)

# Cause of death any with bandwidth of 48 months
rd48_any <- lm(rod_any ~ post,
               data = mortality,
               subset = between(agemo_mda, -48, 48) & agemo_mda != 0)
broom::tidy(rd48_any)

# Cause of death MVA(Motor Vehicle Accidents)

# Cause of death due to motor vehicle accidents with bandwidth of 6 months
rd6_mva <- lm(rod_MVA ~ post,
              data = mortality,
              subset = between(agemo_mda, -6, 6) & agemo_mda != 0)
broom::tidy(rd6_mva)

# Cause of death due to motor vehicle accidents with bandwidth of 12 months
rd12_mva <- lm(rod_MVA ~ post,
               data = mortality,
               subset = between(agemo_mda, -12, 12) & agemo_mda != 0)
broom::tidy(rd12_mva)

# Cause of death due to motor vehicle accidents with bandwidth of 24 months
rd24_any <- lm(rod_MVA ~ post,
               data = mortality,
               subset = between(agemo_mda, -24, 24) & agemo_mda != 0)
broom::tidy(rd24_any)

# Cause of death due to motor vehicle accidents with bandwidth of 48 months
rd48_any <- lm(rod_MVA ~ post,
               data = mortality,
               subset = between(agemo_mda, -48, 48) & agemo_mda != 0)
broom::tidy(rd48_any)

# Part D
# "Donut" Parametric RD
# Cause of death any

# Cause of death any with bandwidth of 6 months
p_rd6_any <- lm(rod_any ~ post * agemo_mda,
              data = mortality,
              subset = between(agemo_mda, -6, 6) & agemo_mda != 0)
broom::tidy(p_rd6_any)

# Cause of death any with bandwidth of 12 months
p_rd12_any <- lm(rod_any ~ post * agemo_mda,
               data = mortality,
               subset = between(agemo_mda, -12, 12) & agemo_mda != 0)
broom::tidy(p_rd12_any)

# Cause of death any with bandwidth of 24 months
p_rd24_any <- lm(rod_any ~ post * agemo_mda,
               data = mortality,
               subset = between(agemo_mda, -24, 24) & agemo_mda != 0)
broom::tidy(p_rd24_any)

# Cause of death any with bandwidth of 48 months
p_rd48_any <- lm(rod_any ~ post * agemo_mda,
               data = mortality,
               subset = between(agemo_mda, -48, 48) & agemo_mda != 0)
broom::tidy(p_rd48_any)

# Cause of death MVA(Motor Vehicle Accidents)

# Cause of death due to motor vehicle accidents with bandwidth of 6 months
p_rd6_mva <- lm(rod_MVA ~ post * agemo_mda,
              data = mortality,
              subset = between(agemo_mda, -6, 6) & agemo_mda != 0)
broom::tidy(p_rd6_mva)

# Cause of death due to motor vehicle accidents with bandwidth of 12 months
p_rd12_mva <- lm(rod_MVA ~ post * agemo_mda,
               data = mortality,
               subset = between(agemo_mda, -12, 12) & agemo_mda != 0)
broom::tidy(p_rd12_mva)

# Cause of death due to motor vehicle accidents with bandwidth of 24 months
p_rd24_any <- lm(rod_MVA ~ post * agemo_mda,
               data = mortality,
               subset = between(agemo_mda, -24, 24) & agemo_mda != 0)
broom::tidy(p_rd24_any)

# Cause of death due to motor vehicle accidents with bandwidth of 48 months
p_rd48_any <- lm(rod_MVA ~ post * agemo_mda,
               data = mortality,
               subset = between(agemo_mda, -48, 48) & agemo_mda != 0)
broom::tidy(p_rd48_any)

            
