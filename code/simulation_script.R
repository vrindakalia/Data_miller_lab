##### 
# A test script
# 02.05.2020
library(tidyverse)
library(tidyr)


##### 
#Create simulation data
# Create a growth curve for worms
# Data is measure of worm length over time
set.seed(16)
n <- 500 # number of observations
a <- 5 # intercept - size of an egg
b <- 14 # slope, 14 mm increase in size with a unit increase (1 hour) in time
var <- 10^2 # variation in size 

time <- runif(n, min = 0, max = 24*3) # set up measures of time - uniform distribution- over three days

er <- rnorm(n, mean = 0, sd = sqrt(var)) # normally distributed error

length <- a + b*time  + er # length measured (simulated)

growthfit  <- lm(length ~ time)

summary(growthfit)
#mean(time)
#mean(length)

plot(time, length) # relationship between time and length 

worm.grow <- data.frame(time, length, er) %>% # create  a dataframe to use in ggplot below
    mutate(stage = case_when(time < 20 ~ "L1", # create a factor variable to store stage information
                             time >= 20 & time < 27 ~ "L2",
                             time >= 27 & time < 34 ~ "L3",
                             time >= 34 & time < 43 ~ "L4",
                             time >=43 ~ "Adult"))


ggplot(data = worm.grow, aes(x = time, y = length)) +
    geom_point() +
    theme_classic() +
    geom_smooth(method = 'lm') #fit a line using lm() (linear regression)


ggplot(data= worm.grow, aes(x = stage, y = length)) +
    geom_boxplot()

# reorder factor levels to be: L1, L2, L3, L4 and Adult
fct_reorder(worm.grow$stage, worm.grow$length) %>%  # Reorder based on median length
    levels() %>% head()

ggplot(data= worm.grow, aes(x = fct_reorder(stage, length), y = length)) +
    geom_boxplot() +
    xlab("Worm stage") +
    ylab("Worm length (mm)")


#######
# March 11, 2020: https://aosmith.rbind.io/2018/01/09/simulate-simulate-part1/
# Create a function
library(purrr)
library(broom)
suppressMessages( library(dplyr) )
library(ggplot2)

worm_grow_fun = function(n = 500, a =  5, b = 14, var = 100, days = 3) {
    time <- runif(n, min = 0, max = 24*days) # set up measures of time - uniform distribution- over three days
    er <- rnorm(n, mean = 0, sd = sqrt(var)) # normally distributed error
    length <- a + b*time  + er # length measured (simulated)
    growthfit  <- lm(length ~ time)
}

sims = rerun(1000, worm_grow_fun()) #use rerun() function to run simulation 1000 times

tidy(growthfit) # observe what tidy version of model output  looks like

summary(growthfit)$sigma #what  did the standard deviation?

sims %>%
    map_df(tidy) %>%
    filter(term == "time") %>% #time coefficient
    ggplot( aes(estimate)) + # plot the estimate
    geom_density(fill = "blue", alpha = .5) +
    geom_vline( xintercept = 14) #add line at expectation

sims %>%
    map_dbl(~summary(.x)$sigma) %>% #extract the standard deviation
    data.frame(sigma = .) %>%
    ggplot(aes(sigma)) +
    geom_density(fill = "blue", alpha = .5) +
    geom_vline(xintercept = 10) #add line at expectation

sims %>%
    map_dbl(~summary(.x)$sigma) %>%
    {. < 10} %>% #how many times was the standard deviation less than expected?
    mean()

#sims %>%
#    map_df(tidy) %>%
#    filter(term == "time") %>%
#    pull(p.value) %>%
#    {. <  0.05} %>%
#    mean()
