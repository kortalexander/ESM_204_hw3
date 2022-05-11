library(tidyverse)
library(rootSolve)

df <- read_csv("G:/My Drive/0UCSB_EES/3TA_Material/S22_ESM204/Homework 3/HW3_data.csv") %>% 
  select(-1) %>% 
  clean_names()

model_demand_l <- lm(price_cents  ~ q_low_kwh, data=df)
model_demand_h <- lm(price_cents ~ q_high_kwh, data=df)

# need to rearrange the parameter to get Q(P)! 

# Qgg = Qlow(P) + Qlow(h) 

# Importantly, since they-intercepts are different, we know that Qagg(P) will have a kink. I include an ifelse() statement to take
# care of the kink.

# define a function to get demand

demand <- function(p, model){
  q <- (p - model$coefficients[[1]])/model$coefficients[[2]]
  q <- ifelse(q<0,0,q)
  return(q)
}

# for each p level, return estimated aggregate demand
demand_agg <- function(p){
  q <- demand(p, model_demand_l) + demand(p, model_demand_h)
  return(q)
}

price = seq(0, 30, length.out = 100)
Qagg <- map(price, demand_agg) %>% unlist()

df<- tibble(Qagg = Qagg, price = price)

ggplot(df, aes(Qagg, price)) +
  geom_line()

# I also define functions for calculating the consumer surplus:

CS <- function(p, model){
  q <- demand(p, model)
  cs <- 0.5*(model$coefficients[[1]] - p)*q
  return(cs)
}

CS_agg <- function(p){
  cs <- CS(p,model_demand_l) + CS(p,model_demand_h)
  return(cs)
}