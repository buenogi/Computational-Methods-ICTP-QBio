#Class 9
# A bunch of things

# ODE ----------------------------------

library(deSolve)
library(ggplot2)
library(tidyr)

# Loading function

source("fct/LogGrowth.R")

# named vector with parameters
p <- c(r = 0.8, a = 0.05)
# initial condition
y0 <- c(N = 10)
# time steps
t <- seq(1, 20, lengh.out = 400)


# give the function and the parameters to the ode function
out_log <- ode(y = y0, times = t, func = logGrowth, parms = p)

head(out_log)

df_log <- as.data.frame(out_log)
ggplot(df_log)+
  geom_line(aes(x = time, y = N))+
  theme_classic()

# Lotka Volterra model

source("fct/LotkaV.R")


# LV parameters
a <- matrix(c(0.02, 0.01, 0.01, 0.03), nrow = 2)
r <- c(1, 1)
p2 <- list(r, a)
N0 <- c(10, 10)
t2 <- c(1:100)


out_lv <- ode(y = N0, times = t2, func = LVComp, parms = p2)
head(out_lv)


df_lv <- pivot_longer(as.data.frame(out_lv), cols = 2:3)
head(df_lv)

ggplot(df_lv) +
  geom_line(aes(x = time, y = value, color = name)) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic()


