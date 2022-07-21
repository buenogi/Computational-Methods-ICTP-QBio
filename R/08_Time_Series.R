# Time series

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

covid <- read.csv("data/raw/covid19-dd7bc8e57412439098d9b25129ae6f35.csv")

# First checking the class
class(covid$date)

# Changing to date format
covid$date <- as_date(covid$date)

# Checking the class
class(covid$date)

# Now we can make numeric operations
range(covid$date)

summary(covid$date)

#ploting

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal()

# Applying transformations

covid$new_confirmed[covid$new_confirmed < 0] <- 0


ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal()+
  scale_x_date(breaks = "4 months", date_labels = "%Y-%m")+
  labs(x = "Date", y = "New Cases" )

# Rolling mean

covid$roll_mean <- zoo::rollmean(covid$new_confirmed, k =14, fill = NA, ) # adicionando a média movel

head(covid)


ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal()+
  scale_x_date(breaks = "4 months", date_labels = "%Y-%m")+
  labs(x = "Date", y = "New Cases" )+
  geom_line(aes(x = date , y = roll_mean), color = "red", size = 1.2)
