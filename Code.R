#read data

global_data <- read.csv(file = "Global_Weather.csv", header = TRUE, sep = ",")

lapaz_data <- read.csv(file = "LaPaz_Weather.csv", header = TRUE, sep = ",")

#table sizes

dim(global_data)

dim(lapaz_data)

#table details

str(global_data)

str(lapaz_data)

#moving averages computations

#create the moving average column and fill it with 0's

lapaz_data$mavg <- 0
global_data$mavg <-0

#start the index in 1

i <- 1

#compute the moving average for a time range of 5 years

n <- 5

#moving average for La Paz city

for (i in 1:(nrow(lapaz_data)-n+1)){
  lapaz_data$mavg[i+n-1] <- sum(lapaz_data$avg_temp[i:(i+n-1)])/n
}

#moving average for global data

for (i in 1:(nrow(global_data)-n+1)){
  global_data$mavg[i+n-1] <- sum(global_data$avg_temp[i:(i+n-1)])/n
}

#new dataframe
#a new dataframe is created, with the following  columns: year, source, mavg
alldata1 <- data.frame(year = lapaz_data$year, source = "La Paz", mavg = lapaz_data$mavg)
alldata2 <- data.frame(year = global_data$year, source = "Global", mavg = global_data$mavg)

alldata <- rbind(alldata1, alldata2)

#plots

library(ggplot2)

lapaz_plot <- ggplot(data = lapaz_data, aes(x = year, y = avg_temp)) +
  geom_point(color = "navy") + geom_line(aes(x = year, y = mavg), color = "orange")

print(lapaz_plot)


global_plot <- ggplot(data = global_data, aes(x = year, y = avg_temp)) + 
  geom_point(color = "navy") + geom_line(aes(x = year, y = mavg), color = "orange")

print(global_plot)

combined_plot <- ggplot(data = alldata, aes(x = year, y =mavg)) +
  geom_point(color = "navy") + geom_smooth(method = "lm", se = TRUE) + facet_grid(source~., scales = "free")

print(combined_plot)

plot1 <- ggplot(data = alldata, aes(x = year, y = mavg, col  = source))+ 
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Average Temperatures Moving Averages") +
  xlab("Years") +
  ylab("Temperature")

print(plot1)