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


#compute the moving average for a time range of different years in vector n

n <- c(2, 5, 10, 20, 25, 50)

#moving average for La Paz city

for (i in 1:length(n)){
  column_name <- paste("mavg", n[i], sep = "")
  new_data <- rep(0, nrow(lapaz_data))
  for (j in 1:(nrow(lapaz_data)-n[i]+1)){
    new_data[j+n[i]-1] <- sum(lapaz_data$avg_temp[j:(j+n[i]-1)])/n[i]
  }
  lapaz_data[ , ncol(lapaz_data) + 1] <- new_data
  colnames(lapaz_data)[ncol(lapaz_data)] <- column_name
}


#moving average for global data

for (i in 1:length(n)){
  column_name <- paste("mavg", n[i], sep = "")
  new_data <- rep(0, nrow(global_data))
  for (j in 1:(nrow(global_data)-n[i]+1)){
    new_data[j+n[i]-1] <- sum(global_data$avg_temp[j:(j+n[i]-1)])/n[i]
  }
  global_data[ , ncol(global_data) + 1] <- new_data
  colnames(global_data)[ncol(global_data)] <- column_name
}

#new dataframe

#a new dataframe is created, with the following  columns: year, source, variable, value

library(reshape2)

lapaz_computeddata <- melt(lapaz_data, id = c("year", "city", "country"))

#rename the column names
names(lapaz_computeddata) <- c("year", "source", "country", "variable", "value")

#drop the column "country" because is no longer necessary

lapaz_computeddata <- subset(lapaz_computeddata, select = c("year", "source", "variable", "value"))

#we do the same for the global dataframe

global_computeddata <- melt(global_data, id = c("year"))

#add the column "source

global_computeddata$source <- "Global"

#reorder the DataFrame 

global_computeddata <- global_computeddata[ , c(1, 4, 2, 3)]

#bind both dataframes

alldata <- rbind(lapaz_computeddata, global_computeddata)

#plots

library(ggplot2)

#La Paz plot

lapaz_avg_temp <- lapaz_computeddata %>% filter(variable == "avg_temp")

lapaz_computeddata %>% filter(variable != "avg_temp", value > 0) %>% 
  ggplot (aes(x = year, y = value, color = variable)) + 
  geom_line() +
  geom_point(data = lapaz_avg_temp, aes(x = year, y = value)) +
  ggtitle("Averaged measured temperatures in La Paz, Bolivia and Computed Moving Averages") +
  xlab("Time [years]") +
  ylab("Temperature [degrees Celsius]")

#Global Plot

global_avg_temp <- global_computeddata %>% filter(variable == "avg_temp")

global_computeddata %>% filter(variable != "avg_temp", value > 0) %>%
  ggplot(aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point(data = global_avg_temp, aes(x = year, y = value)) +
  ggtitle("Averaged measured Global Temperatures and Computed Moving Averages") +
  xlab("Time [years]") + 
  ylab("Temperature [degrees Celsius]")


#Combined plot

alldata %>% filter(value > 0) %>%
  ggplot( aes(x = year, y =value, color = source)) +
  geom_point() + 
  facet_grid(~variable, scales = "free") +
  ggtitle("Temperature Comparisson between Global average temperatures and La Paz", subtitle = "Measured Temperatures and Computed Moving Averages") +
  xlab("Time [years]") + 
  ylab("Temperature [Degrees Celsius]")

#mavg25 dataframe
mavg25 <- data.frame(year = lapaz_data$year, lapaz_mavg25 = lapaz_data$mavg25, global_mavg25 = global_data$mavg25)
mavg25$difference <- mavg25$lapaz_mavg25 - mavg25$global_mavg25