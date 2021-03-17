#read data

global_data <- read.csv(file = "Global_Weather.csv", header = TRUE, sep = ",")

lapaz_data <- read.csv(file = "LaPaz_Weather.csv", header = TRUE, sep = ",")

#moving averages computations


#compute the moving average for a time range of different years in vector n

n <- c(2, 5, 10, 20, 25, 50)

#moving average for La Paz city

names_index <- length(names(test1))

dataframe_prueba <- lapaz_data

for (i in 1:length(n)){
  column_name <- paste("mavg", n[i], sep = "")
  new_data <- rep(0, nrow(lapaz_data))
  for (j in 1:(nrow(lapaz_data)-n[i]+1)){
    new_data[j+n[i]-1] <- sum(lapaz_data$avg_temp[j:(j+n[i]-1)])/n[i]
  }
  lapaz_data[ , ncol(lapaz_data) + 1] <- new_data
  colnames(lapaz_data)[ncol(lapaz_data)] <- column_name
}

new_data <- rep(0, nrow(lapaz_data))
n <- c(2, 5, 10, 20, 25, 50)

i <- 2



for (j in 1:(nrow(lapaz_data)-n[i]+1)){
  print(j)
  print(sum(lapaz_data$avg_temp[j:(j+n[i]-1)]))
  print(n[i])
  new_data[(j+n-1)] <- sum(lapaz_data$avg_temp[j:(j+n[i]-1)])/n[i]
  print(new_data[j+n-1])
}