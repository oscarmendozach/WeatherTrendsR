n <- 7

for (i in (nrow(test)- n + 1)){
  test$mavg[i] <- i
  print(test$mavg[i])
  #test$mavg[i+n-1] <- sum(test$avg_temp[i:i+n-1])/n
  i <- i + 1
}

i <- 1
for (i in 1:nrow(test)){
  print(i)
}