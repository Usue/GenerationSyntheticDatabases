###############################################
#################SIMULATION FUNCTION###########
###############################################

#This function creates a synthetic database of type ARMA with the
#following parameters: 
  #num.series: # of series in the database (integer).
  #length.series: length of series in the database (integer).
  #noise.level: level of noise introduced in database (real).
  #outlier.level: proportion of outliers introduced in database  (integer).
  #shift.level: mean level of shift introduced in the database (integer).
  #classpercentages: proportion of series in each clusters. (vector of length 8 that sums 1)


armas<-function(num.series, length.series, noise.level, outlier.level, shift.level, class.percentages){

#An initial check for errors is done
armaInitialCheck(num.series, length.series, noise.level, outlier.level, shift.level, class.percentages)
  
#Create the list where the synthetic series will be saved.
database<-list() 
#Create a vector where the cluster number of each series will be saved.
clases<-c() 

#The shift level is normalized based on the series length.
shift.level <- shift.level * length.series / 100

#The maximum value to which the series may be shifted is calculated.
max.shift <- round((3 * shift.level - 2 + sqrt(9 * shift.level ^ 2 + 4)) / 4)

#The number of series in each cluster is calculated.
class.frequencies <- round(num.series * class.percentages)

#The database is created:

#The series from the first cluster are generated.
if (class.frequencies[1] != 0){
  for (i in 1:class.frequencies[1]){
    #A specific shift value is sampled from the defined interval.
    set.seed(i)
    shift <-sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[i]] <- a1(length.series, max.shift, shift)
    #The cluster number is saved.
    clases[i] <- 1
  }
}

#The series from the second cluster are generated.
if (class.frequencies[2] != 0){
  for (i in 1:class.frequencies[2]){
    #A specific shift value is sampled from the defined interval.
    set.seed(class.frequencies[2]+i)
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[class.frequencies[1] + i]] <- a2(length.series, max.shift, shift)
    #The cluster number is saved.
    clases[class.frequencies[1] + i] <- 2
  }
}

#The series from the third cluster are generated.
if (class.frequencies[3] != 0){
  for(i in 1:class.frequencies[3]){
    #A specific shift value is sampled from the defined interval.
    set.seed(class.frequencies[3]+i)
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[sum(class.frequencies[1:2]) + i]] <- a3(length.series, max.shift, shift)
    #The cluster number is saved.
    clases[sum(class.frequencies[1:2]) + i] <- 3
  }
}

#The series from the fourth cluster are generated.
if (class.frequencies[4] != 0){
  for(i in 1:class.frequencies[4]){
    #A specific shift value is sampled from the defined interval.
    set.seed(class.frequencies[4]+i)
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[sum(class.frequencies[1:3]) + i]] <- a4(length.series, max.shift, shift)
    #The cluster number is saved.
    clases[sum(class.frequencies[1:3]) + i] <- 4
  }
}

#The series from the fifth cluster are generated.
if (class.frequencies[5] != 0){
  for (i in 1:class.frequencies[5]){
    #A specific shift value is sampled from the defined interval.
    set.seed(class.frequencies[5]+i)
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[sum(class.frequencies[1:4]) + i]] <- a5(length.series, max.shift, shift)
    #The cluster number is saved.
    clases[sum(class.frequencies[1:4]) + i] <- 5
  }
}

#The series from the sixth cluster are generated.
if (class.frequencies[6] != 0){
  for (i in 1:class.frequencies[6]){
  #A specific shift value is sampled from the defined interval.
    set.seed(class.frequencies[6]+i)
  shift <- sample(-max.shift:max.shift, 1)
  #The series is generated and saved into the list.
  database[[sum(class.frequencies[1:5]) + i]] <- a6(length.series, max.shift, shift)
  #The cluster number is saved.
  clases[sum(class.frequencies[1:5]) + i] <- 6
  }
}

#The series from the seventh cluster are generated.
if (class.frequencies[7] != 0){
  for (i in 1:class.frequencies[7]){
  #A specific shift value is sampled from the defined interval.
  set.seed(class.frequencies[7]+i)
  shift <- sample(-max.shift:max.shift, 1)
  #The series is generated and saved into the list.
  database[[sum(class.frequencies[1:6]) + i]] <- a7(length.series, max.shift, shift)
  #The cluster number is saved.
  clases[sum(class.frequencies[1:6]) + i] <- 7
  }
}

#The series from the eighth cluster are generated.
if (class.frequencies[8] != 0){
for (i in 1:class.frequencies[8]){
  #A specific shift value is sampled from the defined interval.
  set.seed(class.frequencies[8]+i)
  shift <- sample(-max.shift:max.shift, 1)
  #The series is generated and saved into the list.
  database[[sum(class.frequencies[1:7]) + i]] <- a8(length.series, max.shift, shift)
  #The cluster number is saved.
  clases[sum(class.frequencies[1:7]) + i] <- 8
  }
}


#The noise is added.
if (noise.level != 0){
  for (i in c(1:length(database))){
    #A series of random noise is generated considering the desired level of noise.
    noise <- rnorm(length.series, 0, (max(database[[i]])-min(database[[i]])) * noise.level / 100)
    #It is added to the series.
    database[[i]] <- database[[i]] + noise
  }
}

#The outliers are added.
if (outlier.level != 0){
  #The number of points that will become outliers are selected.
  num.points <- length.series * outlier.level / 100
  #For each series in the database:
  for(i in c(1:length(database))){
    #The exact points that will be modified are selected.
    positions <- sample(1:length.series, num.points)
    #For each of the selected points a value from another series is selected.
    for(j in c(1:length(positions))){
      a <- sample(1:length(database), 1)
      b <- sample(1:length.series, 1)
      #The value is interchanged.
      database[[i]][positions[j]] <- database[[a]][b]
    }
 }
}

#The database and the cluster values are returned in a list.
return(list(database,clases))

}

###############################################
#################INITIAL ERRORS################
###############################################

#This function checks for possible errors in the introduced parameters.

armaInitialCheck<-function(num.series, length.series, noise.level, outlier.level, shift.level, class.percentages){
  if (sum(class.percentages) != 1){
    stop('Class percentages must sum 1')
  }
  if (length(class.percentages) != 8){
    stop('You must give percentages for 8 classes')
  }
  if (num.series < 1){
    stop('The number of series must be larger than 0')
  }
  if (length.series < 1){
    stop('The length of the series must be larger than 0')
  }
  if (noise.level < 0){
    stop('The noise level must be positive')
  }
  if (outlier.level < 0){
    stop('The outlier level must be positive')
  }
  if (shift.level < 0){
    stop('The shift level must be positive')
  }
}

###############################################
#################BASIC SHAPES##################
###############################################

a1<-function(length.series,max.shift,shift){
  #The seed is selected.
  set.seed(123)
  #The innovations are created. They are specific to this shape.
  innovations <- rnorm((length.series + 100 + 2 * abs(max.shift)), 0, 1)
  #An arma series is generated based on the innovations.
  serie <- arima.sim(list(order=c(3,0,2), ar=c(1,-0.24,0.1), ma=c(1,1.2)), n=(length.series + 100 + 2 * abs(max.shift)), n.start=100, innov=innovations)
  #The series is shifted.
  serie <- serie[(101 + abs(max.shift) + shift):(length(serie) - abs(max.shift) + shift)]
  return(serie)
}


a2<-function(length.series,max.shift,shift){
 #The seed is selected.
 set.seed(456)
 #The innovations are created. They are specific to this shape.
 innovations <- rnorm((length.series + 100 + 2 * abs(max.shift)), 0, 1)
 #An arma series is generated based on the innovations.
 serie <- arima.sim(list(order=c(3,0,2), ar=c(1,-0.24,0.1), ma=c(1,1.2)), n=(length.series + 100 + 2 * abs(max.shift)), n.start=100, innov=innovations)
 #The series is shifted.
 serie <- serie[(101 + abs(max.shift) + shift):(length(serie) - abs(max.shift) + shift)]
 return(serie)
}


a3<-function(length.series,max.shift,shift){
 #The seed is selected.
 set.seed(789)
 #The innovations are created. They are specific to this shape.
 innovations <- rnorm((length.series + 100 + 2 * abs(max.shift)), 0, 1)
 #An arma series is generated based on the innovations.
 serie <- arima.sim(list(order=c(3,0,2), ar=c(1,-0.24,0.1), ma=c(1,1.2)), n=(length.series + 100 + 2 * abs(max.shift)), n.start=100, innov=innovations)
 #The series is shifted.
 serie <- serie[(101 + abs(max.shift) + shift):(length(serie) - abs(max.shift) + shift)]
 return(serie)
}


a4<-function(length.series,max.shift,shift){
  #The seed is selected.
 set.seed(1011)
 #The innovations are created. They are specific to this shape.
 innovations <- rnorm((length.series + 100 + 2 * abs(max.shift)), 0, 1)
 #An arma series is generated based on the innovations.
 serie <- arima.sim(list(order=c(3,0,2), ar=c(1,-0.24,0.1), ma=c(1,1.2)), n=(length.series + 100 + 2 * abs(max.shift)), n.start=100, innov=innovations)
 #The series is shifted.
 serie <- serie[(101 + abs(max.shift) + shift):(length(serie) - abs(max.shift) + shift)]
 return(serie)
}


a5<-function(length.series,max.shift,shift){
  #The seed is selected.
  set.seed(1112)
  #The innovations are created. They are specific to this shape.
  innovations <- rnorm((length.series + 100 + 2 * abs(max.shift)), 0, 1)
  #An arma series is generated based on the innovations.
  serie <- arima.sim(list(order=c(3,0,2), ar=c(1,-0.24,0.1), ma=c(1,1.2)), n=(length.series + 100 + 2 * abs(max.shift)), n.start=100, innov=innovations)
  #The series is shifted.
  serie <- serie[(101 + abs(max.shift) + shift):(length(serie) - abs(max.shift) + shift)]
  return(serie)
}

a6<-function(length.series,max.shift,shift){
  #The seed is selected.
  set.seed(115)
  #The innovations are created. They are specific to this shape.
  innovations <- rnorm((length.series + 100 + 2 * abs(max.shift)), 0, 1)
  #An arma series is generated based on the innovations.
  serie <- arima.sim(list(order=c(3,0,2), ar=c(1,-0.24,0.1), ma=c(1,1.2)), n=(length.series + 100 + 2 * abs(max.shift)), n.start=100, innov=innovations)
  #The series is shifted.
  serie <- serie[(101 + abs(max.shift) + shift):(length(serie) - abs(max.shift) + shift)]
  return(serie)
}

a7<-function(length.series,max.shift,shift){
  #The seed is selected.
  set.seed(111)
  #The innovations are created. They are specific to this shape.
  innovations <- rnorm((length.series + 100 + 2 * abs(max.shift)), 0, 1)
  #An arma series is generated based on the innovations.
  serie <- arima.sim(list(order=c(3,0,2), ar=c(1,-0.24,0.1), ma=c(1,1.2)), n=(length.series + 100 + 2 * abs(max.shift)), n.start=100, innov=innovations)
  #The series is shifted.
  serie <- serie[(101 + abs(max.shift) + shift):(length(serie) - abs(max.shift) + shift)]
  return(serie)
}

a8<-function(length.series,max.shift,shift){
  #The seed is selected.
  set.seed(1871)
  #The innovations are created. They are specific to this shape.
  innovations <- rnorm((length.series + 100 + 2 * abs(max.shift)), 0, 1)
  #An arma series is generated based on the innovations.
  serie <- arima.sim(list(order=c(3,0,2), ar=c(1,-0.24,0.1), ma=c(1,1.2)), n=(length.series + 100 + 2 * abs(max.shift)), n.start=100, innov=innovations)
  #The series is shifted.
  serie <- serie[(101 + abs(max.shift) + shift):(length(serie) - abs(max.shift) + shift)]
  return(serie)
}

