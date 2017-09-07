
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
#warp.level: Level of warp introduced in the database (integer).
#class.percentages: proportion of series in each clusters. (vector of length 5 that sums 1)


kohlerlorenz<-function(num.series, length.series, noise.level, outlier.level, shift.level, warp.level, class.percentages){

#An initial check for errors is done
kohlerInitialCheck(num.series, length.series, noise.level, outlier.level, shift.level, warp.level, class.percentages)

#Create the list where the synthetic series will be saved.
database<-list() 
#Create a vector where the cluster number of each series will be saved.
clases<-c() 

#The shift level is normalized based on the series length.
shift.level <- shift.level * length.series / 100

#The maximum value to which the series may be shifted is calculated.
max.shift <- round((3 * shift.level - 2 + sqrt(9 * shift.level ^ 2 + 4)) / 4)

#The warping interval is defined.
max.warp <- floor(warp.level * length.series / 100)

if ((round(length.series/3) + (max.shift + max.warp) * 1.1) > length.series){
  stop('Shift and warp levels are too large for this type of database.')
}

if ((round(length.series/2) + (max.shift) * 1.1) > length.series){
  stop('Shift level is too large for this type of database.')
}

if ((round(length.series/4) - (max.shift) * 1.1) < 1){
  stop('Shift level is too large for this type of database.')
}


#The number of series in each cluster is calculated.
class.frequencies <- round(num.series * class.percentages)

if (class.frequencies[1] != 0){
  for (i in 1:class.frequencies[1]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[i]] <- k1(length.series, shift)
    #The cluster number is saved.
    clases[i] <- 1
  }
}

if (class.frequencies[2]!=0){
  for (i in 1:class.frequencies[2]){
  #A specific shift value is sampled from the defined interval.
  shift <- sample(-max.shift:max.shift, 1)
  #The series is generated and saved into the list. 
  database[[class.frequencies[1] + i]] <- k2(length.series, shift)
  #The cluster number is saved.
  clases[class.frequencies[1] + i] <- 2
  }
}

if(class.frequencies[3] != 0){
  for (i in 1:class.frequencies[3]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #A specific warp value is sampled from the defined interval.
    warp <- sample(-max.warp:max.warp, 1)
    #The series is generated and saved into the list.
    database[[sum(class.frequencies[1:2]) + i]] <- k3(length.series, shift, warp)
    #The cluster number is saved.
    clases[sum(class.frequencies[1:2])+i]<-3
  }
}

if (class.frequencies[4] != 0){
  for (i in 1:class.frequencies[4]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list. 
    database[[sum(class.frequencies[1:3]) + i]] <- k4(length.series, shift)
    #The cluster number is saved.
    clases[sum(class.frequencies[1:3]) + i] <- 4
  }
}
     
if (class.frequencies[5] != 0){   
  for(i in 1:class.frequencies[5]){
    #A specific shift value is sampled from the defined interval.
    set.seed(i)
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list. 
    database[[sum(class.frequencies[1:4])+ i]] <- k5(length.series, max.shift, shift)
    #The cluster number is saved.
    clases[sum(class.frequencies[1:4]) + i] <- 5
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

kohlerInitialCheck<-function(num.series, length.series, noise.level, outlier.level, shift.level, warp.level, class.percentages){
  if (sum(class.percentages) != 1){
    stop('Class percentages must sum 1')
  }
  if (length(class.percentages) != 5){
    stop('You must give percentages for 5 classes')
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
  if (warp.level < 0){
    stop('The warp level must be positive')
  }
}

###############################################
#################BASIC SHAPES##################
###############################################

k1 <- function(length.series, shift){
  #A sinusoidal function is defined.
  T<-length.series * 0.1
  serie<-rnorm(1, 1, 0.1) * 13 * sin((5 * c(1:length.series) + shift) / T)
  return(serie)
}

k2<-function(length.series, shift){
  #The cut point is defined.
  cut.point<-round(length.series/2)+shift
  #The possible errors for the cut point are identified.
  if(cut.point <= 0){cut.point<-1}
  if(cut.point > length.series){cut.point <- length.series}
  #The series is created.
  series <- c(1:length.series)
  series[1:cut.point] <- c(1:cut.point) ^ 2 / 100
  series[(cut.point + 1):length.series]<-c((cut.point+1):length.series) ^ 2 / 530
  return(series)
}

k3<-function(length.series, shift, warp){
  #The cut points are defined.
  cut.point1 <- round(length.series / 3) + (shift + warp) * rnorm(1,1,0.1)
  cut.point2<-round(2 * length.series / 3) + shift * rnorm(1,1,0.1)
  #The possible errors for the cut point are identified.
  if(cut.point1 <= 0){cut.point1 <- 1}
  if(cut.point2 > length.series){cut.point2 <- length.series} 
  #The piecewise constant function is defined.
  series<-c(1:length.series)
  series[1:cut.point1] <- 5
  series[(cut.point1+1):cut.point2] <- 12
  series[(cut.point2+1):length.series] <- 8
  return(series)
}

k4<-function(length.series, shift){
  #The cut points are defined.
  cut.point1<-round(length.series / 8) + shift * rnorm(1, 1, 0.1)
  cut.point2<-round(2 * length.series / 8) + shift * rnorm(1, 1, 0.1)
  cut.point3<-round(3 * length.series / 8) + shift * rnorm(1, 1, 0.1)
  cut.point4<-round(4 * length.series / 8) + shift * rnorm(1, 1, 0.1)
  cut.point5<-round(5 * length.series / 8) + shift * rnorm(1, 1, 0.1)
  #The possible errors for the cut point are identified.
  if(cut.point1 <= 0){cut.point1 <- 1}
  if(cut.point5>length.series){cut.point5<-length.series}
  #The series is created.
  serie<-c(1:length.series)
  serie[1:cut.point1]<-rnorm(1, 15, 0.1) * sin(5 * rnorm(1,1,0.1) * c(1:cut.point1))
  serie[(cut.point1+1):cut.point2] <- rnorm(1, 1, 0.1) * sin(7 * rnorm(1, 1, 0.1) * c((cut.point1 + 1):cut.point2))
  serie[(cut.point2+1):cut.point3] <- 10
  serie[(cut.point3+1):cut.point4] <- 2
  serie[(cut.point4+1):cut.point5] <- c(((cut.point4 + 1):cut.point5) / (length.series / 10)) ^ 2 / 2
  serie[(cut.point5+1):length.series] <- c(((cut.point5 + 1):length.series) / (length.series / 2)) ^ 2 / 2   
  return(serie)
}

k5<-function(length.series,max.shift,shift){
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
