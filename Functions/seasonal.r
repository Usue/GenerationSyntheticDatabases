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
#classpercentages: proportion of series in each clusters. (vector of length 4 that sums 1)

seasonal<-function(num.series, length.series, noise.level, outlier.level, shift.level, class.percentages){
   
  #An initial check for errors is done
  seasonalInitialCheck(num.series, length.series, noise.level, outlier.level, shift.level, class.percentages)
 
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
if(class.frequencies[1] != 0){
  for(i in 1:class.frequencies[1]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[i]] <- s1(length.series, shift)
    #The cluster number is saved.
    clases[i] <- 1
    }
  }
  
if(class.frequencies[2] != 0){
    for(i in 1:class.frequencies[2]){
      #A specific shift value is sampled from the defined interval.
      shift <- sample(-max.shift:max.shift, 1)
      #The series is generated and saved into the list.
      database[[class.frequencies[1] + i]] <- s2(length.series, shift)
      #The cluster number is saved.
      clases[class.frequencies[1] + i] <- 2
  }
}
  
  if(class.frequencies[3] != 0){
    for(i in 1:class.frequencies[3]){
      #A specific shift value is sampled from the defined interval.
      shift <- sample(-max.shift:max.shift, 1)
      #The series is generated and saved into the list.
      database[[sum(class.frequencies[1:2]) + i]] <- s3(length.series, shift)
      #The cluster number is saved.
      clases[sum(class.frequencies[1:2]) + i] <- 3
    }
  }
  
  if(class.frequencies[4] != 0){
    for(i in 1:class.frequencies[4]){
      #A specific shift value is sampled from the defined interval.
      shift <- sample(-max.shift:max.shift, 1)
      #The series is generated and saved into the list.
      database[[sum(class.frequencies[1:3]) + i]] <- s4(length.series, shift)
      #The cluster number is saved.
      clases[sum(class.frequencies[1:3]) + i] <- 4
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
  
return(list(database,clases))
  
}

#This function checks for possible errors in the introduced parameters.

seasonalInitialCheck<-function(num.series, length.series, noise.level, outlier.level, shift.level, class.percentages){
  if (sum(class.percentages) != 1){
    stop('Class percentages must sum 1')
  }
  if (length(class.percentages) != 4){
    stop('You must give percentages for 4 classes')
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


s1<-function(length.series, shift){
  
  base <- c(1:length.series)
  #A basic period for the sinusoidal forms is generated.
  T <- length(base)*0.5
  #A set of sinusoidal forms are added.
  series <- sin(2 * pi * (base + shift) / T) + 1/2 * sin(2 * (2 * pi * (base + shift) / T) - 2) + 1/3 * cos(4 * (2 * pi * (base + shift) / T) - 2) + sin(5 * (2 * pi * (base + shift) / T) - 6)
  #The series is multiplied by a random number.
  multi<-runif(1,1,1.3)
  series<-series*multi
  return(series)
}


s2<-function(length.series, shift){
  
  base <- c(1:length.series)
  #A basic period for the sinusoidal forms is generated.
  T <- length(base)*0.5
  #A set of sinusoidal forms are added.
  series<-1.5 * sin(2  * pi * (base + shift) / T) + 1/2 * cos(6 * (2 * pi * (base + shift) / T) - 1) + 1/6 * cos(2 * (2 * pi * (base + shift) / T) - 7) + sin(2 * (2 * pi * (base + shift) / T) - 9)
  #The series is multiplied by a random number.
  multi<-runif(1,1,1.3)
  series<-series*multi
  return(series)
}


s3<-function(length.series, shift){
  
  base <- c(1:length.series)
  #A basic period for the sinusoidal forms is generated.
  T <- length(base)*0.5
  series<- 1.2 * cos(3*(2 * pi * (base + shift) / T) + 7) + 0.9 * sin((2 * pi * (base + shift) / T) - 2) + 1/3 * cos((2 * pi * (base + shift) / T) / 7)
  #The series is multiplied by a random number.
  multi<-runif(1,1,1.3)
  series<-series*multi
  return(series)
}


s4<-function(length.series, shift){
  
  base <- c(1:length.series)
  #A basic period for the sinusoidal forms is generated.
  T <- length(base)*0.5
  series<-1.2 * sin(2.2 * (2 * pi * (base + shift) / T) + 7) / 5 + 0.5 * cos((2 * pi * (base + shift) / T) - 5) + 1/3 * cos((2 * pi * (base+shift) / T) / 7)
  #The series is multiplied by a random number.
  multi<-runif(1,1,1.3)
  series<-series*multi
  return(series)
}


