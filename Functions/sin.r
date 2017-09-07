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
#classpercentages: proportion of series in each clusters. (vector of length 5 that sums 1)

sinseries <- function(num.series, length.series, noise.level, outlier.level, shift.level, class.percentages){
  
#An initial check for errors is done
sinInitialCheck(num.series, length.series, noise.level, outlier.level, shift.level, class.percentages)
  
  
#Create the list where the synthetic series will be saved.
database<-list() 
#Create a vector where the cluster number of each series will be saved.
clases<-c() 

#The shift level is normalized based on the series length.
shift.level <- shift.level * length.series / 100

#The maximum value to which the series may be shifted is calculated.
max.shift <- round((3 * shift.level - 2 + sqrt(9 * shift.level ^ 2 + 4)) / 4)

if(length.series * 0.5 - max.shift < 1){
  stop('Shift level is to large for this type of database.')
}


#The number of series in each cluster is calculated.
class.frequencies <- round(num.series * class.percentages)
  


#The database is created:

#The series from the first cluster are generated.
if (class.frequencies[1] != 0){
    for(i in 1:class.frequencies[1]){
      #A specific shift value is sampled from the defined interval.
      shift <- sample(-max.shift:max.shift, 1)
      #The series is generated and saved into the list.
      database[[i]] <- s1(length.series, shift, noise.level)
      #The cluster number is saved.
      clases[i]<-1
    }
}
  
if(class.frequencies[2] != 0){
    for(i in 1:class.frequencies[2]){
      #A specific shift value is sampled from the defined interval.
      shift <- sample(-max.shift:max.shift, 1)
      #The series is generated and saved into the list.
      database[[class.frequencies[1]+i]] <- s2(length.series, shift, noise.level)
      #The cluster number is saved.
      clases[class.frequencies[1]+i] <- 2
    }
  }
  
if (class.frequencies[3] != 0){
    for (i in 1:class.frequencies[3]){
      #A specific shift value is sampled from the defined interval.
      shift <- sample(-max.shift:max.shift, 1)
      #The series is generated and saved into the list.
      database[[sum(class.frequencies[1:2]) + i]] <- s3(length.series, shift, noise.level)
      #The cluster number is saved.
      clases[sum(class.frequencies[1:2]) + i] <- 3
    }
  }
  
if(class.frequencies[4]!=0){
  for(i in 1:class.frequencies[4]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[sum(class.frequencies[1:3]) + i]] <- s4(length.series, shift, noise.level)
    #The cluster number is saved.
    clases[sum(class.frequencies[1:3]) + i] <- 4
  }
}
  
if(class.frequencies[5]!=0){        
  for(i in 1:class.frequencies[5]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[sum(class.frequencies[1:4]) + i]] <- s5(length.series, shift, noise.level)
      clases[sum(class.frequencies[1:4]) + i] <- 5
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

###############################################
#################INITIAL ERRORS################
###############################################

#This function checks for possible errors in the introduced parameters.

sinInitialCheck<-function(num.series, length.series, noise.level, outlier.level, shift.level, class.percentages){
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
}

###############################################
#################BASIC SHAPES##################
###############################################




s1<-function(length.series, shift, noise.level){
  
  base<-c(1:length.series)
  #The period of the sinusoidal form is defined.
  T<-length(base)*0.5
  #The series is generated.
  series <- sin (2 * pi * (base + shift) / T) 
  #The series is multiplied by a random number
  multi <- runif(1, 1, 1.1)
  series <- series * multi

  #The noise is added.
  if(noise.level!=0){
    #The random noise is created.
    noise<-rnorm(length.series, 0, noise.level/100)
    series<-series+noise
  }

return(series)
}


s2<-function(length.series, shift, noise.level){
  
  base<-c(1:length.series)
  #The period of the sinusoidal form is defined.
  T<-length(base)*0.5
  #The sinusoidal form is generated is generated.
  series <- sin (2 * pi * (base + shift) / T) 
  #A LOESS approximation is done.
  aprox <- loess(series~base, span=0.25,degree=1)
  #The series is multiplied by a random number
  multi<-runif(1,1,1.1)
  series<-predict(aprox)*multi
  #The noise is added.
  if(noise.level!=0){
  #The random noise is created.
  noise<-rnorm(length.series, 0, noise.level/100)
  series<-series+noise
  }

  return(series)
}


s3 <- function(length.series, shift, noise.level){
  
  base<-c(1:length.series)
  #The period of the sinusoidal form is defined.
  T<-length(base)*0.5
  #The series is generated.
  series <- sin (2 * pi * (base + shift) / T) 
  #The series is multiplied by a random number
  multi <- runif(1, 1, 1.1)
  series <- series * multi
  #The noise is added.
  if(noise.level!=0){
    #The random noise is created.
    noise<-rnorm(length.series, 0, noise.level/100)
    series<-series+noise
  }
  #The values that are above a threhold are truncated.
  threshold<-runif(1,0.90,0.99)
  series[series>threshold]<-threshold
  return(series)
}


s4<-function(length.series, shift, noise.level){
  
  base<-c(1:length.series)
  #The period of the sinusoidal form is defined.
  T<-length(base)*0.5
  #The series is generated.
  series <- sin (2 * pi * (base + shift) / T) 
  #The series is multiplied by a random number
  multi <- runif(1, 1, 1.1)
  series <- series * multi
  #The noise is added.
  if(noise.level!=0){
    #The random noise is created.
    noise<-rnorm(length.series, 0, noise.level/100)
    series<-series+noise
  }
  #The values that are below a threhold are truncated.
  threshold<-runif(1,-0.99,-0.90)
  series[series<threshold]<-threshold
  return(series)
}


s5 <- function(length.series, shift, noise.level){
 
  base<-c(1:length.series)
  #The period of the sinusoidal form is defined.
  T<-length(base)*0.5
  #The series is generated.
  series <- sin (2 * pi * (base + shift) / T) 
  #The noise is added.
  if(noise.level!=0){
    #The random noise is created.
    noise<-rnorm(length.series, 0, noise.level/100)
    series<-series+noise
  }
  #A random number is added to a small part of the series.
  cut.point1<-max((round(length.series * 0.4) + shift), 1)
  cut.point2<-min((round(length(base) * 0.5) + shift), length.series)
  summ <- runif(1,0.2,0.3)           
  series[cut.point1:cut.point2] <- series[cut.point1:cut.point2] + summ
  multi<-runif(1,1,1.1)
  series<-series*multi
  return(series)
}


