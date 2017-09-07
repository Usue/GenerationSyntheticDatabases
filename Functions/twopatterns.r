
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
#class.percentages: proportion of series in each clusters. (vector of length 4 that sums 1)


twopatterns<-function(num.series, length.series, noise.level, outlier.level, shift.level, warp.level, class.percentages){

twopatInitialCheck(num.series, length.series, noise.level, outlier.level, shift.level, warp.level, class.percentages)
    
#An initial check for errors is done
twopatInitialCheck(num.series, length.series, noise.level, outlier.level, shift.level, warp.level, class.percentages)
  
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

if ((length.series * 0.1 - max.warp)<= 0){
  stop('Warp level is too high for this type of database')
}

if((floor(2 * length.series / 3) + max.shift + length.series * 0.1 + max.warp) > length.series){
  stop('Shift and warp levels are too high for this type of database.')
}
  
#The number of series in each cluster is calculated.
class.frequencies <- round(num.series * class.percentages)

if (class.frequencies[1] != 0){
  for (i in 1:class.frequencies[1]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #A specific warp value is sampled from the defined interval.
    warp <- sample(-max.warp:max.warp, 1)
    #The series is generated and saved into the list.
    database[[i]]<-tp1(length.series, shift, warp)
    #The cluster number is saved.
    clases[i]<-1
  }
}

if (class.frequencies[2] != 0){
  for (i in 1:class.frequencies[2]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #A specific warp value is sampled from the defined interval.
    warp <- sample(-max.warp:max.warp, 1)
    #The series is generated and saved into the list.
    database[[class.frequencies[1] + i]] <- tp2(length.series, shift, warp)
    #The cluster number is saved.
    clases[class.frequencies[1] + i] <- 2
  }
}

if(class.frequencies[3] != 0){
  for(i in 1:class.frequencies[3]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #A specific warp value is sampled from the defined interval.
    warp <- sample(-max.warp:max.warp, 1)
    #The series is generated and saved into the list.
    database[[sum(class.frequencies[1:2]) + i]] <- tp3(length.series, shift, warp)
    #The cluster number is saved.
    clases[sum(class.frequencies[1:2]) + i] <- 3
  }
}

if(class.frequencies[4] != 0){
for(i in 1:class.frequencies[4]){
  #A specific shift value is sampled from the defined interval.
  shift <- sample(-max.shift:max.shift, 1)
  #A specific warp value is sampled from the defined interval.
  warp <- sample(-max.warp:max.warp, 1)
  #The series is generated and saved into the list.
  database[[sum(class.frequencies[1:3]) + i]] <- tp4(length.series, shift, warp)
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

#The database and the cluster values are returned in a list.
return(list(database,clases))

}

###############################################
#################INITIAL ERRORS################
###############################################

#This function checks for possible errors in the introduced parameters.

twopatInitialCheck<-function(num.series, length.series, noise.level, outlier.level, shift.level, warp.level, class.percentages){
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
  if (warp.level < 0){
    stop('The warp level must be positive')
  }
}

###############################################
#################BASIC SHAPES##################
###############################################

#Upward shape
us<-function(t,l){
  
  vector<-c(1:length(t))*0
  vector[1:ceiling(l/2)]<--5
  vector[(ceiling(l/2)+1):length(t)]<-5
  return(vector)
}


#Downward shape
ds<-function(t,l){
  
  vector<-c(1:length(t))*0
  vector[1:ceiling(l/2)]<-5
  vector[(ceiling(l/2)+1):length(t)]<--5
  return(vector)
}



tp1<-function(length.series, shift, warp){
  #The cut points are defined
  t1 <- max(floor(length.series / 3) + shift, 1)
  t2 <- floor(2 * length.series / 3) + shift
  l1 <- length.series * 0.1 + warp
  l2 <- length.series * 0.1 + warp
  #The series is created.
  series <- c(1:length.series)
  series[1:t1] <- rnorm(t1, 0, 1)
  series[(t1 + 1):(t1 + l1)] <- us(c((t1 + 1):(t1 + l1)), l1)
  series[(t1 + l1 + 1):t2] <- rnorm((t2 - (t1 + l1)), 0, 1)
  series[(t2 + 1):(t2 + l2)]<-us(c((t2 + 1):(t2 + l2)), l2)
  series[(t2 + l2 + 1):length.series] <- rnorm((length.series - (t2 + l2)), 0, 1)
  return(series)
}


tp2<-function(length.series, shift, warp){
  #The cut points are defined
  t1 <- max(floor(length.series / 3) + shift, 1)
  t2 <- floor(2 * length.series / 3) + shift
  l1 <- length.series * 0.1 + warp
  l2 <- length.series * 0.1 + warp
  #The series is created.
  series <- c(1:length.series)
  series[1:t1] <- rnorm(t1, 0, 1)
  series[(t1 + 1):(t1 + l1)] <- us(c((t1 + 1):(t1 + l1)), l1)
  series[(t1 + l1 + 1):t2] <- rnorm((t2 - (t1 + l1)), 0, 1)
  series[(t2 + 1):(t2 + l2)]<-ds(c((t2 + 1):(t2 + l2)), l2)
  series[(t2 + l2 + 1):length.series] <- rnorm((length.series - (t2 + l2)), 0, 1)
  return(series)
}

tp3<-function(length.series, shift, warp){
  #The cut points are defined
  t1 <- max(floor(length.series / 3) + shift, 1)
  t2 <- floor(2 * length.series / 3) + shift
  l1 <- length.series * 0.1 + warp
  l2 <- length.series * 0.1 + warp
  #The series is created.
  series <- c(1:length.series)
  series[1:t1] <- rnorm(t1, 0, 1)
  series[(t1 + 1):(t1 + l1)] <- ds(c((t1 + 1):(t1 + l1)), l1)
  series[(t1 + l1 + 1):t2] <- rnorm((t2 - (t1 + l1)), 0, 1)
  series[(t2 + 1):(t2 + l2)]<-ds(c((t2 + 1):(t2 + l2)), l2)
  series[(t2 + l2 + 1):length.series] <- rnorm((length.series - (t2 + l2)), 0, 1)
  return(series)
}


tp4<-function(length.series, shift, warp){
  #The cut points are defined
  t1 <- max(floor(length.series / 3) + shift, 1)
  t2 <- floor(2 * length.series / 3) + shift
  l1 <- length.series * 0.1 + warp
  l2 <- length.series * 0.1 + warp
  #The series is created.
  series <- c(1:length.series)
  series[1:t1] <- rnorm(t1, 0, 1)
  series[(t1 + 1):(t1 + l1)] <- ds(c((t1 + 1):(t1 + l1)), l1)
  series[(t1 + l1 + 1):t2] <- rnorm((t2 - (t1 + l1)), 0, 1)
  series[(t2 + 1):(t2 + l2)]<-us(c((t2 + 1):(t2 + l2)), l2)
  series[(t2 + l2 + 1):length.series] <- rnorm((length.series - (t2 + l2)), 0, 1)
  return(series)
}