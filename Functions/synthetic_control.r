###############################################
#################SIMULATION FUNCTION###########
###############################################

#This function creates a synthetic database of type 
#following parameters: 
#num.series: # of series in the database (integer).
#length.series: length of series in the database (integer).
#noise.level: level of noise introduced in database (real).
#outlier.level: proportion of outliers introduced in database  (integer).
#shift.level: mean level of shift introduced in the database (integer).
#classpercentages: proportion of series in each clusters. (vector of length 6 that sums 1)

synthetic.control<-function(num.series, length.series, noise.level, outlier.level, shift.level, class.percentages){

scInitialCheck(num.series, length.series, noise.level, outlier.level, shift.level, class.percentages)
  
#Create the list where the synthetic series will be saved.
database<-list() 
#Create a vector where the cluster number of each series will be saved.
clases<-c() 

#The shift level is normalized based on the series length.
shift.level <- shift.level * length.series / 100

#The maximum value to which the series may be shifted is calculated.
max.shift <- round((3 * shift.level - 2 + sqrt(9 * shift.level ^ 2 + 4)) / 4)

if(max.shift+floor(length.series/2)>length.series){
  stop('Shift level is too large for this type of database.')
}

#The number of series in each cluster is calculated.
class.frequencies <- round(num.series * class.percentages)


#The database is created:

#The series from the first cluster are generated.
if (class.frequencies[1] != 0){
  for (i in 1:class.frequencies[1]){
    #The series is generated and saved into the list.
    database[[i]] <- sc1(length.series)
    #The cluster number is saved.
    clases[i] <- 1
  }
}

if (class.frequencies[2] != 0){
  for (i in 1:class.frequencies[2]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[class.frequencies[1]+i]] <- sc2(length.series, shift)
    #The cluster number is saved.
    clases[class.frequencies[1]+i] <- 2 
  }
}

if(class.frequencies[3] != 0){
  for (i in 1:class.frequencies[3]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[sum(class.frequencies[1:2])+i]] <- sc3(length.series, shift)
    #The cluster number is saved.
    clases[sum(class.frequencies[1:2])+i]<-3
  }
}

if (class.frequencies[4]!=0){
  for (i in 1:class.frequencies[4]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[sum(class.frequencies[1:3])+i]] <- sc4(length.series, shift)
    #The cluster number is saved.
    clases[sum(class.frequencies[1:3])+i] <- 4
  }
}

if (class.frequencies[5]!=0){        
  for (i in 1:class.frequencies[5]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[sum(class.frequencies[1:4])+i]] <- sc5(length.series, shift)
    #The cluster number is saved.
    clases[sum(class.frequencies[1:4])+i] <- 5
  }
}
 
if (class.frequencies[6]!=0){
  for (i in 1:class.frequencies[6]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #The series is generated and saved into the list.
    database[[sum(class.frequencies[1:5])+i]] <- sc6(length.series, shift)
    #The cluster number is saved.
    clases[sum(class.frequencies[1:5])+i] <- 6
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



###############################################
#################INITIAL ERRORS################
###############################################

#This function checks for possible errors in the introduced parameters.

scInitialCheck<-function(num.series, length.series, noise.level, outlier.level, shift.level, class.percentages){
  if (sum(class.percentages) != 1){
    stop('Class percentages must sum 1')
  }
  if (length(class.percentages) != 6){
    stop('You must give percentages for 6 classes')
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

sc1<-function(length.series){
  #The mean height of the series.
  mean <- 80 
  #A random series is generated.
  series <- mean + rnorm(length.series, 0, 3)
  return(series)
}

sc2<-function(length.series, shift){
  #The mean height of the series is defined.
  mean <- 80
  #The amplitude of the sinusoidal form is defined
  am <- 15
  #The period of the sinusoidal form is defined
  T <- length.series*0.3
  #The series is created
  series <- mean + am * sin(2 * pi * (c(1:length.series) + shift) / T)
  return(series)
}

sc3<-function(length.series, shift){
  #The mean height of the series is defined.
  mean <- 80
  #The slope of the linear series is defined.
  g<-0.4
  #The series is created
  series <- mean + g * c(1:length.series) + shift
  return(series)
}

sc4<-function(length.series, shift){
  #The mean height of the series is defined.
  mean <- 80
  #The slope of the linear series is defined.
  g<-0.4
  #The series is created
  series <- mean - g * c(1:length.series) + shift
  return(series)
}

sc5<-function(length.series, shift){
  #The mean height of the series is defined.
  mean <- 80
  #The initial cut point is defined
  cut.point<-floor(length.series/2) 
  #The cut point is modified
  mod.cut.point<-cut.point + shift
  #The magnitude of the jump is defined
  size.jump<-10 
  #The basic shape is created.
  shape<-fun(c(1:length.series), mod.cut.point)
  #The final series is created.
  series<-mean + shape * size.jump
  return(series)
}

sc6<-function(length.series, shift){
  #The mean height of the series is defined.
  mean <- 80
  #The initial cut point is defined
  cut.point<-floor(length.series/2) 
  #The cut point is modified
  mod.cut.point<-cut.point + shift
  #The magnitude of the jump is defined
  size.jump<-10 
  #The basic shape is created.
  shape<-fun(c(1:length.series), mod.cut.point)
  #The final series is created.
  series<-mean - shape * size.jump
  return(series)
}

fun<-function(vector, b){
    vector[1:(b-1)]<-0
    vector[b:length(vector)]<-1
    return(vector)
}

