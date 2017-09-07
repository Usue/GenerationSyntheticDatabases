
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
#classpercentages: proportion of series in each clusters. (vector of length 3 that sums 1)



cbf<-function(num.series, length.series, noise.level, outlier.level, shift.level, warp.level, class.percentages){

#An initial check for errors is done
cbfInitialCheck(num.series, length.series, noise.level, outlier.level, shift.level, warp.level, class.percentages)

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

if(max.shift+max.warp+floor(length.series / 3)>length.series){
  stop('Shift and warp levels are to large for this type of synthetic database.')
}

#The number of series in each cluster is calculated.
class.frequencies <- round(num.series * class.percentages)

#The database is created:

#The series from the first cluster are generated.
if (class.frequencies[1] !=0){
  for (i in 1:class.frequencies[1]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #A specific warp value is sampled from the defined interval.
    warp <- sample(-max.warp:max.warp, 1)
    #The series is generated and saved into the list.
    database[[i]]<-cbf1(length.series, shift, warp)
    #The cluster number is saved.
    clases[i]<-1
  }
}

  if(class.frequencies[2] != 0){
    for (i in 1:class.frequencies[2]){
      #A specific shift value is sampled from the defined interval.
      shift <- sample(-max.shift:max.shift, 1)
      #A specific warp value is sampled from the defined interval.
      warp <- sample(-max.warp:max.warp, 1)
      #The series is generated and saved into the list.
      database[[class.frequencies[1]+i]]<-cbf2(length.series, shift, warp)
      #The cluster number is saved.
      clases[class.frequencies[1]+i]<-2
  }
}

if(class.frequencies[3]!=0){
  for(i in 1:class.frequencies[3]){
    #A specific shift value is sampled from the defined interval.
    shift <- sample(-max.shift:max.shift, 1)
    #A specific warp value is sampled from the defined interval.
    warp <- sample(-max.warp:max.warp, 1)
    #The series is generated and saved into the list.
    database[[sum(class.frequencies[1:2])+i]]<-cbf3(length.series, shift, warp)
    #The cluster number is saved.
    clases[sum(class.frequencies[1:2])+i]<-3
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

cbfInitialCheck<-function(num.series, length.series, noise.level, outlier.level, shift.level, warp.level, class.percentages){
  if (sum(class.percentages) != 1){
    stop('Class percentages must sum 1')
  }
  if (length(class.percentages) != 3){
    stop('You must give percentages for 3 classes')
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

#Cylinder shape

cbf1<-function(length.series, shift, warp){
  #The initial cut points are situated:
  cut.point1 <- floor(length.series / 3)
  cut.point2 <- floor(2 * length.series / 3)
  #The cut points are moved depending on the shift and warp:
  mod.cut.point1 <- max(cut.point1 + shift + warp, 1)
  mod.cut.point2 <- min(cut.point2 + shift, length.series)
  #The series is defined
  series <- fun(c(1:length.series), mod.cut.point1, mod.cut.point2)
  series <- (6 + rnorm(1,0,1))  * series
  return(series)
}

#Bell shape

cbf2<-function(length.series, shift, warp){
  #The initial cut points are situated:
  cut.point1 <- floor(length.series / 3)
  cut.point2 <- floor(2 * length.series / 3)
  #The cut points are moved depending on the shift and warp:
  mod.cut.point1 <- max(cut.point1 + shift + warp, 1)
  mod.cut.point2 <- min(cut.point2 + shift, length.series)
  #The series is defined
  series <- fun(c(1:length.series), mod.cut.point1, mod.cut.point2)
  series <- (6 + rnorm(1,0,1)) * series * (c(1:length.series) - mod.cut.point1) / (mod.cut.point2 - mod.cut.point1)
  return(series)
}

#Funnel shape

cbf3<-function(length.series, shift, warp){
  #The initial cut points are situated:
  cut.point1 <- floor(length.series / 3)
  cut.point2 <- floor(2 * length.series / 3)
  #The cut points are moved depending on the shift and warp:
  mod.cut.point1 <- max(cut.point1 + shift + warp, 1)
  mod.cut.point2 <- min(cut.point2 + shift, length.series)
  #The series is defined
  series <- fun(c(1:length.series), mod.cut.point1, mod.cut.point2)
  series <- (6 + rnorm(1,0,1)) * series * (mod.cut.point2 - c(1:length.series)) / (mod.cut.point2 - mod.cut.point1)
  return(series)
}

#This function defines the pieces of the function.
fun<-function(vector,a,b){
    if(a != 1){
    vector[1:(a-1)] <- 0
    }
    vector[a:b] <- 1
    if(b != length(vector)){
    vector[(b+1):length(vector)] <- 0 
    }
    return(vector)
}
