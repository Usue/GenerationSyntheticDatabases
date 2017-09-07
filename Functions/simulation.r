###############################################
#################SIMULATION FUNCTION###########
###############################################

#This source file summarizes the generation process 
#of the synthetic databases used in the submitted 
#paper titled: 

# "Similarity Measure Selection for Clustering Time
#Series Databases"


###############################################
#################DIMENSION OPTIONS#############
###############################################

#As a first step, we select some options for the
#database dimension.

x<-c(70,100,150,200,300,500,1000)
a<-matrix(nrow=7,ncol=7);
for(i in 1:7){
  a[i,]<-x[i]*x
}

dimensions<-which(a<=60000, arr.in=TRUE)

#Once this is done we start with the generation of the databases. Before 
#executing this part we should define the path where the functions to 
#generate the different synthetic databases are situated. For example:

path<-getwd()

#Also, we should define a working directory where the databases 
#will be saved, for example:

path2<-paste(getwd(),"/databases",sep="")

###############################################
#################ARMA DATABASE#################
###############################################

#The allowed categories are defined.
noise<-c(1,2,3,4)
outliers<-c(1,2,3,4)
shift<-c(1,2,3,4)

#The code for generating an arma database is loaded.
source(paste(path,"/arma.r",sep=""))

#The database number is initialized
numbd<-1

for(i in c(1:length(noise))){
  
  for(j in c(1:length(outliers))){
    
      for(k in c(1:length(shift))){
        
        #The dimension is sampled randomnly from the set of options.
        a<-sample(c(1:dim(dimensions)[1]),1)
        numseries<-x[dimensions[a,1]]
        lengthseries<-x[dimensions[a,2]]
        
        #We discard the databases with all parameters=0.
        if(i!=1 || j!=1 || k!=1){
        
        #We define the noise level.
        if(i==1){noiselevel<-0}
        if(i==2){noiselevel<-sample(c(1,2),1)}
        if(i==3){noiselevel<-sample(c(3,4),1)}
        if(i==4){noiselevel<-sample(c(5,6),1)}
        
        
        #We define the outlier level.
        if(j==1){outlierlevel<-0}
        if(j==2){outlierlevel<-sample(c(1,2),1)}
        if(j==3){outlierlevel<-sample(c(3,4),1)}
        if(j==4){outlierlevel<-sample(c(5,6),1)}
        
        #We define the shift level.
        if(k==1){shiftlevel<-0}
        if(k==2){shiftlevel<-sample(c(1:5),1)}
        if(k==3){shiftlevel<-sample(c(6:12),1)}
        if(k==4){shiftlevel<-sample(c(13:30),1)}
        
        #We select the proportion of series in each cluster
        #randomly from the set of options.
        classfreq<-sample(c(1:4),1)
        if(classfreq==1){classfrequencies<-c(1/8,1/8,1/8,1/8,1/8,1/8,1/8,1/8)}
        if(classfreq==2){classfrequencies<-c(1/4,1/8,1/8,1/16,3/16,1/8,1/8,0)}
        if(classfreq==3){classfrequencies<-c(1/5,1/5,0,0,1/5,0,1/5,1/5)}
        if(classfreq==4){classfrequencies<-c(1/4,0,1/3,1/8,1/8,0,1/6,0)}
        
        #We generate the database and save it in a subdirectory /database
        #inside the working directory.
        database<-armas(numseries,lengthseries,noiselevel,outlierlevel,shiftlevel,classfrequencies)
        save(database,file=paste(path2,"/arima-",numbd,".RData",sep=""))
        numbd<-numbd+1
        }     
      } 
  }
}


###############################################
#################CBF DATABASE#################
###############################################

#The allowed categories are defined.
noise<-c(1,2,3)
outliers<-c(1,2,3)
shift<-c(1,2,3)
warp<-c(1,2,3)

#The code for generating an cbf database is loaded.
source(paste(path,"/cbf.r",sep=""))

#The database number is initialized
numbd<-1

for(i in c(1:length(noise))){
  
  for(j in c(1:length(outliers))){
    
    for(k in c(1:length(shift))){
      
      for(t in c(1:length(warp))){
        
      #The dimension is sampled randomnly from the set of options.
      a<-sample(c(1:dim(dimensions)[1]),1)
      numseries<-x[dimensions[a,1]]
      lengthseries<-x[dimensions[a,2]]
      
      #We discard the databases with all parameters=0.
      if(i!=1 || j!=1 || k!=1 || t!=1){
        
        #We define the noise level.
        if(i==1){noiselevel<-0}
        if(i==2){noiselevel<-sample(c(1:3),1)}
        if(i==3){noiselevel<-sample(c(3:5),1)}
        
        #We define the outlier level.
        if(j==1){outlierlevel<-0}
        if(j==2){outlierlevel<-sample(c(1:3),1)}
        if(j==3){outlierlevel<-sample(c(3:5),1)}
        
        #We define the shift level.
        if(k==1){shiftlevel<-0}
        if(k==2){shiftlevel<-sample(c(1:5),1)}
        if(k==3){shiftlevel<-sample(c(6:12),1)}
        
        #We define the warp level.
        if(t==1){warplevel<-0}
        if(t==2){warplevel<-sample(c(1:5),1)}
        if(t==3){warplevel<-sample(c(5:10),1)}
        
        #We select the proportion of series in each cluster
        #randomly from the set of options.
        classfreq<-sample(c(1:3),1)
        if(classfreq==1){classfrequencies<-c(1/3,1/3,1/3)}
        if(classfreq==2){classfrequencies<-c(1/6,1/3,1/2)}
        if(classfreq==3){classfrequencies<-c(2/3,1/6,1/6)}
        
        database<-cbf(numseries,lengthseries,noiselevel,outlierlevel,shiftlevel,warplevel,classfrequencies)
        save(database,file=paste(path2,"/cbf-",numbd,".RData",sep=""))
        numbd<-numbd+1
      }
      }
    } 
  }
}


###############################################
################TWO PATTERNS DATABASE##########
###############################################

#The allowed categories are defined.
noise<-c(1,2,3)
outliers<-c(1,2,3)
shift<-c(1,2,3)
warp<-c(1,2,3)

#The code for generating an two patterns database is loaded.
source(paste(path,"/twopatterns.r",sep=""))

#The database number is initialized
numbd<-1

for(i in c(1:length(noise))){
  
  for(j in c(1:length(outliers))){
    
    for(k in c(1:length(shift))){
      
      for(t in c(1:length(warp))){
        
        
        #The dimension is sampled randomnly from the set of options.
        a<-sample(c(1:dim(dimensions)[1]),1)
        numseries<-x[dimensions[a,1]]
        lengthseries<-x[dimensions[a,2]]
        
        #We discard the databases with all parameters=0.
        if(i!=1 || j!=1 || k!=1 || t!=1){
          
          #We define the noise level.
          if(i==1){noiselevel<-0}
          if(i==2){noiselevel<-sample(c(1:3),1)}
          if(i==3){noiselevel<-sample(c(3:5),1)}
          
          #We define the outlier level.
          if(j==1){outlierlevel<-0}
          if(j==2){outlierlevel<-sample(c(1:3),1)}
          if(j==3){outlierlevel<-sample(c(3:5),1)}
          
          #We define the shift level.
          if(k==1){shiftlevel<-0}
          if(k==2){shiftlevel<-sample(c(1:5),1)}
          if(k==3){shiftlevel<-sample(c(6:10),1)}
          
          #We define the warp level.
          if(t==1){warplevel<-0}
          if(t==2){warplevel<-sample(c(1:5),1)}
          if(t==3){warplevel<-sample(c(6:8),1)}
          
          #We select the proportion of series in each cluster
          #randomly from the set of options.
          classfreq<-sample(c(1:3),1)
          if(classfreq==1){classfrequencies<-c(0.25,0.25,0.25,0.25)}
          if(classfreq==2){classfrequencies<-c(1/3,1/3,1/6,1/6)}
          if(classfreq==3){classfrequencies<-c(1/8,1/4,1/2,1/8)}
          
          
          #We generate the database and save it in a subdirectory /database
          #inside the working directory.
          database<-twopatterns(numseries,lengthseries,noiselevel,outlierlevel,shiftlevel,warplevel,classfrequencies)
          save(database,file=paste(path2,"/twopatterns-",numbd,".RData",sep=""))
          numbd<-numbd+1
        }
      }
    } 
  }
}

#####################################################################################################################################
###############################################################SYNTHETIC.CONTROL#################################################################
#####################################################################################################################################

#The allowed categories are defined.
noise<-c(1,2,3,4)
outliers<-c(1,2,3,4)
shift<-c(1,2,3,4)

#The code for generating a "synthetic database" database is loaded.
source(paste(path,"/synthetic_control.r",sep=""))

#The database number is initialized
numbd<-1

for(i in c(1:length(noise))){
  
  for(j in c(1:length(outliers))){
    
    for(k in c(1:length(shift))){
      
      #The dimension is sampled randomnly from the set of options.
      a<-sample(c(1:dim(dimensions)[1]),1)
      numseries<-x[dimensions[a,1]]
      lengthseries<-x[dimensions[a,2]]
      
      #We discard the databases with all parameters=0.
      if(i!=1 || j!=1 || k!=1){
        
        #We define the noise level.
        if(i==1){noiselevel<-0}
        if(i==2){noiselevel<-sample(c(1,2),1)}
        if(i==3){noiselevel<-sample(c(3,4),1)}
        if(i==4){noiselevel<-sample(c(5,6),1)}
        
        #We define the outlier level.
        if(j==1){outlierlevel<-0}
        if(j==2){outlierlevel<-sample(c(1,2),1)}
        if(j==3){outlierlevel<-sample(c(3,4),1)}
        if(j==4){outlierlevel<-sample(c(5,6),1)}
        
        #We define the shift level.
        if(k==1){shiftlevel<-0}
        if(k==2){shiftlevel<-sample(c(1:5),1)}
        if(k==3){shiftlevel<-sample(c(6:12),1)}
        if(k==4){shiftlevel<-sample(c(13:30),1)}
        
        #We select the proportion of series in each cluster
        #randomly from the set of options.
        classfreq<-sample(c(1:3),1)
        if(classfreq==1){classfrequencies<-c(1/6,1/6,1/6,1/6,1/6,1/6)}
        if(classfreq==2){classfrequencies<-c(1/6,1/12,1/12,1/6,1/4,1/4)}
        if(classfreq==3){classfrequencies<-c(1/3,1/3,1/12,1/12,1/12,1/12)}
        
        #Creamos la base de datos
        database<-synthetic.control(numseries,lengthseries,noiselevel,outlierlevel,shiftlevel,classfrequencies)
        save(database,file=paste(path2,"/synthetic.control-",numbd,".RData",sep=""))
        numbd<-numbd+1
      }
    } 
  }
}


#####################################################################################################################################
###############################################################KOHLER#################################################################
#####################################################################################################################################

#The allowed categories are defined.
noise<-c(1,2,3)
outliers<-c(1,2,3)
shift<-c(1,2,3)
warp<-c(1,2,3)

#The code for generating an arma database is loaded.
source(paste(path,"/kohlerlorenz.r",sep=""))

#The database number is initialized
numbd<-1

for(i in c(1:length(noise))){
  
  for(j in c(1:length(outliers))){
    
    for(k in c(1:length(shift))){
      
      for(t in c(1:length(warp))){
        
        
        #The dimension is sampled randomnly from the set of options.
        a<-sample(c(1:dim(dimensions)[1]),1)
        numseries<-x[dimensions[a,1]]
        lengthseries<-x[dimensions[a,2]]
        
        #We discard the databases with all parameters=0.
        if(i!=1 || j!=1 || k!=1 || t!=1){
          
          #We define the noise level.
          if(i==1){noiselevel<-0}
          if(i==2){noiselevel<-sample(c(1:3),1)}
          if(i==3){noiselevel<-sample(c(3:5),1)}
          
          #We define the outlier level.
          if(j==1){outlierlevel<-0}
          if(j==2){outlierlevel<-sample(c(1:3),1)}
          if(j==3){outlierlevel<-sample(c(3:5),1)}
          
          #We define the shift level.
          if(k==1){shiftlevel<-0}
          if(k==2){shiftlevel<-sample(c(1:5),1)}
          if(k==3){shiftlevel<-sample(c(6:10),1)}
          
          #We define the warp level.
          if(t==1){warplevel<-0}
          if(t==2){warplevel<-sample(c(1:5),1)}
          if(t==3){warplevel<-sample(c(6:8),1)}
          
          #We select the proportion of series in each cluster
          #randomly from the set of options.
          classfreq<-sample(c(1:4),1)
          if(classfreq==1){classfrequencies<-c(1/5,1/5,1/5,1/5,1/5)}
          if(classfreq==2){classfrequencies<-c(1/10,1/5,1/10,2/5,1/5)}
          if(classfreq==3){classfrequencies<-c(1/15,2/5,1/15,2/5,1/15)}
          if(classfreq==4){classfrequencies<-c(2/5,1/10,1/5,1/10,1/5)}
          
          
          #We generate the database and save it in a subdirectory /database
          #inside the working directory.
          database<-kohlerlorenz(numseries,lengthseries,noiselevel,outlierlevel,shiftlevel,warplevel,classfrequencies)
          save(database,file=paste(path2,"/kohler-",numbd,".RData",sep=""))
          numbd<-numbd+1
        }
      }
    } 
  }
}

###############################################
#################SINES DATABASE#################
###############################################

#The allowed categories are defined.
noise<-c(1,2,3,4)
outliers<-c(1,2,3,4)
shift<-c(1,2,3,4)

#The code for generating an sines database is loaded.
source(paste(path,"/sin.r",sep=""))

#The database number is initialized
numbd<-1

for(i in c(1:length(noise))){
  
  for(j in c(1:length(outliers))){
    
    for(k in c(1:length(shift))){
      
      #The dimension is sampled randomnly from the set of options.
      random<-sample(c(1:dim(dimensions)[1]),1)
      numseries<-x[dimensions[random,1]]
      lengthseries<-x[dimensions[random,2]]
      
      #We discard the databases with all parameters=0.
      if(i!=1 || j!=1 || k!=1){
        
        #We define the noise level.
        if(i==1){noiselevel<-0}
        if(i==2){noiselevel<-sample(c(1,2),1)}
        if(i==3){noiselevel<-sample(c(3,4),1)}
        if(i==4){noiselevel<-sample(c(5,6),1)}
        
        
        #We define the outlier level.
        if(j==1){outlierlevel<-0}
        if(j==2){outlierlevel<-sample(c(1,2),1)}
        if(j==3){outlierlevel<-sample(c(3,4),1)}
        if(j==4){outlierlevel<-sample(c(5,6),1)}
        
        
        #We define the shift level.
        if(k==1){shiftlevel<-0}
        if(k==2){shiftlevel<-sample(c(1:5),1)}
        if(k==3){shiftlevel<-sample(c(6:12),1)}
        if(k==4){shiftlevel<-sample(c(13:18),1)}
        
        #We select the proportion of series in each cluster
        #randomly from the set of options.
        classfreq<-sample(c(1:4),1)
        if(classfreq==1){classfrequencies<-c(1/5,1/5,1/5,1/5,1/5)}
        if(classfreq==2){classfrequencies<-c(1/10,1/5,1/10,2/5,1/5)}
        if(classfreq==3){classfrequencies<-c(1/15,2/5,1/15,2/5,1/15)}
        if(classfreq==4){classfrequencies<-c(2/5,1/10,1/5,1/10,1/5)}
        
        
        database<-sinseries(numseries,lengthseries,noiselevel,outlierlevel,shiftlevel,classfrequencies)
        save(database,file=paste(path2,"/sin-",numbd,".RData",sep=""))
        numbd<-numbd+1
      }
      
    } 
  }
}


###############################################
#################RATIONAL DATABASE#############
###############################################

#The allowed categories are defined.
noise<-c(1,2,3,4)
outliers<-c(1,2,3,4)
shift<-c(1,2,3,4)

source(paste(path,"/rational.r",sep=""))

#The database number is initialized
numbd<-1

for(i in c(1:length(noise))){
  
  for(j in c(1:length(outliers))){
    
    for(k in c(1:length(shift))){
      
      #The dimension is sampled randomnly from the set of options.
      random<-sample(c(1:dim(dimensions)[1]),1)
      numseries<-x[dimensions[random,1]]
      lengthseries<-x[dimensions[random,2]]
      
      #We discard the databases with all parameters=0.
      if(i!=1 || j!=1 || k!=1){
        
        #We define the noise level.
        if(i==1){noiselevel<-0}
        if(i==2){noiselevel<-sample(c(1,2),1)}
        if(i==3){noiselevel<-sample(c(3,4),1)}
        if(i==4){noiselevel<-sample(c(5,6),1)}
        
        
        #We define the outlier level.
        if(j==1){outlierlevel<-0}
        if(j==2){outlierlevel<-sample(c(1,2),1)}
        if(j==3){outlierlevel<-sample(c(3,4),1)}
        if(j==4){outlierlevel<-sample(c(5,6),1)}
        
        
        #We define the shift level.
        if(k==1){shiftlevel<-0}
        if(k==2){shiftlevel<-sample(c(1:5),1)}
        if(k==3){shiftlevel<-sample(c(6:12),1)}
        if(k==4){shiftlevel<-sample(c(13:18),1)}
        
        #We select the proportion of series in each cluster
        #randomly from the set of options.
        classfreq<-sample(c(1:3),1)
        if(classfreq==1){classfrequencies<-c(0.25,0.25,0.25,0.25)}
        if(classfreq==2){classfrequencies<-c(1/3,1/3,1/6,1/6)}
        if(classfreq==3){classfrequencies<-c(1/8,1/4,1/2,1/8)}
        
        
        database<-rational(numseries,lengthseries,noiselevel,outlierlevel,shiftlevel,classfrequencies)
        save(database,file=paste(path2,"/rational-",numbd,".RData",sep=""))
        numbd<-numbd+1
      }
      
    } 
  }
}

###############################################
#################SEASONAL DATABASE#################
###############################################


#The allowed categories are defined.
noise<-c(1,2,3,4)
outliers<-c(1,2,3,4)
shift<-c(1,2,3,4)

source(paste(path,"/seasonal.r",sep=""))

#The database number is initialized
numbd<-1

for(i in c(1:length(noise))){
  
  for(j in c(1:length(outliers))){
    
    for(k in c(1:length(shift))){
      
      #The dimension is sampled randomnly from the set of options.
      random<-sample(c(1:dim(dimensions)[1]),1)
      numseries<-x[dimensions[random,1]]
      lengthseries<-x[dimensions[random,2]]
      
      #We discard the databases with all parameters=0.
      if(i!=1 || j!=1 || k!=1){
        
        #We define the noise level.
        if(i==1){noiselevel<-0}
        if(i==2){noiselevel<-sample(c(1,2),1)}
        if(i==3){noiselevel<-sample(c(3,4),1)}
        if(i==4){noiselevel<-sample(c(5,6),1)}
        
        
        #We define the outlier level.
        if(j==1){outlierlevel<-0}
        if(j==2){outlierlevel<-sample(c(1,2),1)}
        if(j==3){outlierlevel<-sample(c(3,4),1)}
        if(j==4){outlierlevel<-sample(c(5,6),1)}
        
        
        #We define the shift level.
        if(k==1){shiftlevel<-0}
        if(k==2){shiftlevel<-sample(c(1:5),1)}
        if(k==3){shiftlevel<-sample(c(6:12),1)}
        if(k==4){shiftlevel<-sample(c(13:18),1)}
        
        #We select the proportion of series in each cluster
        #randomly from the set of options.
        classfreq<-sample(c(1:3),1)
        if(classfreq==1){classfrequencies<-c(0.25,0.25,0.25,0.25)}
        if(classfreq==2){classfrequencies<-c(1/3,1/3,1/6,1/6)}
        if(classfreq==3){classfrequencies<-c(1/8,1/4,1/2,1/8)}
        
        
        database<-seasonal(numseries,lengthseries,noiselevel,outlierlevel,shiftlevel,classfrequencies)
        save(database,file=paste(path2,"/seasonal-",numbd,".RData",sep=""))
        numbd<-numbd+1
      }
      
    } 
  }
}
