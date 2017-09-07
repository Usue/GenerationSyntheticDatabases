
This directory contains additional information and data concerning the submitted publication titled: 

"Similarity Measure Selection for Clustering Time Series Databases"

In this document the content of each folder is explained in detail.

###################DATABASES####################

-In this folder the databases used in the study are available. We must note that each database is saved in an .RData file
 with a title that specifies the type of database and a number.

-These .RData files contain a unique R object, denominated database.

-The database object is a list with two positions.

-To access to one of these two positions we use the sintax in R: database[[1]] and database[[2]].

-The two positions save the following data: 

	*The first is another list, that saves the actual database. Each of the positions of the list is an array that describes one time series. 
	 (Example: to access the 17th series of the database we use database[[1]][[17]])

	*The second is a numerical array that saves the cluster that each series belongs to. 
	 (Example: to find out the cluster to which the 17th series belongs we run database[[2]][17])

###################PARAMETERS####################

-This file contains 5 files each corresponding to a database type.

-In each file we can find the parameters used to generate each time series database.

-The parameters are ordered in the following manner: 

	*nº of series
	*length of series
	*noise level
	*outliers level
	*shift level
	*warp level (only for database types kohler, twopatterns and CBF)
	*option nº for proportion of series in each cluster.


###################FUNCTIONS####################

-This file contains the R code to generate the synthetic databases. 

-There is a .R code for each of the types of database. This code can be used to generate a time series database of a certain type and some specific characteristics.

-There is a simulation.R file that codes the complete generation process of the synthetic databases.
