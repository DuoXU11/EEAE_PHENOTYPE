#### Initialisation #### ------------------------------------------------------------------------------------------------------

# Packages
rm(list=ls())

# Food (constant)
dataf = 1

# Temperature (constant)
dataT = 293.15 
## modify
#dataT=seq(5,30,5)

# Retrieve parameters
source("par_salmon.R")
source("parchem_salmon.R")

# Fix maximum length and time step
par$t_max = 400 # in days
par$dt = 1 # time step (in days)

# Transform parameter data.frame to matrix
allPar <- as.matrix(par)
dataf <- as.matrix(dataf)
dataT <- as.matrix(dataT)

# Run popDEB function
source("popDEB.R")
test <- popDEB(allPar, dataf = dataf, dataT = dataT)
View(test[[1]])