#### Initialisation #### ------------------------------------------------------------------------------------------------------

# Packages
rm(list=ls())

# Food (constant)
dataf = 1

# Temperature (constant)
dataT = matrix(seq(278.15,303.15,by=5), ncol = 1) 
## modify


# Retrieve parameters
source("par_salmon.R")
source("parchem_salmon.R")

# Fix maximum length and time step
par$t_max = 400 # in days
par$dt = 1 # time step (in days)

# Transform parameter data.frame to matrix
allPar <- list_rbind(rep(list(par), times = nrow(dataT)))
allPar$ind <- as.numeric(row.names(allPar))
allPar <- as.matrix(allPar)
dataf <- as.matrix(dataf)
dataT <- as.matrix(dataT)

# Run popDEB function
source("popDEB.R")
test <- popDEB(allPar, dataf = dataf, dataT = dataT)
View(test[[6]])


write.csv(test,"C:/Users/许多/Desktop/DEB.csv")

