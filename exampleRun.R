#### Initialisation #### ------------------------------------------------------------------------------------------------------
library(purrr)
# Packages
rm(list=ls())

# Food (constant)
dataf = 1

# Temperature (levels)
dataT = matrix(seq(278.15,303.15,by=5), ncol = 1) 

# Parasite density                      ##adjust
dataP = matrix(seq(0,1,length=6), ncol = 1) 
alpha <- 0.1 # Maintenance increase (10% per unit parasite density)
beta <- 0.07# Assimilation reduction (7% per unit parasite density)


# Retrieve parameters
source("par_salmon.R")
source("parchem_salmon.R")

# Fix maximum length and time step
par$t_max = 2000 # in days
par$dt = 1 # time step (in days)

# Systems-specific adjustment
# if (type == "open"){            ##adjust
#   dataf = 0.8*dataT
#   par_open <- par
#   
# }


# Transform parameter data.frame to matrix
allPar <- list_rbind(rep(list(par), times = nrow(dataT)))
allPar$ind <- as.numeric(row.names(allPar))
allPar <- as.matrix(allPar)
dataf <- as.matrix(dataf)
dataT <- as.matrix(dataT)
dataP <- as.matrix(dataP)

# Run popDEB function
source("popDEB.R")

test <- popDEB(allPar, dataf = dataf, dataT = dataT, dataP = dataP)
View(test[[6]])

for (id in 1:6) {
  testT<-test[[id]]
  counter<-1
  strs<-c("0->1","1->2")
  
  for (i in 1:(nrow(testT) - 1)){
    if ((testT[i+1,]$stage - testT[i,]$stage)==1){
      print(paste(id,i,dataT[id,],strs[counter]))
      counter<-counter+1
    }
  }
}



