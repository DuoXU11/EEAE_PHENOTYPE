## Generate the full dataset
# - allPar : matrix of model parameters (1 row per set of parameters = individual)
# - dataf : matrix of food levels (1 row per individual and 1 column per time step)
# - timef : vector of time steps at which food level was "measured"
# - dataX : matrix of food densities (1 row per individual and 1 column per time step)
# - timeX : vector of time steps at which food density was "measured"
# - dataT : matrix of temperatures (1 row per individual and 1 column per time step)
# - timeT : vector of time steps at which temperature was "measured"

popDEB <- function(allPar, dataf = NULL, timef = NULL, dataX = NULL, timeX = NULL, dataT, timeT = NULL){
  
  library(deSolve)
  
  source("indDEB.R", local = TRUE)
  source("DEB_std.R", local = TRUE)
  
  if(is.null(dataf) & is.null(dataX)){
    print("Information on food is missing.")
  }
  
  if (nrow(dataf) == 1 & ncol(dataf) > 1){
    funcf <- splinefun(timef, dataf, method='natural',ties = mean)
  }
  if(is.null(dataf)){
    if (nrow(dataX) == 1 & ncol(dataX) > 1){
      funcX <- splinefun(timeX, dataX, method='natural',ties = mean)
    }
  }
  if (nrow(dataT) == 1 & ncol(dataT) > 1){
    funcT <- splinefun(timeT, dataT, method='natural',ties = mean)
  }
  
  out <- vector('list', nrow(allPar))
  
  for (i in 1:nrow(allPar)){
    
    par_ind <- allPar[i,]
    
    # Initial state = state at fertilization
    stateInit <- c(E = as.numeric(par_ind["E_0"]), 
                   V = as.numeric(par_ind["L_0"]), 
                   E_H = 0, 
                   E_R = 0,
                   q_ddot = 0, 
                   h_dot = 0, 
                   S = 1)
    
    # Vector of time points
    times = seq(from = 0, to = par_ind["t_max"], by = par_ind["dt"])
    
    if(nrow(dataf)>1 & ncol(dataf)>1){
      funcf <- splinefun(timef, dataf[i,], method='natural',ties = mean)
    }
    if(ncol(dataf)==1){
      if(nrow(dataf)>1){funcf <- dataf[i,]
      }else{funcf = dataf}
    }
    
    if(is.null(dataf)){
      if(nrow(dataX)>1 & ncol(dataX)>1){
        funcX <- splinefun(timeX, dataX[i,], method='natural',ties = mean)
      }
      if(ncol(dataX)==1){
        if(nrow(dataX)>1){funcX <- dataX[i,]
        }else{funcX = dataX}
      }
    }
    
    if(nrow(dataT)>1 & ncol(dataT)>1){
      funcT <- splinefun(timeT, dataT[i,], method='natural',ties = mean)
    }
    if(ncol(dataT)==1){
      if(nrow(dataT)>1){funcT <- dataT[i,]
      }else{funcT = dataT}
    }
    
    out[[i]] <- as.data.frame(indDEB(times, funcT, funcf, funcX, stateInit, par_ind))
    eval(parse(text=paste("out[[i]]$f <-", c("rep(funcf, times = nrow(out[[i]]))","funcf(out[[i]]$time)")[is.function(funcf)+1])))
    eval(parse(text=paste("out[[i]]$temp <-", c("rep(funcT, times = nrow(out[[i]]))","funcT(out[[i]]$time)")[is.function(funcT)+1])))
    
  }
  
  return(out)
  
}
