# Get outputs of DEB model for a given set of parameters and forcing variables 

indDEB <- function(times, funcT, funcf, funcX, funcP, stateInit, par_ind){
  # t: numeric vector of time steps (in days) at which states variables should be computed
  # funcT: scalar giving the constant value of ambient T or spline function giving T as a function of time
  # funcf: same for f
  # stateInit: vector of length 4 giving the initial values of E, V, E_H, E_R, q_ddot, h_dot, S
  # par_ind: list giving the parameters' values for the individual (see par_daphnia.R for an example)
  
  # Solve ODE system
  res <- ode(y = stateInit, times = times, funcP = funcP,
             func = DEBc_std, rootfun = rootfun,
             parms = par_ind, method="radau")
  out <- matrix(NA,nrow = nrow(res), ncol = 16)
  colnames(out) <- c("ind","time","E","V","E_H","E_R","q_ddot","h_dot","S",
                     "Lw","Ww","E_R_dt","E_R_rate","R_dt","R_rate","stage")
  out[,"ind"] = par_ind["ind"] # individual number
  out[1:nrow(res),1+1:ncol(res)] <- res
  out[,"Lw"] <- ((out[,"V"]^(1/3))/par_ind["del_M"]) # Physical length (cm)
  out[,"Ww"] <- (out[,"V"] + out[,"E"]*w_E/mu_E)*1 # Weight weight (g) excluding the reproduction buffer
  out[,"E_R_dt"] <- (out[,"E_R"] - c(0,out[-nrow(out),"E_R"])) # Reproduction per time step (J)
  out[,"E_R_rate"] <- out[,"E_R_dt"]/(out[,"time"] - c(0,out[-nrow(out),"time"])) # Reproduction rate (J/day)
  out[,"R_dt"] <- out[,"E_R_dt"]/par_ind["E_0"] # Number of eggs per time step (#)
  out[,"R_rate"] <- out[,"E_R_rate"]/par_ind["E_0"]  # Reproduction rate (#/day)
  out[,"stage"] <- c(0,1,2)[1 + (out[,"E_H"] > par_ind["E_Hb"]) + (out[,"E_H"] > par_ind["E_Hp"])] # embryo 0, juvenile 1, adult 2
  return(out)
}
