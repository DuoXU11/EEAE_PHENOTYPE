

#########################################
#####                               #####
#####          std DEB model        #####
#####                               #####
#########################################


# Arguments :
# - t : vector of time points
# - y : Initial values of state variables
# - data : primary parameters


DEBc_std <- function(t, y, data){
  with(as.list(c(y,data)), {
    
    #### Temperature Correction #### --------------------------------------------------------------------------------
    
    if(is.numeric(funcT)){
      T_amb = funcT
    }else{T_amb = funcT(t)}
    T_1 = T_ref #reference temperature in K
    T_ = T_amb # ambient temperature in K
    anti_dot <- exp(T_A/T_1 - T_A/T_) #temperature correction factor for rates (time-dependent parameters)
    
    #### DEB Primary Parameters #### ---------------------------------------------------------------------------------
    
    p_Am_dot = p_Am * anti_dot # maximum assimilation rate
    p_M_dot = p_M * anti_dot # volume-specific somatic maintenance costs
    p_T_dot = p_T * anti_dot # surface-area-specific somatic maintenance costs
    v_dot = v * anti_dot # energy conductance
    k_J_dot = k_J * anti_dot # maturity maintenance costs
    h_a_ddot <- h_a * anti_dot^2 # Weibull acceleration
    
    #### Food #### -------------------------------------------------------------------------------------------
    
    if(is.null(funcf)){
      if(is.numeric(funcX)){
        # Conversion of X into f
        K = ((p_Am_dot/kappa_X)/F_m_dot)
        f = funcX/(K + funcX)
      }
    }
    
    if(is.numeric(funcf)){
      f = funcf
    }else{f = funcf(t)}
    
    #### DEB Coumpound Parameters #### -------------------------------------------------------------------------------
    
    E_m <- p_Am_dot/v_dot # maximum energy density
    g <- E_G / (kap*E_m) # energy investment ratio [E_G]/kappa*[Em]
    L_m = (kap * p_Am_dot)/p_M_dot # max volumetric structural length L_m p.51 DEB book
    e <- 1 + (( ((E/V) / E_m) * (E_H > E_Hb) ) -1) * (E_H > E_Hb) # specific reserve density
    r_dot =  v_dot * ( ((e/V^(1/3)) - 1/L_m) / (e + g) ) # growth rate p.51 DEB Book
    
    # Energy fluxes ----
    
    p_A_dot = (p_Am_dot * f * V^(2/3)) # Assimilation
    p_S_dot = p_M_dot * V + p_T_dot * V^(2/3) # Maintenance somatique
    p_C_dot = (E/V) * (E_G * v_dot * V^(2/3) + p_S_dot) / (kap * E/V + E_G) # Mobilisation surface-specifique
    p_G_dot = (kap * p_C_dot - p_S_dot) # Growth
    p_J_dot = (k_J_dot * E_H) # Maturity maintenance
    p_R_dot = ((1-kap) * p_C_dot - p_J_dot) # Maturation (before puberty) or reproduction (after puberty)
    p_D_dot = p_S_dot + p_J_dot + (1 - kap_R) * p_R_dot # dissipation lié à la maintenance et à la repro : adult
    
    
    # Dynamics of state variables ----
    
    # Reserve: dE = dt * (- pC) before birth, dt * (pA - pC) after birth
    dE <- (E_H > E_Hb) * p_A_dot - p_C_dot
    
    # Volume: growth flux corrected for the volume-specific costs of structure
    dV <- p_G_dot/E_G
    
    # Maturity (before puberty) 
    dE_H <- p_R_dot * (E_H < E_Hp)
    
    # Reproduction buffer
    
    dE_R <- kap_R * p_R_dot * (E_H >= E_Hp) # reproduction buffer is never emptied
    
    # Change in acceleration q^..
    dq_ddot <- (q_ddot * (V/L_m^3) * s_G + h_a_ddot) * e * ( (v_dot/V^(1/3)) - r_dot) - (r_dot * q_ddot)
    
    # Hazard rate (Mortality rate)
    dh_dot <-  q_ddot - (r_dot*h_dot) 
    
    # Probability of survival at age
    dS = -S * h_dot
    
    # List of state variables ----
    mod <- list(c(dE, dV, dE_H, dE_R, dq_ddot, dh_dot, dS))
    return(mod)
  })
  
}

#### END  #### -----------------------------------------------------------------------------------------------------------------------------------

### Root function -------------
### Integration stops when S < 0.001

rootfun <- function (t, y, data) {
  return(y[7] - 0.001)
}
