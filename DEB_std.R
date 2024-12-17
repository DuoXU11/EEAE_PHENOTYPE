

#########################################
#####                               #####
#####          std DEB model        #####
#####                               #####
#########################################


# Arguments :
# - t : vector of time points
# - y : Initial values of state variables
# - data : primary parameters


DEBc_std <- function(t, y, data, funcP = NULL){
  with(as.list(c(y,data)), {
    
    #### Temperature Correction #### --------------------------------------------------------------------------------
    
#                           no data available to use
#     s_T <- function(T) {
#       
#       T_AL <- 300   
#       T_AH <- 315   
#       T_L  <- 273   
#       T_H  <- 303  
#       
#       term1 <- exp((T_AL / T) - (T_AL / T_L))
#       term2 <- exp((T_AH / T_H) - (T_AH / T))
#       return(1 / (1 + term1 + term2))
#     }
#     
    
    if(is.numeric(funcT)){
      T_amb = funcT
    }else{T_amb = funcT(t)}
    T_1 = T_ref #reference temperature in K
    T_ = T_amb # ambient temperature in K
     anti_dot <- exp(T_A/T_1 - T_A/T_) #temperature correction factor for rates (time-dependent parameters)
    # anti_dot = exp(T_A/T_1) / exp(T_A/T_)
    # anti_dot = s(T_) / s(T_1)
    # 
    
    ############ Parasite density per cm²
    if (is.numeric(funcP)) {
      P_density <- funcP
    } else {
      P_density <- funcP(t)
    }
    
    
    #### Seuils cycles de vie #### -----------------------------------------------------------------------------------
    
    # E_Hb = E_Hb # maturit? ? la naissance - acceleration starts
    # E_Hj = E_Hj # Maturity at metamorphosis - acceleration ceases
    # E_Hp = E_Hp # maturit? ? la pubert?
     
    
    #### Metamorphosis #### ------------------------------------------------------------------------------------------
    
    # Lw_b <- Lw_b
    sM = s_M # s_M = l_j/ l_b; when j stage is reached
    
    L = V^(1/3)
    Lb = Lw_b*del_M
    ######################
    P_eff <- P_density * L^2  # Effective parasite density based on surface area
    
    s_M = 1 # s_M acceleration before birth
    if (E_H >= E_Hb & E_H < E_Hj) { # between birth and juv
      s_M = (L/Lb)
    }
    if (E_H >= E_Hj) { # after metamorphosis
      s_M = sM
    }
    
    #### DEB Primary Parameters #### ---------------------------------------------------------------------------------
    
    p_Am_dot = p_Am * anti_dot * s_M # flux d'assimilation specifique max corrig? pour la temp?rature
    
    p_M_dot = p_M * (1 + alpha * P_eff) * anti_dot # couts maintenance somatique specifique au volume
    
    p_T_dot = p_M * anti_dot # couts maintenance somatique surface-specifique
    E_G = E_G # couts structure volume-specifique
    v_dot = v * anti_dot * s_M # conductance energetique
    kap = kap # coef d'allocation
    k_J_dot = k_J * anti_dot # cout de maintenance de maturit?
    kap_R = kap_R # fraction d'energie allou?e au soma
    E_0 <- E_0 # initial energy of one egg
    s_G <- s_G # Gomperz stress coefficient
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
    
    ############## Adjust food availability (f) based on parasite load
    f_adjusted <- f * (1 - beta * P_eff)
    f_adjusted <- max(f_adjusted, 0)  # Ensure f remains non-negative
    
    #### DEB Coumpound Parameters #### -------------------------------------------------------------------------------
    
    E_m <- p_Am_dot/v_dot # maximum energy density
    g <- E_G / (kap*E_m) # energy investment ratio [E_G]/kappa*[Em]
    L_m = (kap * p_Am_dot)/p_M_dot # max volumetric structural length L_m p.51 DEB book
    e <- 1 + (( ((E/V) / E_m) * (E_H > E_Hb) ) -1) * (E_H > E_Hb) # specific reserve density
    r_dot =  v_dot * ( ((e/V^(1/3)) - 1/L_m) / (e + g) ) # growth rate p.51 DEB Book
    
    # Energy fluxes ----
    
    p_A_dot = (p_Am_dot * f_adjusted * V^(2/3)) # Assimilation
    
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

rootfun <- function (t, y, data, funcP) {
  return(y[7] - 0.001)
}
