## Chemical parameters

# chemical indices for water-free organics
n_CX = 1
n_HX = 1.8
n_OX = 0.5
n_NX = 0.15
n_CV = 1
n_HV = 1.8
n_OV = 0.5
n_NV = 0.15
n_CE = 1
n_HE = 1.8
n_OE = 0.5
n_NE = 0.15
n_CP = 1
n_HP = 1.8
n_OP = 0.5
n_NP = 0.15
n_CN = 0 # = 0 or 1 by definition (0 for Daphnia _ AmP)
n_HN = 3
n_ON = 0
n_NN = 1


n_O <- matrix(data=c(n_CX, n_HX, n_OX, n_NX, 
                     n_CV, n_HV, n_OV, n_NV, 
                     n_CE, n_HE, n_OE, n_NE, 
                     n_CP, n_HP, n_OP, n_NP), 
              nrow=4, ncol=4)
n_M <- matrix(data=c(1, 0, 2, 0, 0, 2, 1, 0, 0,0,2,0,n_CN,n_HN,n_ON,n_NN), nrow=4, ncol=4)

# molecular weights : add-my-pet/SI/KooyLika2014/KooyLika2014_SI.m
w_O = t(n_O) %*% matrix(data=c(12, 1, 16, 14)) # g/mol, mol-weights for org. compounds (C = 12, H = 1, O = 16, N = 14 - matrice colonne)
w_V = w_O[2,] # should be 23.9: OK !
w_E = w_O[3,] # should be 23.9: OK !

# specific density for dry weight
d_V = 0.1              # specific density for dry weight in structure (V) g/cm^3 - fixed to AmP default value
d_E = d_V              # specific density for dry weight in reserve (V) g/cm^3 
M_V = d_V/ w_V          # mol/cm^3, [M_V] volume-specific mass of structure

# chemical potentials (J/C-mol)
mu_X = 525000 # Food
mu_V = 500000 # Structure
mu_E = 550000 # Reserve
mu_P = 480000 # Faeces
