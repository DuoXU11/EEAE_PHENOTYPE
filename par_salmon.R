# DEB Parameters salmon salar #

# https://www.bio.vu.nl/thb/deb/deblab/add_my_pet/entries_web/Salmo_salar/Salmo_salar_par.html

par <- data.frame("del_M"=0.1783, ## shape coefficient
                  "E_G"=  5025,	## cout de croissance volume-sp?cifique (NRJ necessaire pour construire un volume donn? (structure))
                  "E_Hb"=	621.5, ## maturity at birth
                  "E_Hj"= 102000, ## maturity at metamorphosis (if exists)
                  "E_Hp"= 1.063*10^6, ## maturity at puberty
                  "T_A"=  6000, ## temperature d'arrhenius (K)
                  "T_ref"=293.15,## reference temperature (K)########
                  "h_a"=  8.075e-09, ## weibull aging acceleration -> pour survival
                  "k_J"=	0.002, ## maturity maint coefficient
                  "kap"=	0.41754, ## allocation fraction to soma
                  "kap_P"=0.1, ## faecation efficiency of food to faeces
                  "kap_R"=0.95, ## reproduction efficiency
                  "kap_X"=0.8, ## digestion efficiency of food to reserve
                  "p_Am"= 161.378, ## specific assimilation flux
                  "p_M"=	13.9507, ## volume-specific somatic maintenance
                  "p_T"=	0, ## surface-area-specific somatic maintenance
                  "s_G"=  1, ## Gompertz stress 
                  "v"=	  0.029776, ## Energy conductance
                  "Lw_b"= 2.486, ## Length at birth at T_ref and ad libitum food
                  "s_M" =	5.43748,
                  "E_0" = 1584.51, ## Egg energy content at T_ref and ad libitum food
                  "L_0"=  10^(-6) ## initial length (embryo)
)


