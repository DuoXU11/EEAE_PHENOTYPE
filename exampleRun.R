#### Initialisation #### ------------------------------------------------------------------------------------------------------
library(purrr)
# Packages
#rm(list=ls())



##simulation 1 P_Temp=0, fix P_density at certain value, check different T in two system
##simulation 2 P_Temp=0, fix T at the optimal, check the impact of different density of parasite in two system
##simulation 3 P_Temp=1, check what will happen if parasite density increase with temperature




P_Temp<-1 #if parasite increase with temperature ="1"

system_type<- c("OpenPens","ClosedWater")
for (s_t in 1:2){
type <- system_type[s_t]
# Systems-specific adjustment
if (type == "OpenPens"){            ##Openpens

  # Food (constant)
  dataf = 0.8
  
  # Parasite density
  if (P_Temp==1){
    dataP = matrix(seq(0,0.4,length=6), ncol = 1) 
  }else{
    dataP = matrix(seq(0,0.4,length=6), ncol = 1) 
  }
  
}else{                                    ##ClosedWater
  
  # Food (constant)
  dataf = 1
  
  # Parasite density                      ##adjust
  if (P_Temp==1){
    dataP = matrix(seq(0,0.4,length=6), ncol = 1) 
  }else{
    dataP = matrix(seq(0,0.4,length=6), ncol = 1) 
  }
  
}

alpha <- 0.25 # Maintenance increase
beta <- 0.15  # Food availability reduction 


# Temperature (levels)
dataT = matrix(seq(278.15,303.15,length=6), ncol = 1) 


# Retrieve parameters
source("par_salmon.R")
source("parchem_salmon.R")

# Fix maximum length and time step
par$t_max = 3000 # in days
par$dt = 1 # time step (in days)




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
#View(test[[6]])
Lgrow<-NULL
for (id in 1:6) {
  testT<-test[[id]]
  Lgrow<-cbind(Lgrow,testT$Lw,testT$stage,testT$temp)
  counter<-1
  strs<-c("0->1","1->2")
  
  for (i in 1:(nrow(testT) - 1)){
    if ((testT[i+1,]$stage - testT[i,]$stage)==1){
      print(paste(id,i,dataT[id,],strs[counter]))
      counter<-counter+1
    }
  }
}

Lgrow<-data.frame(time=0:par$t_max,Lgrow)
library(ggplot2)
library(tidyr)
data1 <- Lgrow[, 1:4]
data2 <- Lgrow[, c(1,5,6,7)]
data3 <- Lgrow[, c(1,8,9,10)]
data4 <- Lgrow[, c(1,11,12,13)]
data5 <- Lgrow[, c(1,14,15,16)]
data6 <- Lgrow[, c(1,17,18,19)]
colnames(data2) <- colnames(data1)
colnames(data3) <- colnames(data1)
colnames(data4) <- colnames(data1)
colnames(data5) <- colnames(data1)
colnames(data6) <- colnames(data1)

data <- rbind(data1, data2,data3, data4,data5, data6)
colnames(data)<-c('Time','Length','stage','Temp')
# data<- pivot_longer(Lgrow, cols = starts_with("X"), names_to = "Variable", values_to = "Value")
# data$Variable[which(data$Variable=="X1")]<-dataT[1]
# data$Variable[which(data$Variable=="X2")]<-dataT[2]
# data$Variable[which(data$Variable=="X3")]<-dataT[3]
# data$Variable[which(data$Variable=="X4")]<-dataT[4]
# data$Variable[which(data$Variable=="X5")]<-dataT[5]
# data$Variable[which(data$Variable=="X6")]<-dataT[6]

if (type == "OpenPens"){
  dataOP<-cbind(data,type="Open Pens")
}else{
  dataCW<-cbind(data,type="Closed Water")
}




}
combined_data <- rbind(dataOP, dataCW)


annotations <- data.frame(
  type = c("Closed Water", "Open Pens"),    # Facet names
  x = c(500, 500),                       # X positions for text
  y = c(60, 60),                         # Y positions for text
  label = c("f=1", "f=0.8")  # Multi-line text using \n
)
  ggplot(combined_data, aes(x = Time)) + 
  # Line for Length with Temp as color
  geom_line(aes(y = Length, color = factor(Temp)), size = 1) +
  
  # Line for Stage scaled to match Length on the secondary y-axis
  geom_line(aes(y = stage * max(Length) / max(stage), 
            color = factor(Temp)), linetype = "dashed", size = 1) +
  
  # Primary and secondary y-axes
  scale_y_continuous(
    name = "Length/cm", 
    sec.axis = sec_axis(~ . * max(data$stage) / max(data$Length), 
                        name = "Stage")
  ) +
  
  geom_text(data = annotations, aes(x = x, y = y, label = label), 
            size = 4, color = "black", fontface = "bold", inherit.aes = FALSE) +
  
  # Facet based on 'type'
  facet_wrap(~ type, scales = "free") +
  
  # Labels and theme
  labs(
    title = "Length and Stage Over Time",
    x = "Time",
    color = "Temp, P_dens"
  ) +
    scale_color_manual(
      breaks = c("278.15", "283.15", "288.15", "293.15", "298.15", "303.15"), # Original numbers
      labels = c("278.15, 0.00", "283.15, 0.08", "288.15, 0.16", "293.15, 0.24", "298.15, 0.32", "303.15, 0.40"), # New labels
      values = c("#F8766D","#C49A00","#53B400","#00BFC4", "#619CFF","#FB61D7" )
      )+
  theme_gray() +
  theme(
    axis.title.y.left = element_text(color = "black"),  # Left y-axis color
    axis.title.y.right = element_text(color = "black") # Right y-axis color
  )
