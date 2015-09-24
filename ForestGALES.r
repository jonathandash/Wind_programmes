# ***************************************************************************************************
# **                                    R Code for ForestGALES                                     **
# **                          Translated from MathCad into R by John Moore                         **
# **                                      August 12, 2010                                          **
# ***************************************************************************************************


ForestGALES<-function(hght, dbh, sph, age, vol, soil.type, edge.distance=500, gap.size=0, mu, sigma){
library(dplyr)
  
#This function computes the wind speed required to damage a tree based on the 
#ForestGALES modelling approach developed by Prof. Barry Gardiner
#The model has been ported over from MathCad and modified slightly for NZ conditions
                                                                                    
  #Calculate Zeroplane Displacement and Aerodynamic Roughness from Drag and Drag
  #Partitioning on Rough Surfaces by M.R. Raupach

  Cs<-0.003     #Suface Drag Coefficient
  Cr<-0.3       #Element Drag Coefficient
  con<-0.25     #constant
  k<-0.4        #von Karman's constant
  Cd<-0.29      #Drag Coefficient

  Ds<-100/sqrt(sph)
  pmh<-0.3471+0.9824*hght   #calculate predominant mean height
  b<-ifelse(Ds<0.0095+0.149*(dbh*100), Ds, 0.542+0.159*(dbh*100))  #Canopy breadth
  cht<-14.8116*pmh^0.2837*sph^(-0.1467)   #crown height - based on equation in Madgwick
  #cht<-ifelse(pmh<=8, 0, ifelse(pmh>8 & pmh<=20, 0.45*pmh, 14.8116*pmh^0.2837*sph^(-0.1467)))
    
  lamda<-(b*Cd*cht)/(Ds^2*2)  #Roughness density

  #plot(age, lamda, type="l")
  #plot(hght, cht, type="l")

  B<-Cr/Cs

  GG<-rep(NA, length(sph))
    for(i in 1:length(sph)){
      f.lamda<-function(g) g-(Cs+lamda[i]*Cr)^-0.5*exp(con*lamda[i]*(g/2))
      GG[i]<-uniroot(f.lamda, lower=0, upper=5)$root
      }

  tau.ratio<-(B*lamda)/(1+B*lamda)
  d<-tau.ratio*(1-(Cr*(b/(hght*lamda))^0.5*GG^-1))*hght

  #plot(log(lamda), d/hght)

  cw<-4
  phi.h<-log(cw)-1+cw^-1
  z0<-(hght-d)*exp(phi.h)*exp(-k*GG)

  #plot(lamda, z0/hght, type="l")

  #edge.distance<-500
  #gap.size<-0

  x.h<-ifelse(gap.size/hght>10,10, gap.size/hght)
  mean.gap10<-0.001+0.001*10^0.562
  max.gap10<-0.0072+0.0064*10^0.3467
  mean.gap.factor<-(0.001+0.001*x.h^0.562)/mean.gap10
  max.gap.factor<-(0.0072+0.0064*x.h^0.3467)/max.gap10

  s.h<-ifelse(Ds/hght<0.0075,0.0075,ifelse(Ds/hght>0.45, 0.45, Ds/hght))
  #plot(age, s.h)

  mean.bm.edge<-(0.68*s.h-0.0385)+(-0.68*s.h+0.4785)*(1.7329*s.h+0.0316)^(edge.distance/hght)*mean.gap.factor
  max.bm.edge<-(2.7193*s.h-0.061)+(-1.273*s.h+0.9701)*(1.1127*s.h+0.0316)^(edge.distance/hght)*max.gap.factor
  mean.edge5<-(0.68*s.h-0.0385)+(-0.68*s.h+0.4785)*(1.7329*s.h+0.0316)^5*mean.gap.factor
  max.edge<-(-1.273*s.h+0.9701)*(1.1127*s.h+0.0316)^(edge.distance/hght)*max.gap.factor
  gust<-max.bm.edge/mean.bm.edge
  edge.factor<-mean.bm.edge/mean.edge5

#Add in different soil types

soil.type.mult<-switch(soil.type,
      B = 98452,
      BF = 94889,
      BS = 91745,
      BO = 77545,
      M = 84690,
      R = 80935,
      Z = 98452,
      P = 114094,
      U = 100548,
      L = 80935,
      R = 80935,
      N = 80935,
      E = 98452,
      G = 98452,
      O = 98452,
      X = 98452)


utenturn<-(2.23/Ds)*((soil.type.mult*vol)/(gust*d*edge.factor))^0.5*log(10/z0)

#Calculate AEP from Weibull A and k parameters

#U.c<-13.5690-11.8633*Weib.k+4.4345*Weib.k^2-0.590*Weib.k^3
#U<-(U.c*Weib.A)^2

#AEP<-1-exp(-1*exp(-1*((utenturn^2-U)/(U/5))))

AEP<-1-exp(-1*exp(-1*((utenturn^2-mu)/sigma)))

RP<-1/AEP

#results<-c(AEP)
results<-c(utenturn)#, AEP, RP)
print(results)

#need to add cumulative probability of damage

#plot(age, utenturn, type="l", ylim=c(0,40))

}

#ForestGALES<-function(hght, dbh, sph, age, vol, soil.type, edge.distance=500, gap.size=0, mu, sigma){
test<-data.frame(id=c(1,2), hght=c(22,35), dbh=c(330,440), sph=c(550,560), age=c(27,28), vol=c(2, 2), soil.type=c('B','B'),
                 edge.distance=c(500,500), gap.size=c(0,0), mu= c(12.7, 12.2), sigma=c(1.2, 1.5))

FG.out<- test %>% group_by(id) %>% summarise (cws=ForestGALES(hght, dbh, sph, age, vol, soil.type, edge.distance, 
                                                              gap.size, mu, sigma))




