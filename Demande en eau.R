
#Demande en eau de AEPI et l'irrigartion 
Irrigation<-c(9,9,4,3,3,4,6,8,12,13,14,15)
AEPI<-c(10.9,8.3,8.3,8.3,8.3,5.8,5.8,5.8,5.8,10.9,10.9,10.9)

DEMANDE_EN_EAU<-function(Irrigation,AEPI,DMIRR,DMAEP){
  N=12
  DEMANDE_Irrigation<-rep(0,N)
  DEMANDE_AEPI<-rep(0,N)
  
  for(i in 1:N){
    DEMANDE_Irrigation[i]=Irrigation[i]*DMIRR/100
    DEMANDE_AEPI[i]=AEPI[i]*DMAEP/100
    DEMANDE<-matrix(nrow = 2,ncol = 12,dimnames = list(c("Irrigation (Mm3)","APEI (Mm3)"),c("s","O","N","D","J","F","M","A","M","J","J","A")))
    DEMANDE[1,]=DEMANDE_Irrigation
    DEMANDE[2,]=DEMANDE_AEPI
  }
  return(DEMANDE)
  
  
}


Resultat<-matrix(nrow = 39,ncol = 12,dimnames = list(c(1997:2035),c("s","O","N","D","J","F","M","A","M","J","J","A")))

for(j in 0:23){
  
  Resultat[j+1,]<-DEMANDE_EN_EAU(Irrigation,AEPI,34,6.6+((38.2-6.6)*(j)/(2020-1997)))[2,]
  
  
}
for(k in 1:15){
  
  Resultat[k+24,]<-DEMANDE_EN_EAU(Irrigation,AEPI,34,38.2+((48-38.2)*k/(2035-2020)))[2,]
  
  
}

return(Resultat)


return(DEMANDE_EN_EAU(Irrigation,AEPI,34,6.6))

return(DEMANDE_EN_EAU(Irrigation,AEPI,34,38.2))

return(DEMANDE_EN_EAU(Irrigation,AEPI,34,48))





