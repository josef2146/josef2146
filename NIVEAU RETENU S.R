#Volume d'eau de pluie
V_pluie<-rep(0,12)
pluie<-c(9.78,17,14,11,10.1,13,8.66,7.8,5.5,3.7,2.4,4.89)
for(j in 1:12){
  V_pluie[j]<-SURFACE_RETENUE(675)*10^3*pluie[j]
}

return(V_pluie)
 
#volume d'eau pour produir de l'énergie électrique en pointe
Heures_P<-c(0,0,50,85,70,55,0,0,0,0,0,0)
Vhp<-rep(0,12)
for(k in 1:12){
  Vhp[k]<-13*3600*Heures_P[k]
}
return(Vhp)
  