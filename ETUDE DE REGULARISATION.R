#Régularisation de la retenue d'un barrage (Exemple pour l'année 1997)

V_final<-rep(0,12)
V_evap<-rep(0,12)
Heures_P<-c(0,0,50,85,70,55,0,0,0,0,0,0)
Evap<-c(320,216,142,96,92,11,182,243,304,383,446,420)
pluie<-c(9.78,17,14,11,10.,13,8.66,7.8,5.5,3.7,2.4,4.89)
V_app<-c(46.500,15.031,5.932,5.113,5.606,4.896,4.405,4.679,7.607,5.306,1.133,3.357,109.564)
D_aep<-rep(0,12)
D_irr<-rep(0,12)
V_init<-rep(0,12)
t_irr<-0.32
t_aep<-0.20
Pui_élec<-0
Vpluie<-rep(0,12)
i<-1
Daep<-6.6 #Pour chaque année , on doit inserer la valeur de Daep , les apports et la volume initial
while (i <=12){
  
  Vpluie[i]<-(SURFACE_RETENUE(NIVEAU_RETENUE(V_init[i]*10^6))+SURFACE_RETENUE(NIVEAU_RETENUE((V_init[i]+V_app[i])*10^6))*pluie[i]*10^-3)/2
  
  V1<-V_app[i]+V_init[i]+Vpluie[i]-(DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]+DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i])
  
  V2<-V_app[i]+V_init[i]+Vpluie[i]-((1-t_irr)*DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]+DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i])
  
  V3<-V_app[i]+V_init[i]+Vpluie[i]-((1-t_irr)*DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]+(1-t_aep)*DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i])
  
  V4<-V_app[i]+V_init[i]+Vpluie[i]-((1-t_aep)*DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i])
  
  V5<-V_app[i]+V_init[i]+Vpluie[i]
  
  
  if(V1>(VOLUME_RETENUE(675)*10^-6)){
    D_aep[i]<-0
    D_irr[i]<-0
    V_evap[i]<-0
    V_final[i]<-(VOLUME_RETENUE(675)*10^-6)-Vhp[i]
    V_init[i+1]<-V_final[i]-V_evap[i]
    Pui_élec<-Pui_élec+13*50*1000*9.81*0.88*Heures_P[i]*10^-9
  }
  else if (V1>0){
    D_aep[i]<-0
    D_irr[i]<-0
    if(NIVEAU_RETENUE((V_init[i]+V_app[i]+Vpluie[i]-(DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]+DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i]))*10^6)>660){
      Q<-13*50/(NIVEAU_RETENUE((V_init[i]+V_app[i]+Vpluie[i]-(DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]+DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i]))*10^6)-660)
      Vw<-Q*3600*Heures_P[i]
      if(Vw>Vhp[i]){
        V_final[i]<-V_init[i]+V_app[i]+Vpluie[i]-(DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]+DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i])-Vhp[i]*10^-6
        Pui_élec<-Pui_élec+13*50*1000*9.81*0.88*Heures_P[i]*10^-9
        }
      else{
        V_final[i]<-V_init[i]+V_app[i]+Vpluie[i]-(DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]+DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i])-Vw*10^-6
        Pui_élec<-Pui_élec+Vw*1000*9.81*0.88*(NIVEAU_RETENUE((V_init[i]+V_app[i]+Vpluie[i]-(DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]+DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i]))*10^6)-660)/3600
      }
    }
    else{V_final[i]<-V_init[i]+V_app[i]+Vpluie[i]-(DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]+DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i])
    Pui_élec<-Pui_élec+0
    }
    
    V_evap[i]<-((SURFACE_RETENUE(NIVEAU_RETENUE(V_init[i]*10^6))+SURFACE_RETENUE(NIVEAU_RETENUE(V_final[i]*10^6)))*Evap[i]*10^-3)/2
    V_init[i+1]<-V_final[i]-V_evap[i]
    
    }
  else if (V2>0){
    D_aep[i]<-0
    D_irr[i]<-DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]-(V_app[i]+V_init[i]+Vpluie[i]-DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i])
    V_evap[i]<-0
    V_final[i]<-0
    V_init[i+1]<-0
    Pui_élec<-Pui_élec+0
  }
  else if(V3>0){
    D_aep[i]<-(DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i]-(V_app[i]+V_init[i]+Vpluie[i]-(1-t_irr)*DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]))*100/DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i]
    D_irr[i]<-t_irr*DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]*100/DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]
    V_evap[i]<-0
    V_final[i]<-0
    V_init[i+1]<-0
    Pui_élec<-Pui_élec+0
    
  }
  else if (V4>0) {
    D_aep[i]<-t_aep*DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i]*100/DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i]
    D_irr[i]<-(DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]-(V_app[i]+V_init[i]+Vpluie[i]-(1-t_aep)*DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i]))*100/DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]
    V_evap[i]<-0
    V_final[i]<-0
    V_init[i+1]<-0
    Pui_élec<-Pui_élec+0
  }
  else if (V5>0){
    D_aep[i]<-(DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i]-V5)*100/DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[2,i]
    D_irr[i]<-DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]*100/DEMANDE_EN_EAU(Irrigation,AEPI,34,Daep)[1,i]
    V_evap[i]<-0
    V_final[i]<-0
    V_init[i+1]<-0
    Pui_élec<-Pui_élec+0
    
    
    
  }
  
  
  
  

  i<-i+1
}






RESULTAT<-matrix(1:60,nrow=5,ncol = 12,dimnames = list(c("Volume initial","Volume final","Volume évaporé","Déficit en AEPI","Déficit en irrigation"),c("Sept","Oct","Nov","Déc","Janv","Févr","Mars","Avr","Mai","Juin","Juil","Aout")))
RESULTAT[1,]<-V_init[1:12]
RESULTAT[2,]<-V_final
RESULTAT[3,]<-V_evap
RESULTAT[4,]<-D_aep
RESULTAT[5,]<-D_irr

return(RESULTAT)


return(max(D_aep))
return(max(D_irr))
return(Pui_élec)


