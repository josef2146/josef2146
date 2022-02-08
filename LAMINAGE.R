cr<-crues(cre,3100,3100)
N=length(cr)
VS<-rep(0,N)

NR<-rep(0,N)

QS<-rep(0,N)

VSOR<-rep(0,N)

VE<-VOL_ENT(cr)

NR[1]<-675

VS[1]<-VOLUME8RETENUE(NR[1])

QS[1]<-0

VSOR[1]<-0

for (j in 2:N){
  VS[j]<-VE[j]+VS[j-1]-VSOR[j-1]
  NR[j]<-NIVEAU8RETENUE(VS[j])
  QS[j]<-DEBIT_SORTANT(2.144,60,NR[j],675)+14
  VSOR[j]<-(QS[j]+QS[j])*(3600)/2
}
return(cr)
return(VE)
return(NR)
return(VS)
return(QS)
return(VSOR)
RESULTAT<-matrix(1:222,nrow=37,ncol = 6,dimnames = list(c(0:36),c("Débit entrant","Volume entrant","Cote Retenue","Volume stockage","Débit sortant","Volume sortant")))
RESULTAT[,1]<-cr
RESULTAT[,2]<-VE
RESULTAT[,3]<-NR
RESULTAT[,4]<-VS
RESULTAT[,5]<-QS
RESULTAT[,6]<-VSOR
return(RESULTAT)

split.screen(1:2)
screen(1);plot(T,cr,ylab = "débit en m3/s",xlab ="Temps en h" ,main = " Hydrogramme d'entrée ",xlim=c(0,36),ylim = c(0,3200))
screen(2);plot(T,QS,ylab = "débit en m3/s",xlab ="Temps en h" ,main = " Hydrogramme de sortie ",xlim=c(0,36),ylim = c(0,3200))
return(max(cr))
return(max(QS))
return(max(NR))
return(max(VS))  