volume_entrant<-function(QI,QF){
  return((QI+QF)*3600/2)
}
VOL_ENT<-function(cr){N=length(cr)
VE<-rep(0,N)

for(i in 2:N){
  VE[i]<-volume_entrant(cr[i],cr[i-1])
}
return (VE)}
cr1000<-cre
cr100=crues(cre,3100,1200)
cr50=crues(cre,3100,950)
cr5=crues(cre,3100,450)
return(VOL_ENT(cr1000))
return(VOL_ENT(cr100))
return(VOL_ENT(cr50))
return(VOL_ENT(cr5))
         

