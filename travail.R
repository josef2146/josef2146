DEBIT_SORTANT<-function(CD,L,NR,NS){
  ifelse(NR>NS,Q<-CD*L*(NR-NS)^(3/2),0)
}
VOLUME8RETENUE<-function(NR){HV<-(27.784*(NR-638)^4)-(1674.3*(NR-638)^3)+((116386)*(NR-638)^2)-(752191*(NR-638))+10^6
return(HV)}

                                 
NIVEAU8RETENUE<-function(V){
  a<-638
  b<-689
  c<-(a+b)/2
  i<-0
  while (b-a>10^-6){
    ifelse((VOLUME8RETENUE(a)-V)*(VOLUME8RETENUE(c)-V) <0,b<-c,a<-c)
    c=(a+b)/2
    i<-i+1}
  return(c)
  
}










