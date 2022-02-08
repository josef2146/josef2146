close.screen(all=TRUE)
split.screen(1:2)
split.screen(c(1:2), screen = 1)
split.screen(c(1:2), screen = 2)
cre<-c(200,190,220,310,400,600,930,1260,1590,1920,2250,2430,2610,2766.67,2900,3000,3100,3000,2900,2766.67,2610,2430,2250,1920,1590,1260,930,600,560,520,480,440,400,350,300,275,250)
screen(3,4);plot(T,cre,ylab="Débits en m3/s",xlab="Temps en h",xlim=c(0,36),ylim=c(0,3100),col="black",main="Pour T=1000 ans ",type="l")
screen(4,5);plot(T,crues(cre,3100,1200),ylab="Débits en m3/s",xlab="Temps en h",xlim=c(0,36),ylim=c(0,3100),col="red",main="Pour T=100 ans ",type="l")
screen(5,6);plot(T,crues(cre,3100,950),ylab="Débits en m3/s",xlab="Temps en h",xlim=c(0,36),ylim=c(0,3100),col="green",main="Pour T=50 ans ",type="l")
screen(6,7);plot(T,crues(cre,3100,450),ylab="Débits en m3/s",xlab="Temps en h",xlim=c(0,36),ylim=c(0,3100),col="blue",main="Pour T=5 ans ",type="l")
RESULTAT<-matrix(1:148,nrow=37,ncol = 4,dimnames = list(c(0:36),c("T=1000 ans","T=100 ans","T=50 ans","T=5 ans")))
RESULTAT[,1]<-cre
RESULTAT[,2]<-crues(cre,3100,1200)
RESULTAT[,3]<-crues(cre,3100,950)
RESULTAT[,4]<-crues(cre,3100,450)


return(RESULTAT)