crues<-function(cr,pe,ps){
  i<-1 
  while (i<=length(cr)){
    cr[i] <- cr[i]*ps/pe 
    i<-i+1}
  return(cr)
}




