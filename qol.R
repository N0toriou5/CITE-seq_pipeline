# Quality of Life functions


slice<-function(x){
  return(x[1:5,1:5])
}

# Correlation to P-value
r2p<-function(r,n){
    t<-(r*sqrt(n-2)) / sqrt(1-(r^2))
    p<-2*pt(t,df=n-2,lower=FALSE)
    return(p)
}
