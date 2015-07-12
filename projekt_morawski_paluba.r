
alfa=rbeta(50,5,10)

pr0=alfa/sum(alfa)

n=10

y=c(1:length(alfa))
X=vector(mode="numeric",length=n)
N=vector(mode="numeric",length=length(y))

urna<-function(){
  X[1]=sample(y,1,replace=TRUE,pr0)
  
  N[X[1]]=1
  
  for (i in 2:n){
    pr=(alfa+N)/(sum(alfa)+i-1)
    X[i]=sample(y,1,replace=TRUE,pr)
    N[X[i]]=N[X[i]]+1
  }
  
  M=N/n
  return(M)
}

m=1000
Z=matrix(data=NA,nrow=m,ncol=length(alfa))
Z=replicate(m,urna())

plot(density(t(Z)))

#por?wnanie z rozk?adem Dirichleta
install.packages("gtools")
library(gtools)

w=rdirichlet(1000,alfa)
plot(density(w))
