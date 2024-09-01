rm(list=objects())   
graphics.off()    

### Ex 1

set.seed(2022)
n=50
p=0.4
Y=rgeom(n,p)

nlogL=function(p) -sum(log(dgeom(Y,p)))
# nlogL=Vectorize(nlogL)
# curve(nlogL,from=0, to=1)
res=optimize(nlogL,c(0,1))
res$par # [1] 0.4032278
res=optim(par=.5,nlogL,hessian=T,method="Brent",lower=0.1,upper=0.9)
res$par #[1] 0.4032258
se=1/sqrt(res$hessian) # 0.04405206

# calcul analytique
ph=1/(1+mean(Y))
ph
#[1] 0.4032258 idem
Ih=n/ph^2+sum(Y)/(1-ph)^2
1/sqrt(Ih)
#[1] 0.04405227 idem

hist(Y,proba=T,
     main="histogramme ajusté par loi géométrique")
x=0:max(Y)
points(x,dgeom(x,res$par),col=2)

### Ex 2
don=read.table("jaws.txt",header=T)
dim(don)
attach(don)

# log-vraisemblance
mlogvrais=function(beta){
  a=beta[1]
  b=beta[2]
  c=beta[3]
  -sum(dnorm(bone,a-b*exp(-c*age),log=T))
}

# asymptote a=100
# ordonnée à l'origine=0: b=a
# c>0
res=optim(c(100,100,1),mlogvrais)
res$par
#[1] 115.253155 118.686122   0.123509

plot(don,pch=16)
curve(res$par[1]-res$par[2]*exp(-res$par[3]*x),add=T)

pred=res$par[1]-res$par[2]*exp(-res$par[3]*20)
pred
# [1] 105.2159
