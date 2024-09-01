
# données
y=c(9.82, 7.35, 4.41, 3.74 ,1.30, 0.85, 0.27,   0.23,  0.54)
time=c(1,  4,  7,  10 , 13,  16,  22,  25,  28)
n=length(time)
PK=data.frame(time=time,y=y)
plot(time,y,pch=20,col="red")

# fonction de régression
freg=function(beta,time)
{   
k=beta[1]
V=beta[2]
return((100/V)* exp(-k*time))
}

# log-vraisemblance
logvrais=function(beta){sum(log(dnorm(y,freg(beta,time))))}

# ordonnée à l'origine V=10
# pente à l'origine 10*k=.5
nlogvrais=function(beta){-logvrais(beta)}
res=optim(par=c(.5,10),nlogvrais)
res$par
# [1] 0.1358591 8.5819458
res$convergence
# [1] 0

# ajustement
curve(freg(res$par,x),from=0,to=28,add=T,col="blue" )



