rm(list=objects())     # supprime les objets existant en session
graphics.off()         # supprime les graphiques existant en session
setwd("MonRepertoire") # définit le répertoire en cours, à compléter

###############################
#### 1- Données de comptage
###############################

don=read.table("counts.txt")
y=don$V1
length(y) # 16

n=2:17 # nb emissions de particules
y # comptages (données aggrégées)

# EMV (données agrégées)
EMV=sum(n*y)/sum(y)  # weighted.mean(n,y) idem
# [1] 8.369511

# ou bien on reconstitue les données individuelles
#x=rep(n,y)
# mean(x) idem

# diagramme en bâtons
plot(n,y/sum(y),type="h",xlab="n",ylab="proportion",ylim=c(0,.15),
     main="distribution des comptages ajustée par une loi de Poisson")
points(n,dpois(n,EMV),col=2) 

## graphique quantile-quantile
# permet de mieux juger visuellement de la qualité d'ajustement de 
# la loi théorique aux données
# compare quantiles empiriques de l'échantillon aux quantiles estimés 
# de la loi théorique

plot(qpois(ppoints(n),EMV),quantile(rep(n,y),ppoints(n)))
abline(0,1)
# la loi de Poisson estimée est un bon ajustement de la loi générative
# des données
# Ici l'alignement avec la 1ère bissectrice est particulièrement bon car 
# la loi est discrète donc les quantiles prennent des valeurs entières. 
# Lorsque la loi est continue, la variabilité par rapport à la droite 
# est plus visible même avec un ajustement correct. 

# log-vraisemblance de Poisson (données agrégées)
logL=function(lambda) sum(n*y)*log(lambda)-sum(y)*lambda

# ou bien avec données individuelles: rep(n,y)
logL=function(lambda) sum(log(dpois(rep(n,y),lambda)))
vlogL=Vectorize(logL)

# tracé de la log-vraisemblance
curve(vlogL(x),from=0,to=17,
      main="log-vraisemblance de l'échantillon",
      ylab="p",xlab="valeur du paramètre")
# optimize(logL,c(0,15),maximum=T)$optimize (on retrouve l'EMV)

abline(v=EMV,col="red",lty=2)
text(10,-15000,paste("EMV=",round(EMV,2)),col="red")

# Intervalle de confiance avec calcul info de Fisher observée (estimée)
se=sqrt(EMV/sum(y)) # 0.0832715
IC=c(EMV-qnorm(0.975)*sqrt(EMV/sum(y)), EMV +qnorm(0.975)*sqrt(EMV/sum(y)))
# [1] 8.206302 8.532720

## possible de retrouver le calcul numérique du hessien avec optim
## optim, algo de MINIMISATION 

# on minimise -logL
nlogL=function(lambda) -sum(log(dpois(rep(n,y),lambda)))
nlogL=Vectorize(nlogL)
res=optim(7,nlogL,hessian=T,method="Brent",lower=1,upper=11)
res$par # 8.369511 idem EMV
1/sqrt(res$hessian) # 0.0832715 idem s.e. calculé plus haut


################################################
#### 2- Etude par simulation de la loi de l'EMV
################################################
par(mfrow=c(1,1))

set.seed(123)
n=20
lambda=3
Y=rpois(n,lambda)
hist(Y,proba=T,xlim=c(0,10),
     main="distribution des comptages ajustée par une loi de Poisson")
points(0:10,dpois(0:10,lambda),col=2)

S=200
EMV=replicate(S, mean(rpois(n,lambda)) )

### loi d'échantillonnage de l'EMV
hist(EMV,proba=T,ylim=c(0,1.2))

# écart-type théorique= racine de l'inverse de l'info. de Fisher théorique
se_theta=sqrt(lambda/n)

### adéquation de la loi asymptotique gaussienne

# Estimation de la densité
hist(EMV,proba=T,ylim=c(0,1.2))
curve(dnorm(x,lambda,se_theta),add=T)

# Comparaison des fonctions de répartition empirique et théorique
Fn=ecdf(EMV)
plot(Fn)
curve(pnorm(x,lambda,se_theta),add=T,col="red")

# qqplot
qqplot(qnorm(ppoints(n),lambda,se_theta),quantile(EMV,ppoints(n)))
abline(0,1)


