rm(list=objects());graphics.off()

########################################################
#### exo 1
#################

set.seed(2024)
n=20
y=rgeom(n,0.3)
mean(y) #  3.25
yrep=factor(y)
summary(yrep)
# 0  1  2  3  4  7  9 25 
# 3  9  2  1  2  1  1  1 
z=table(yrep);z # données agrégées
plot(z/sum(z),type="h",main="diagramme en batons",ylab="density")
# ou aussi barplot(z/sum(z))

     
logL=function(p) sum(log(dgeom(y,p)))
vlogL=Vectorize(logL)
curve(vlogL,from=0.01,to=1)

nlogL=function(p) -logL(p)
res=optim(0.5,nlogL,hessian=T,method="Brent",lower=0,upper=1)
res$par # 0.2352941 

# l'EMV résout l'éq. de vraisemblance : n/p-sum(y)/(1-p)=0
# on obtient EMV= n/(sum(y)+n) ou 1/(1+mean(y))
EMV=1/(1+mean(y));EMV # 0.2352941 idem 
plot(z/sum(z),type="h",main="estimation de la loi de y",ylab="density")
points(0:max(y),dgeom(0:max(y),prob=EMV),col=2) 

# écart-type: la variance de l'EMV est estimée par l'inverse du hessien de nlogL
1/sqrt(res$hessian) # 0.0460084 

# par le calcul direct, on estime l'info de Fisher
I=n/EMV^2-sum(y)/(1-EMV)^2; Iobs # 250.0962
se=1/sqrt(Iobs); se  #  0.06323339 res différent car n petit
# le hessien a des valeurs variables autour de la moyenne théorique I

# qqplot
plot(qgeom(ppoints(n),prob=EMV),quantile(y,ppoints(n)))
abline(0,1)

qqplot(qgeom(ppoints(n),prob=EMV),y)
points(qgeom(ppoints(n),prob=EMV),quantile(y,ppoints(n)),col=2)
abline(0,1)

#### exo 2
##########################################

# les ODi suivent une loi gaussienne d'espérance 
# f(logdi,beta) et de variance sigma2

elisa=read.table("elisa.txt",header=T)
dim(elisa) # 16  2 
plot(elisa$logd,elisa$OD,main="dosage ELISA",xlab="logd",ylab="OD")

###  Estimation avec nlm

# fonction de régression
freg=function(beta,x) beta[1]+(beta[2]-beta[1])/(1+exp(beta[3]*(x-beta[4])))

# log-vraisemblance 
logvrais=function(beta){sum(log(dnorm(elisa$OD,freg(beta,elisa$logd))))}

# minimisation avec nlm
nlogvrais=function(beta){-logvrais(beta)}

# b_1 représente l'asymptote inférieure et b_2 l'asymptote supérieure
elisa.st=c(0,2,1,1)
elisa.nlm=nlm(f=nlogvrais,p=elisa.st)
elisa.nlm

# $minimum
# [1] 14.70699
# 
# $estimate
# [1] 0.04209397 1.93669384 2.56300699
# [4] 3.46751110
# 
# $gradient
# [1]  2.561507e-06  1.944487e-07
# [3] -3.098047e-07 -6.567504e-07
# 
# $code
# [1] 1
# 
# $iterations
# [1] 85

# attention avec elisa.st=c(1,1,1,1), iterations=100 et pas de cv!
elisa.nlm=nlm(f=nlogvrais,p=c(1,1,1,1),iterlim=250)

elisa.nlm=nlm(f=nlogvrais,p=elisa.st,hessian=T)

beta_est=elisa.nlm$estimate
beta_est
# 0.04209397 1.93669384 2.56300699 3.46751110
curve(freg(beta_est,x),add=T,col=2)

H=elisa.nlm$hessian;H
# [,1]      [,2]        [,3]       [,4]
# [1,]  5.073924 1.6040565 -0.50593397 3.84480312
# [2,]  1.604056 7.7179627  0.57933175 3.94444078
# [3,] -0.505934 0.5793317  0.19170200 0.01826627
# [4,]  3.844803 3.9444408  0.01826627 6.41242162

V=solve(H) # l'inverse de H est une estimation de la matrice de variance de l'EMV

# IC(beta2)
EMV=elisa.nlm$estimate[2];EMV # 1.936694 EMV de la 2ème composante
se=sqrt(V[2,2]);se # 0.5612139 écart-type estimé de la 2ème composante del'EMV

IC=c(EMV-qnorm(0.975)*se, EMV +qnorm(0.975)*se)
IC # 0.8367346 3.0366528
