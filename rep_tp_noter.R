 elisa = read.table(file = "elisa.txt", header=T)
dim(elisa) 
plot(elisa$logd,elisa$OD,main="dosage ELISA",xlab="logd",ylab="OD")

## freg
freg_elisa =function(beta,logd) beta[1]+((beta[2]-beta[1])/(1+exp(beta[3]*(logd-beta[4]))))
## logvrais
logvrais=function(beta){sum(log(dnorm(elisa$OD,freg(beta,elisa$logd))))}
# minimisation avec nlm
nlogvrais=function(beta){-logvrais(beta)}
## nlm
elisa.st=c(0,2,1,1)
elisa.nlm=nlm(f=nlogvrais,p=elisa.st)
elisa.nlm

elisa.nlm=nlm(f=nlogvrais,p=c(1,1,1,1),iterlim=250) ## iterlim permet de choisir la limite d'itérations

elisa.nlm=nlm(f=nlogvrais,p=elisa.st,hessian=T) ## cette on renvoie hessian

beta_est=elisa.nlm$estimate
beta_est  ## renvoie les beta estimés

curve(freg(beta_est,x),add=T,col=2)

H=elisa.nlm$hessian;H  ## renvoie la matrice de variance du modèle

solve(H) ## renvoie l'inverse de la matrice de variance 

# IC(beta2)
EMV=elisa.nlm$estimate[2];EMV # 1.936694 EMV de la 2ème composante
se=sqrt(var[2,2]);se # 0.5612139 écart-type estimé de la 2ème composante del'EMV

IC=c(EMV-qnorm(0.975)*se, EMV +qnorm(0.975)*se)
IC # 0.8367346 3.0366528

