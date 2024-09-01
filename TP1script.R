###############################
#### Elements de corrigé du TP1
###############################
rm(list=objects())     # supprime les objets existant en session
graphics.off()         # supprime les graphiques existant en session
setwd("MonRepertoire") # définit le répertoire en cours, à compléter



################################
#### 1- Echantillon de Bernoulli
################################

?rbinom
p=0.3
n=20
# échantillon de Bernoulli de taille n de paramètre p
Y=rbinom(n,1,p)

# log-vraisemblance (forme développée)
logL=function(p,ech=Y) sum(Y)*log(p)+(length(Y)-sum(Y))*log(1-p)
logL(seq(0.1,0.9,by=0.1),Y)

# log-vraisemblance (forme générique)
# logL=function(p,Y) sum(log(dbinom(Y,1,p)))

# Vectorize transforme une fonction g dont le paramètre d'entrée 
# est un scalaire en une *fonction vectorielle*, c'est-à-dire 
# une fonction qui évalue g en chaque point d'un vecteur d'entrée:
# vlogL=Vectorize(logL,"p")
# vlogL(seq(0.1,0.9,by=0.1),Y)

# tracé de la log-vraisemblance
curve(logL(x,ech=Y),from=0,to=1,
      main="log-vraisemblance de l'échantillon",
      ylab="logL(p)",xlab="p",
      xlim=c(0,1),ylim=c(-70,0))

EMV=mean(Y)
abline(v=EMV,col="red",lty=2)
points(EMV,logL(EMV),pch=19,col=2)
text(0.6,-40,"p=0.3")
text(0.6,-50,paste("EMV=",round(EMV,2)),col="red")

# on répète l'expérience pour observer la variabilité de l'EMV
# (et de la vraisemblance)
Y=rbinom(n,1,p)
curve(logL(x,ech=Y),from=0,to=1,lty=2,add=T)
abline(v=mean(Y),lty=2)

################################
#### 2- Ajustement de données
################################

df=read.table("duree_de_vie.txt",skip=1,header=T)
summary(df)
times=df$times
n=length(times); n; mean(times); var(times)

# histogramme normalisé en densité
hist(times,proba=T)

?dweibull
# log-vraisemblance de Weibull
LogL=function(a) sum(log(dweibull(times,a)))
LogL(c(1,2,3,4))
# dernière valeur uniquement

# Vectorisation de la fonction LogL
vLogL=Vectorize(LogL)
vLogL(c(1,2,3,4))
# [1] -17.70000 -10.72662 -12.10989 -18.80631

# tracé de la log-vraisemblance
curve(vLogL,from=0.17,to=6, main="log-vraisemblance de l'échantillon",
      ylab="LogL(a)",xlab="valeur du paramètre") # maximum obtenu entre 2 et 2.5

# optimisation de la log-vraisemblance
opt=optimize(LogL, interval = c(0.17,6), maximum = TRUE)
print(opt)
# [1] 2.258714 le maximum est global sur l'intervalle considéré
EMV=opt$maximum

abline(v=EMV,col="red",lty=2)
text(3,-45,paste("EMV=",round(EMV,2)),col="red")

# tracé de la densité estimée
hist(times,proba=T)
curve(dweibull(x,shape=EMV),from=0,to=1.8,add=T,col=2)

################################
#### 3- QQplot
################################

n=length(times)
plot(qweibull(ppoints(n),shape=EMV),quantile(times,ppoints(n)),col="blue",
     xlab="quantile théorique", ylab="quantile empirique")
abline(0,1)

# ou bien (quantiles empirique légèrement différents)
qqplot(qweibull(ppoints(n),shape=EMV),times)
abline(0,1)

