# Mapathé FAYE

########################################
# 1- Estimation d'une densité 
#####################################
## 1)
set.seed(0509)
n = 20
p = 0.3
y = rgeom(n,p)
mean(y) ## 1.45
yrep=factor(y)
table(yrep)
densite_y <- table(y) / length(y) ## normalise yrep en densté
# diagramme en batons
plot(densite_y,type="h", main = "Diagramme en bâtons de yrep normalisé en densité", 
        xlab = "Valeurs de yrep", ylab = "Densité")
points(0:6,dgeom(0:6, 0.3,col=2))
##2)
log_vraisemblance <- function(p) {
  sum(dgeom(y, prob = p, log = TRUE))   ## fonction de log-vrais
}
vectorise_log_vraisemblance <- Vectorize(log_vraisemblance) ## vectrise la log-vrai
 
# traços la log-vraisemblance
curve(vectorise_log_vraisemblance, from = 0,
      to = 1,main= "log-vraisemblance de l'échantillon", 
      xlab = "p", ylab = "logL(p)")


# 3) 
EMV_p <- optim(par = 0.5, fn = function(p) 
  -log_vraisemblance(p), method = "Brent",
  lower = 0, upper = 1)$par

print(EMV_p) ##  0.4081633
# Pour calculer l'écart-type estimer de l'estimateur nous pouvons utiliser la méthode de la matrice d'information de Fisher inverse
# La matrice d'information de Fisher est l'espérance du double de la dérivée seconde de la log-vraisemblance par rapport à p
# L'écart-type de l'estimateur du maximum de vraisemblance est alors l'inverse de la racine carrée de cette valeur.

#4) 
plot(densite_y,type="h", main = "Diagramme en bâtons de yrep normalisé en densité", 
     xlab = "Valeurs de yrep", ylab = "Densité")
points(n,dgeom(n,EMV_p),col=2)

# Paramètre estimé p
p_estime <- EMV_p

# Densité géométrique estimée
densite_geometrique <- dgeom(0:max(y), prob = p_estime)
#supperpose
lines(densite_geometrique, col = "red")

# QQplot de l'échantillon y
qqplot(qgeom(ppoints(length(y)), prob = p_estime), y)
abline(0, 1, col = "blue", lwd = 2)
########################################
# Estimation d'une regression non-linéaire
#############################################
#1) D'après le model ODi a pour loi une loi gaussienne d'espérance f(logdi,B) et de variance sigma^2 

## 2) 
elisa = read.table(file="elisa.txt", header=T,dec=".")
head(elisa)
n = dim(elisa) # 16 lignes et 2 colonnes
plot(elisa$logd,elisa$OD) ## beta1 = 5, beta2 = 1.5

#3) fonction de régression

beta = c(beta1,beta2,beta3,beta4)
freg <- function(beta,logd) {
  beta1 <- beta[1]
  beta2<- beta[2]
  beta3<- beta[3]
  beta4<- beta[4]
  beta1=(beta2-beta1)/(1+exp(beta3*(logd-beta4)))
  
}

# 4) fonction logvrais
lodd = elisa$logd
ODi = elisa$OD
logvrais = function(beta){
  sum(log(dnorm(ODi,freg(beta,logd))))
  }

# 5)
nlogvrais = fonction(beta) -logvrais
?nlm
res=nlm(logvrais,beta)

fit.nls = nls(OD~freg(beta,logd),data=elisa,start =list(beta=c(1,2,3,4)))
