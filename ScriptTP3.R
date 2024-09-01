
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
(100/V)* exp(-k*time)
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
# ----------------------------------------------------------------------------
# estimation avec nls
# ----------------------------------------------------------------------------
beta0=c(0.5,10)
PK.nls=nls(y~freg(beta,time),data=PK, 
           start=list(beta=beta0),trace=T)

PK.nls$convInfo # pour voir si l'algo à converger (fournis les infos sur la convergence)
PK.nls$control #  renvoie les paramètres de contrôle associés au processus d'optimisation

summary(PK.nls)
# En conclusion, le modèle semble bien ajusté aux données, avec des paramètres significatifs et 
# une convergence rapide de l'algorithme d'estimation des paramètres.
# L'erreur résiduelle semble relativement faible, ce qui suggère que le modèle
# peut expliquer une grande partie de la variation dans les données

coef(PK.nls) # renvoie les paramètres d'espérance du modèle (ici k et v)
# 0.1358583 8.5821859 idem qu'avec optim

fitted(PK.nls)  # renvoie les valeurs ajustées par le modèle
resid(PK.nls)   # renvoie les résidus du modèle

## matrice de variance estimée
Var=vcov(PK.nls) # calcule la matrice de variance-covariance 
sqrt(Var[2,2])   # [1] 0.4631738 on retrouve écart-type de beta2
sqrt(Var[1,1]) # [1] 0.01137273 on retrouve écart-type de beta1


solve(vcov(PK.nls))   # pour calculer l'inverse de la matrice de covariance.
# beta1     beta2
# beta1 15035.2863 257.304168
# beta2   257.3042   9.064683
# info de Fisher observée


# estimation de sigma2
summary(PK.nls)$sigma^2 ## donne la valeur estimée de la variance 
sum((summary(PK.nls)$residuals)^2)/(n-2) # idem
# ---------------------------------------------------------------------------
# prédiction
# ---------------------------------------------------------------------------
predict(PK.nls,newdata=data.frame(time=8))  # avec predict
freg(coef(PK.nls),8) ##via la formule

# graphe des résidus

residual.nls <- resid(PK.nls)/sd(resid(PK.nls))
par(mfrow=c(2,2))
plot(predict(PK.nls), y)
abline(a=0, b=1, lty=1, col="magenta")
plot(time, residual.nls,ylim=c(-2,2))
abline(a=0, b=0, lty=1, col="magenta")
plot(predict(PK.nls), residual.nls,ylim=c(-2,2))
abline(a=0, b=0, lty=1, col="magenta")
qqnorm(residual.nls)
abline(0,1)


## CI
confint(PK.nls)  # attention méthode fondée sur un "profil de vraisemblance"

confint.default(PK.nls) # IC fondés sur l'approx. par la loi asymptotique
#          2.5 %    97.5 %
#   beta1 0.113568 0.1581483
#   beta2 7.674387 9.4899963

# formule du cours, idem ci-dessus
cbind(coef(PK.nls)-qnorm(.975)*sqrt(diag(Var)), coef(PK.nls) + qnorm(.975)*sqrt(diag(Var)))
# [,1]      [,2]
# theta1 0.113568 0.1581483
# theta2 7.674387 9.4899963



