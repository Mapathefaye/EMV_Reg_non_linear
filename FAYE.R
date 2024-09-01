## Mapathé FAYE

# Estimation d'une régression non-linéaire

# 1) 
pk<-read.table(file="PK.txt",header=T)
dim(pk)  # [1] 17  7
plot(pk$temps,pk$concentration,main="Evolution de la concentration",xlab="temps",ylab="concentration")
## commentaire: La courbe indique une relation non-linéaire entre la concentration et le temps

# 2) fonction de régression
freg=function(param,temps) {(75*param[2]*(exp(-param[3]*temps)-exp(-param[2]*temps)))/(param[1]*(param[2]-param[3]))}
# 3) fonction de log vraisemblance
logvrais=function(param) {sum(log(dnorm(pk$concentration,freg(param,pk$temps))))}

# 4) estimation par nlm
nlogvrais=function(param) {-logvrais(param)}
pk.st=c(0,2,1,1)
pk.nlm=nlm(f=nlogvrais,p=pk.st)
pk.nlm
#$minimum
#[1] 1.797693e+308

#$estimate
#[1] 0 2 1 1

#$gradient
#[1] 0 0 0 0

#$code
#[1] 1

#$iterations
#[1] 0

summary(pk.nlm)
# commentaire: l'algorithme convergence avec le code 1 après une itération
#5)
param_est=pk.nlm$estimate
param_est  # [1] 0 2 1 1
curve(freg(param_est,x),add=T,col=2)

#6)
predict(pk.nlm,newdata=data.frame(temps=20))
# 7) estimation de sigma^2
pk.nlm$gradient


# 10)

param0=c(1,1,1,1)
pk.nls=nls(concentration~freg(param,temps),data=pk,start=list(param=param0),trace=T)
pk.nls$control
pk.nls$convinfo
pk.nls$par
