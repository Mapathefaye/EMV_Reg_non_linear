# TP3: Estimation d’une régression non-linéaire gaussienne
############################################################################""

# 1. Estimation de la fonction de régression
####### 1) création du data.frame 
time <- c(1,4,7,10,13,16,22,25,28)
yi <- c(9.82,7.35,4.41,3.74,1.30,0.85,0.27,0.23,0.54)
donnee <- data.frame(time,yi)
###### représentation grapgique

plot(donnee$time, donnee$yi, type = "o", col = "blue", 
     xlab = "Temps (heures)", ylab = "Concentration du médicament",
     main = "Concentration du médicament dans le sang en fonction du temps")

###### 2) fonction de régression freg ayant pour arguments le vecteur des paramètres beta=(k, V ) et
###########la covariable time
####beta = c(K,V)
freg <- function(beta, time) {
  K <- beta[1]
  V <- beta[2]
  (100/V) * exp(-K * time)
  
}

##  3) fonction logvrais ayant pour argument le vecteur des paramètres beta=(k, V ) qui calcule
#######la log-vraisemblance des observations.

logvrais <- function(beta, data) {
  y <- donnee$yi
  time <- donnee$time
  y_predict <- freg(beta, time)
  sigma_sq <- sum((y - y_predict)^2) / length(y)
  loglik <- -0.5 * sum((y - y_predict)^2) / sigma_sq - 0.5 * length(y) * log(sigma_sq)
  return(-loglik)
}
##### 4) Utilisez la fonction optim pour estimer les paramètres.
initial_value <- c(1, 0.1)  # valeurs initiales des paramètres V et k
optimisation <- optim(par = initial_value, fn = logvrais, data = donnee)
estimated_params <- optimisation$par
print(estimated_params)
lines(donnee$time, freg(estimated_params, donnee$time), col = "red", lwd = 2)
legend("topright", legend = "Régression estimée", col = "red", lty = 1, lwd = 2)
