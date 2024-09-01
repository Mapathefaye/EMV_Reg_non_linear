
# ------------- régression logistique
########################################"
mommo <- read.table(file="mammographic.data.txt", header=T,sep=" ")
head(mommo)

# 1) Analyse exploratoire des données

summary(mommo)
table(mommo$Y) ## résumé de y
str(mommo) ## donne le code des variables
mommo$shape=factor(mommo$shape)
mommo$margin=factor(mommo$margin)
mommo$density=factor(mommo$density)
mommo$Y=factor(mommo$Y)
## shape avec 4 modalités, margin avec 5 modalités, density avec 4 modalités et Y avec 2 modalités
# on peut faire un boxplot de l'age en fct de Y. les autres représentions ne sont pas pertinantes
boxplot(age~Y, data=mommo)

# 2) 
?glm
to_glm = glm(formula = Y ~ age, family = binomial, data=mommo)
summary(to_glm)
proba_tumeur_maligne = exp(-4.421874 + 0.077628 * 30)/(1+exp(-4.421874 + 0.077628 * 30))
predict.glm(to_glm,data.frame(age=30), type="response") ## donne la prédiction de la présence de tumeur maligne à 30 ans

plot(Y~age, data = mommo) ## il ne faut pas mettre y en factor 

curve(predict(to_glm,data.frame(age=x),type ="response",add=T))

## quelle est la dimension du modèles
table(mommo$shape)
summary(mommo)
t_glm = glm(formula = Y~., family = binomial,data = mommo)
summary(t_glm) ## shape1, margin1 et density1 ont disparus

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 1149.9  on 829  degrees of freedom  ====> modèle constant Y~1
#Residual deviance:  724.5  on 818  degrees of freedom =====> modèle complet
#AIC: 748.5

