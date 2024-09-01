#############################################################################

tumeur=read.table("mammographic.data.txt",header=T)

str(tumeur) # Type des covariables
table(tumeur$Y) # les nb de cas positifs/négatifs sont équilibrés
table(tumeur$shape)
hist(tumeur$age)
boxplot(age~Y,data=tumeur)

########################################
# Estimation avec age unique covariable continue

t0.glm=glm(Y~age,data=tumeur,family=binomial)
summary(t0.glm)

# prediction de la probabilité d'une tumeur maligne à 30 ans:
predict(t0.glm,data.frame(age=30),type="response")
exp(coef(t0.glm)[1]+coef(t0.glm)[2]*30)/
        (1+exp(coef(t0.glm)[1]+coef(t0.glm)[2]*30)) # idem
# x--> exp(x)/(1+exp(x)) est l'inverse du lien logit

# graphe des observés-ajustés
plot(Y~age,data=tumeur,main="P(Y=1) estimée")
curve(predict(t0.glm,data.frame(age=x),type="response"),add=T)

########################################
# les autres covariables sont discrètes : facteurs dans R

tumeur$shape=factor(tumeur$shape)
tumeur$margin=factor(tumeur$margin)
tumeur$density=factor(tumeur$density)
tumeur$Y=factor(tumeur$Y)

# estimation du modèle complet
t.glm=glm(Y~.,data=tumeur, family=binomial)
summary(t.glm)

# dimension du modèle = 12 paramètes (identifiables)
# ddl du modèle = length(tumeur$Y)-12


#######################################
IC_age <- c(0.054552-qnorm(0.975)*0.007812, 0.054552+qnorm(0.975)*0.007812)
IC_age  ### [1] 0.03924076 0.06986324
# la p-value montre que la variable age est significativement différente de zéro. De plus son IC ne contient la vaieur zéro
model_sans_shape <- glm(Y~.-shape,family = binomial,data = tumeur)
summary(model_sans_shape)
## test de rapport de vraisemblance
trv <- 2*(logLik(model_sans_shape)-logLik(t.glm))
abs(trv)   ## 'log Lik.' 35.65538 (df=9)
qchisq(0.95,3)  ## [1] 2.353363
# la abs(trv) > c_alpha donc on rejet H0 par conséquent shape est significativement différent de zéro
anova(model_sans_shape,t.glm)
df <- attr(logLik(t.glm), "df") - attr(logLik(model_sans_shape), "df")  # Degré de liberté
p_value <- (1-pchisq(abs(trv),3))  # Calcul de la p-valeur

cat("Statistique de test TRV :", trv, "\n")
cat("Degré de liberté :", df, "\n")
cat("P-valeur :", p_value, "\n")
