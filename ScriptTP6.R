#############################################################################

tumeur=read.table("mammographic.data",header=T)

tumeur$shape=factor(tumeur$shape)
tumeur$margin=factor(tumeur$margin)
tumeur$density=factor(tumeur$density)
tumeur$Y=factor(tumeur$Y)

###################################################################

# estimation du modèle complet
t.glm=glm(Y~.,data=tumeur, family=binomial)
summary(t.glm)

##  IC du paramètre de age dans le modèle complet

IC=c(coef(t.glm)[2]-qnorm(0.975)*sqrt(vcov(t.glm)[2,2]),
     coef(t.glm)[2]+qnorm(0.975)*sqrt(vcov(t.glm)[2,2]))
# niveau 95% 
# 0.03924018 0.06986316

# même résultat que :
confint.default(t.glm,2)

# l'IC ne contient pas 0, la variable age est significative
# l'age augmente le risque d'une tumeur maligne (paramètre>0)

#######################################
# Tests de rapport de vraisemblance

#######################################

# estimation du sous-modèle sans la variable shape

t2.glm=glm(Y~.-shape,data=tumeur, family=binomial)

# test de sous-modèle par rapport de vraisemblance
2*(logLik(t.glm)-logLik(t2.glm))
# 'log Lik.' 35.65538 
qchisq(0.95,3)
# [1] 7.814728
1-pchisq(35.65538,3)
# [1] 8.856051e-08
# on rejette H0: la variable shape est significative
# le sous-modèle n'explique pas aussi bien les données 
# que le modèle complet

anova(t2.glm,t.glm,test="LRT") # idem
anova(t2.glm,t.glm,test="Chisq") # idem

t3.glm=glm(Y~.-margin,data=tumeur, family=binomial)
TRV=2*(logLik(t.glm)-logLik(t3.glm))
# 'log Lik.' 35.17974 (df=12)
1-pchisq(TRV,4)
# 4.266715e-07

# la variable margin est significative

t4.glm=glm(Y~.-density,data=tumeur, family=binomial)
TRV=2*(logLik(t.glm)-logLik(t4.glm))
1-pchisq(TRV,3)
# 0.3380355
anova(t4.glm,t.glm,test="LRT") # idem

# la variable density n'est pas significative


### Prédiction

newdata1 <- data.frame(age=50,shape="3",margin="1",density="1")
pred <-predict(t.glm, newdata = newdata1, type = "response") # 0.3143653 
cbind(newdata1,pred)
#age shape margin density      pred
#1  50     3      1       1 0.3143653


## IC de la prédiction
predl <-predict(t.glm, newdata = newdata1, type = "link",se=TRUE)

# plogis = fonction inverse de logit = exp(x)/(1+exp(x))
cbind(plogis(predl$fit-1.96*predl$se.fit),
      plogis(predl$fit),
      plogis(predl$fit+1.96*predl$se.fit))
#  0.09159837 0.3143653 0.6758347
# la proba est < 0.5 donc on classerait plutôt en tumeur bénigne, 
# mais l'IC est assez large et contient 0.5, donc la conclusion 
# n'est pas nette.

# dans le sous-modèle sans density (3 paramètres estimés en moins)

predict(t4.glm, newdata = newdata1, type = "response") # 0.1871547

pred2 <-predict(t4.glm, newdata = newdata1, type = "link",se=TRUE)
cbind(plogis(pred2$fit-1.96*pred2$se.fit),
      plogis(pred2$fit),
      plogis(pred2$fit+1.96*pred2$se.fit))
# 1 0.1048017 0.1871547 0.3116887
# l'estimation et l'IC sont plus précis : on classe la tumeur comme bénigne