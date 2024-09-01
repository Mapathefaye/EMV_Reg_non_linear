data <- read.table(file="mammographic.data.txt", header=T,sep=" ")
head(data)
data$shape=factor(data$shape)
data$margin=factor(data$margin)
data$density=factor(data$density)
data$Y=factor(data$Y)

glm = glm(formula = Y~., family = binomial,data = data)
summary(glm)
logLik(glm)   #'log Lik.' -362.2512
# la variable shape est-elle sigificative dans le modèle complet
glm_sans_shape = glm(formula = Y~age+margin+density, family = binomial,data = data)
## glm_sans_shape = glm(formula = Y~.-shape, family = binomial,data = data)
summary(glm_sans_shape)
names(glm_sans_shape)
logLik(glm_sans_shape)  #'log Lik.' -380.0789
TRV=2*(logLik(glm)-logLik(glm_sans_shape))
TRV

p_value=1-pchisq(TRV,3)
## rejet de H0 donc shape significative
glm_sans_density = glm(formula = Y~age+shape+margin, family = binomial,data = data)
summary(glm_sans_density)
TRV=2*(logLik(glm)-logLik(glm_sans_density))
TRV
p_value=1-pchisq(TRV,3)
## on ne rejet parH0 donc density non significative
anova(glm_sans_density, glm)
anova(glm_sans_density, glm, test = "LRT")
anova(glm_sans_density, glm, test = "Chisq")
anova(glm_sans_shape, glm, test = "LRT")
### meileur model : age + shape + margin

### prédiction
new_data <- data.frame(age = 50, shape = "3", margin = "1", density = "1")
predicted_prob <- predict(glm_sans_density, newdata = new_data, type = "response")
print(predicted_prob)
## sans type on retrouve xB
## avec type on retrouve P(y=1)
## IC
xbeta=predict(glm_sans_density, newdata = new_data, type = "link");xbeta
v = vcov(glm_sans_density)
v
sdbeta=sqrt(diag(v))
sdbeta
xbeta=predict(glm_sans_density, newdata = new_data, type = "link", se.fit = T);xbeta
ICxbeta=c(xbeta$fit-qnorm(0.975)*xbeta$fit,xbeta$fit+qnorm(0.975)*xbeta$fit)
ICxbeta ##   1.409808 -4.347020 
confidence_interval <- predict(glm_sans_density, newdata = new_data, interval = "confidence", level = 0.95)
print(confidence_interval)
