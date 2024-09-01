# Fixer la graine aléatoire pour la reproductibilité
set.seed(509)

# Générer un échantillon y de taille 20 de loi géométrique de paramètre p = 0.3
y <- rgeom(20, prob = 0.3)

# Transformer y en un facteur yrep
yrep <- factor(y)

# Normaliser en densité
densite_y <- table(y) / length(y)

# Définir la fonction de log-vraisemblance
log_vraisemblance <- function(p) {
  sum(dbinom(y, size = 1, prob = p, log = TRUE))
}

# Vectoriser la log-vraisemblance
vectorise_log_vraisemblance <- Vectorize(log_vraisemblance)

# Tracer la log-vraisemblance
curve(vectorise_log_vraisemblance, from = 0, to = 1, xlab = "p", ylab = "Log-vraisemblance")

# Calculer l'estimateur du maximum de vraisemblance de p avec optim
estimation_p <- optim(par = 0.5, fn = function(p) -log_vraisemblance(p), method = "Brent", lower = 0, upper = 1)$par

# Afficher l'estimation de p
print(estimation_p)

# Calcul de l'écart-type de l'estimateur
# Pour cela, nous pouvons utiliser la méthode de la matrice d'information de Fisher inverse
# La matrice d'information de Fisher est l'espérance du double de la dérivée seconde de la log-vraisemblance par rapport à p
# L'écart-type de l'estimateur du maximum de vraisemblance est alors l'inverse de la racine carrée de cette valeur.

# Calcul de la dérivée seconde de la log-vraisemblance par rapport à p
second_derivative <- function(p) {
  -sum(dbinom(y, size = 1, prob = p, log = FALSE)^2 / (p^2 * (1 - p)^2))
}

# Calcul de la matrice d'information de Fisher inverse
Fisher_inverse <- 1 / second_derivative(estimation_p)

# Calcul de l'écart-type de l'estimateur
ecart_type_estimation <- sqrt(Fisher_inverse)

# Afficher l'écart-type de l'estimateur
print(ecart_type_estimation)

# Fixer la graine aléatoire pour la reproductibilité
set.seed(509)

# Générer un échantillon y de taille 20 de loi géométrique de paramètre p = 0.3
y <- rgeom(20, prob = 0.3)

# Transformer y en un facteur yrep
yrep <- factor(y)

# Normaliser en densité
densite_y <- table(y) / length(y)

# Définir la fonction de log-vraisemblance
log_vraisemblance <- function(p) {
  sum(dgeom(y, prob = p, log = TRUE))
}

# Vectoriser la log-vraisemblance
vectorise_log_vraisemblance <- Vectorize(log_vraisemblance)

# Tracer la log-vraisemblance
curve(vectorise_log_vraisemblance, from = 0, to = 1, xlab = "p", ylab = "Log-vraisemblance")

# Calculer l'estimateur du maximum de vraisemblance de p avec optim
estimation_p <- optim(par = 0.5, fn = function(p) -log_vraisemblance(p), method = "Brent", lower = 0, upper = 1)$par

# Afficher l'estimation de p
print(estimation_p)

# Calcul de l'écart-type de l'estimateur
# Pour cela, nous pouvons utiliser la méthode de la matrice d'information de Fisher inverse
# La matrice d'information de Fisher est l'espérance du double de la dérivée seconde de la log-vraisemblance par rapport à p
# L'écart-type de l'estimateur du maximum de vraisemblance est alors l'inverse de la racine carrée de cette valeur.

# Calcul de la dérivée seconde de la log-vraisemblance par rapport à p
second_derivative <- function(p) {
  -sum(dgeom(y, prob = p, log = FALSE)^2 / p^2)
}

# Calcul de la matrice d'information de Fisher inverse
Fisher_inverse <- 1 / second_derivative(estimation_p)

# Calcul de l'écart-type de l'estimateur
ecart_type_estimation <- sqrt(Fisher_inverse)

# Afficher l'écart-type de l'estimateur
print(ecart_type_estimation)

# Calculer la densité de yrep
densite_yrep <- table(yrep) / length(yrep)

# Tracer le diagramme en bâtons de yrep normalisé en densité
barplot(densite_yrep, main = "Diagramme en bâtons de yrep normalisé en densité", xlab = "Valeurs de yrep", ylab = "Densité")

# Paramètre estimé p
p_estime <- estimation_p

# Densité géométrique estimée
densite_geometrique <- dgeom(0:max(y), prob = p_estime)

# Tracer le diagramme en bâtons de yrep normalisé en densité avec la densité géométrique estimée
barplot(densite_yrep, main = "Diagramme en bâtons de yrep normalisé en densité avec densité géométrique estimée", xlab = "Valeurs de yrep", ylab = "Densité")
lines(densite_geometrique, col = "red")

# QQplot de l'échantillon y
qqplot(qgeom(ppoints(length(y)), prob = p_estime), y)
abline(0, 1, col = "blue", lwd = 2)

