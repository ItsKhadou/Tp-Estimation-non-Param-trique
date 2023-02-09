############

#TP Estimation Non Parametrique
##Partie 01

#01 Loi discrete finie
#a) Generer un échantillon par simulation
#loi binomiale de paramètre (5,0.4)

p= 0.4 #probabilite de reussir 
n=5 #nombre d'experience(essaies)

#Initialiser la taille de chaque echantillon
N1=50    
N2=100   
N3=500   
N4=1000

S1=rbinom(N1,5,0.4)
S2=rbinom(N2,5,0.4)
S3=rbinom(N3,5,0.4)
S4=rbinom(N4,5,0.4)


#les frequences empiriques
fe1=table(S1)/N1
fe2=table(S2)/N2
fe3=table(S3)/N3
fe4=table(S4)/N4
fe4

#Fonctions de repartition empiriques
#pour ce faire on utilise le package stats
install.packages("stats")
library(stats)

ecdf_1 <- ecdf(S1)
ecdf_2 <- ecdf(S2)
ecdf_3 <- ecdf(S3)
ecdf_4 <- ecdf(S4)

#REPRESENTATION GRAPHIQUES DES RESULTATS
par(mfrow=c(1,2))
#1er echantillon
hist(S1, main = paste("La taille de l'échantillon", N1), col = "lightblue")
points(names(fe1), fe1, type = "s", col = "red")
plot(ecdf_1, main = paste("Sample size =", N1), xlab = "S1", ylab = "ECDF")
curve(pbinom(x, 5, 0.4), col = "blue", add = TRUE)

#2eme echantillon
hist(S2, main = paste("La taille de l'échantillon =", N2),col = "lightyellow")
points(names(fe2), fe2, type = "h", col = "red")
plot(ecdf_2, main = paste("Sample size =", N2), xlab = "x", ylab = "ECDF")
curve(pbinom(x, 5, 0.4), col = "yellow", add = TRUE)
#3eme echantillon
hist(S3, main = paste("La taille de l'échantillon =", N3),col = "lightpink")
points(names(fe3), fe3, type = "h", col = "red")
plot(ecdf_3, main = paste("Sample size =", N3), xlab = "x", ylab = "ECDF")
curve(pbinom(x, 5, 0.4), col = "pink", add = TRUE)

#4eme echantillon
hist(S4, main = paste("La taille de l'échantillon =", N4),col = "lightgreen")
points(names(fe4), fe4, type = "h", col = "red")
plot(ecdf_4, main = paste("Sample size =", N4), xlab = "x", ylab = "CDF")
curve(pbinom(x, 5, 0.4), col = "green", add = TRUE)

########
############
#02 Loi absolument continue

#c) un échantillon par simulation
#Loi Exponentielle
#le parametre lambda egale a 1 par defaut
#lambda = 1
e1<- rexp(N1)
e2<- rexp(N2)
e3<- rexp(N3)
e4<- rexp(N4)

#d) Tracer les histogramme en faisant l'amplitude des classes
par(mfrow=c(1,4))
breaks <- seq(0, 5, length.out = 11)


#Sturges
hist(e1,
     main = "Histogram of exponential data N=50",
     xlab = "Classes (Sturges)",
     ylab = "Frequency",
     col = "#B2182B",
     border = "black",
     breaks ="Sturges")
hist(e1,
     main = "Histogram of exponential data N=50",
     xlab = "Classes (FD)",
     ylab = "Frequency",
     col = "#D6604D",
     border = "black",
     breaks = "FD")
hist(e1,
     main = "Histogram of exponential data N=50",
     xlab = "Classes",
     ylab = "Frequency",
     col = "#FF9966",
     border = "black",
     breaks = breaks)


hist(e1,
     main = "Histogram of exponential data N=50",
     xlab = "Classes (Scott)",
     ylab = "Frequency",
     col = "#FDDBC7",
     border = "black",
     breaks = "Scott")



breaks <- seq(0, 7, length.out = 11)
hist(e2,
     main = "Histogram of exponential data N=100",
     xlab = "Classes (Sturges)",
     ylab = "Frequency",
     col = "#B2182B",
     border = "black",
     breaks ="Sturges")
hist(e2,
     main = "Histogram of exponential data N=100",
     xlab = "Classes (FD)",
     ylab = "Frequency",
     col = "#D6604D",
     border = "black",
     breaks = "FD")
hist(e2,
     main = "Histogram of exponential data N=100",
     xlab = "Classes",
     ylab = "Frequency",
     col = "#FF9966",
     border = "black",
     breaks = breaks)


hist(e2,
     main = "Histogram of exponential data N=100",
     xlab = "Classes (Scott)",
     ylab = "Frequency",
     col = "#FDDBC7",
     border = "black",
     breaks = "Scott")

breaks <- seq(0, 7, length.out = 11)
hist(e3,
     main = "Histogram of exponential data N=500",
     xlab = "Classes (Sturges)",
     ylab = "Frequency",
     col = "#B2182B",
     border = "black",
     breaks ="Sturges")
hist(e3,
     main = "Histogram of exponential data N=500",
     xlab = "Classes (FD)",
     ylab = "Frequency",
     col = "#D6604D",
     border = "black",
     breaks = "FD")
hist(e3,
     main = "Histogram of exponential data N=500",
     xlab = "Classes",
     ylab = "Frequency",
     col = "#FF9966",
     border = "black",
     breaks = breaks)


hist(e3,
     main = "Histogram of exponential data N=500",
     xlab = "Classes (Scott)",
     ylab = "Frequency",
     col = "#FDDBC7",
     border = "black",
     breaks = "Scott")

breaks <- seq(0, 7, length.out = 11)
hist(e4,
     main = "Histogram of exponential data N=1000",
     xlab = "Classes (Sturges)",
     ylab = "Frequency",
     col = "#B2182B",
     border = "black",
     breaks ="Sturges")
hist(e4,
     main = "Histogram of exponential data N=1000",
     xlab = "Classes (FD)",
     ylab = "Frequency",
     col = "#D6604D",
     border = "black",
     breaks = "FD")
hist(e4,
     main = "Histogram of exponential data N=1000",
     xlab = "Classes",
     ylab = "Frequency",
     col = "#FF9966",
     border = "black",
     breaks = breaks)


hist(e4,
     main = "Histogram of exponential data N=1000",
     xlab = "Classes (Scott)",
     ylab = "Frequency",
     col = "#FDDBC7",
     border = "black",
     breaks = "Scott")




#question e
#estimateur par histogramme
par(mfrow=c(1,4))
hist(e1, main = "Estimateur par histogramme de la loi expo N=50",
     xlab = "x", ylab = "Densité", freq = FALSE, col = "#CC99FF")
d <- density(e1)
lines(d, col = "red", lwd = 2, lty = 1)
hist(e2, main = "Estimateur par histogramme de la loi expo N=100",
     xlab = "x", ylab = "Densité", freq = FALSE,col = "#9966CC")
d <- density(e2)
lines(d, col = "red", lwd = 2, lty = 1)
hist(e3, main = "Estimateur par histogramme de la loi expo N=500",
     xlab = "x", ylab = "Densité", freq = FALSE,col = "#663399")
d <- density(e3)
lines(d, col = "red", lwd = 2, lty = 1)
hist(e4, main = "Estimateur par histogramme de la loi expo N=1000",
     xlab = "x", ylab = "Densité", freq = FALSE, col = "#330066")
d <- density(e4)
lines(d, col = "red", lwd = 2, lty = 1)

#estimateur a noyau de la densité
#N=50
# Tracé de l'estimateur à noyau avec un noyau gaussien et une fenêtre de lissage de 0.2
plot(density(e1, kernel = "gaussian", bw = 0.2),
     main = "Estimateur à noyau avec un noyau normal et un noyau Epanechinkov",
     xlab = "Valeurs", ylab = "Densité relative", col="black", lwd=2)

# Tracé de l'estimateur à noyau avec un noyau normal et une fenêtre de lissage de 0.5
lines(density(e1, kernel = "gaussian", bw = 0.5), col = "red", lwd = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.2
lines(density(e1, kernel = "epanechnikov", bw = 0.2), col = "blue", lwd = 2, lty = 2)
?lines
# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.5
lines(density(e1, kernel = "epanechnikov", bw = 0.5), col = "green", lwd = 2, lty = 2)
legend("topright", c("Noyau normal, fenêtre 0.2", "Noyau normal, fenêtre 0.5",
                     "Noyau épanechnikov, fenêtre 0.2", "Noyau épanechnikov, fenêtre 0.5"),
       col = c("black", "red", "blue", "green"), lty = c(1,1,2,2), lwd = 1, cex = 0.75)

#N=100
# Tracé de l'estimateur à noyau avec un noyau gaussien et une fenêtre de lissage de 0.2
plot(density(e2, kernel = "gaussian", bw = 0.2), 
     main = "Estimateur à noyau avec un noyau normal et un noyau Epanechinkov",
     xlab = "Valeurs", ylab = "Densité relative", col="black", lwd=2)

# Tracé de l'estimateur à noyau avec un noyau normal et une fenêtre de lissage de 0.5
lines(density(e2, kernel = "gaussian", bw = 0.5), col = "red", lwd = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.2
lines(density(e2, kernel = "epanechnikov", bw = 0.2), col = "blue", lwd = 2, lty = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.5
lines(density(e2, kernel = "epanechnikov", bw = 0.5), col = "green", lwd = 2, lty = 2)
legend("topright", c("Noyau normal, fenêtre 0.2", "Noyau normal, fenêtre 0.5",
                     "Noyau épanechnikov, fenêtre 0.2", "Noyau épanechnikov, fenêtre 0.5"),
       col = c("black", "red", "blue", "green"), lty = c(1,1,2,2), lwd = 1, cex = 0.75)

#N=500
# Tracé de l'estimateur à noyau avec un noyau gaussien et une fenêtre de lissage de 0.2
plot(density(e3, kernel = "gaussian", bw = 0.2), 
     main = "Estimateur à noyau avec un noyau normal et un noyau Epanechinkov",
     xlab = "Valeurs", ylab = "Densité relative", col="black", lwd=2)

# Tracé de l'estimateur à noyau avec un noyau normal et une fenêtre de lissage de 0.5
lines(density(e3, kernel = "gaussian", bw = 0.5), col = "red", lwd = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.2
lines(density(e3, kernel = "epanechnikov", bw = 0.2), col = "blue", lwd = 2, lty = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.5
lines(density(e3, kernel = "epanechnikov", bw = 0.5), col = "green", lwd = 2, lty = 2)
legend("topright", c("Noyau normal, fenêtre 0.2", "Noyau normal, fenêtre 0.5",
                     "Noyau épanechnikov, fenêtre 0.2", "Noyau épanechnikov, fenêtre 0.5"),
       col = c("black", "red", "blue", "green"), lty = c(1,1,2,2), lwd = 1, cex = 0.75)


#N=1000
# Tracé de l'estimateur à noyau avec un noyau gaussien et une fenêtre de lissage de 0.2
plot(density(e4, kernel = "gaussian", bw = 0.02), 
     main = "Estimateur à noyau avec un noyau normal et un noyau Epanechinkov",
     xlab = "Valeurs", ylab = "Densité relative", col="black", lwd=2)

# Tracé de l'estimateur à noyau avec un noyau normal et une fenêtre de lissage de 0.5
lines(density(e4, kernel = "gaussian", bw = 0.5), col = "red", lwd = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.2
lines(density(e4, kernel = "epanechnikov", bw = 0.2), col = "blue", lwd = 2, lty = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.5
lines(density(e4, kernel = "epanechnikov", bw = 0.5), col = "green", lwd = 2, lty = 2)
legend("topright", c("Noyau normal, fenêtre 0.2", "Noyau normal, fenêtre 0.5",
                     "Noyau épanechnikov, fenêtre 0.2", "Noyau épanechnikov, fenêtre 0.5"),
       col = c("black", "red", "blue", "green"), lty = c(1,1,2,2), lwd = 1, cex = 0.7)




#question f
x <- seq(0, 10, by = 0.1)
y <- dexp(x)
plot(x, y, type = "l", lwd = 2, col = "black", xlab = "x", ylab = "density",
     main = paste("Exponential density"))
lines(density(e1, kernel = "gaussian", bw = 0.2), col = "red", lwd = 2)
lines(density(e1, kernel = "gaussian", bw = 0.5), col = "orange", lwd = 2)
lines(density(e1, kernel = "epanechnikov", bw = 0.2), col = "blue", lwd = 2, lty = 2)
lines(density(e1, kernel = "epanechnikov", bw = 0.5), col = "green", lwd = 2, lty = 2)
legend("topright", c("Densité Théorique", "Noyau normal, fenêtre 0.2",
                     "Noyau Gaussien, fenêtre 0.5", "Noyau épanechnikov, fenêtre 0.2", "Noyau épanechnikov, fenêtre 0.5"),
       col = c("black", "orange","red", "blue", "green"), lty = c(1,2,2,2,2), lwd = 1, cex = 0.75)
########

plot(x, y, type = "l", lwd = 2, col = "black", xlab = "x", ylab = "density",
     main = paste("Exponential density"))
lines(density(e2, kernel = "gaussian", bw = 0.2), col = "red", lwd = 2)
lines(density(e2, kernel = "gaussian", bw = 0.5), col = "orange", lwd = 2)
lines(density(e2, kernel = "epanechnikov", bw = 0.2), col = "blue", lwd = 2, lty = 2)
lines(density(e2, kernel = "epanechnikov", bw = 0.5), col = "green", lwd = 2, lty = 2)
legend("topright", c("Densité Théorique", "Noyau normal, fenêtre 0.2",
                     "Noyau Gaussien, fenêtre 0.5", "Noyau épanechnikov, fenêtre 0.2", "Noyau épanechnikov, fenêtre 0.5"),
       col = c("black", "orange","red", "blue", "green"), lty = c(1,2,2,2,2), lwd = 1, cex = 0.75)
########

plot(x, y, type = "l", lwd = 2, col = "black", xlab = "x", ylab = "density",
     main = paste("Exponential density"))
lines(density(e3, kernel = "gaussian", bw = 0.2), col = "red", lwd = 2)
lines(density(e3, kernel = "gaussian", bw = 0.5), col = "orange", lwd = 2)
lines(density(e3, kernel = "epanechnikov", bw = 0.2), col = "blue", lwd = 2, lty = 2)
lines(density(e3, kernel = "epanechnikov", bw = 0.5), col = "green", lwd = 2, lty = 2)
legend("topright", c("Densité Théorique", "Noyau normal, fenêtre 0.2",
                     "Noyau Gaussien, fenêtre 0.5", "Noyau épanechnikov, fenêtre 0.2", "Noyau épanechnikov, fenêtre 0.5"),
       col = c("black", "orange","red", "blue", "green"), lty = c(1,2,2,2,2), lwd = 1, cex = 0.75)
########

plot(x, y, type = "l", lwd = 2, col = "black", xlab = "x", ylab = "density",
     main = paste("Exponential density"))
lines(density(e4, kernel = "gaussian", bw = 0.2), col = "red", lwd = 2)
lines(density(e4, kernel = "gaussian", bw = 0.5), col = "orange", lwd = 2)
lines(density(e4, kernel = "epanechnikov", bw = 0.2), col = "blue", lwd = 2, lty = 2)
lines(density(e4, kernel = "epanechnikov", bw = 0.5), col = "green", lwd = 2, lty = 2)
legend("topright", c("Densité Théorique", "Noyau normal, fenêtre 0.2",
                     "Noyau Gaussien, fenêtre 0.5", "Noyau épanechnikov, fenêtre 0.2", "Noyau épanechnikov, fenêtre 0.5"),
       col = c("black", "orange","red", "blue", "green"), lty = c(1,2,2,2,2), lwd = 1, cex = 0.75)



#question G
#echantillon de taille N=50
plot(x, y, type = "l", lwd = 2, col = "black", xlab = "x", ylab = "density",
     main = paste("Exponential density N=50"))
lines(density(e1, kernel = "gaussian", bw = 0.4), col = "red", lwd = 2)
lines(density(e1, kernel = "gaussian", bw = 0.7), col = "orange", lwd = 2)
lines(density(e1, kernel = "epanechnikov", bw = 0.3), col = "blue", lwd = 2, lty = 2)
lines(density(e1, kernel = "epanechnikov", bw = 0.6), col = "green", lwd = 2, lty = 2)
legend("topright", c("Densité Théorique", "Noyau normal, fenêtre 0.4",
                     "Noyau Gaussien, fenêtre 0.7", "Noyau épanechnikov, fenêtre 0.3", "Noyau épanechnikov, fenêtre 0.7"),
       col = c("black", "orange","red", "blue", "green"), lty = c(1,2,2,2,2), lwd = 1, cex = 0.75)
########
#echantillon de taille N=100
plot(x, y, type = "l", lwd = 2, col = "black", xlab = "x", ylab = "density",
     main = paste("Exponential density N=100"))
lines(density(e1, kernel = "gaussian", bw = 0.4), col = "red", lwd = 2)
lines(density(e1, kernel = "gaussian", bw = 0.7), col = "orange", lwd = 2)
lines(density(e1, kernel = "epanechnikov", bw = 0.3), col = "blue", lwd = 2, lty = 2)
lines(density(e1, kernel = "epanechnikov", bw = 0.6), col = "green", lwd = 2, lty = 2)
legend("topright", c("Densité Théorique", "Noyau normal, fenêtre 0.4",
                     "Noyau Gaussien, fenêtre 0.7", "Noyau épanechnikov, fenêtre 0.3", "Noyau épanechnikov, fenêtre 0.7"),
       col = c("black", "orange","red", "blue", "green"), lty = c(1,2,2,2,2), lwd = 1, cex = 0.75)
########
#echantillon de taille N=500
plot(x, y, type = "l", lwd = 2, col = "black", xlab = "x", ylab = "density",
     main = paste("Exponential density N=500"))
lines(density(e1, kernel = "gaussian", bw = 0.4), col = "red", lwd = 2)
lines(density(e1, kernel = "gaussian", bw = 0.7), col = "orange", lwd = 2)
lines(density(e1, kernel = "epanechnikov", bw = 0.3), col = "blue", lwd = 2, lty = 2)
lines(density(e1, kernel = "epanechnikov", bw = 0.6), col = "green", lwd = 2, lty = 2)
legend("topright", c("Densité Théorique", "Noyau normal, fenêtre 0.4",
                     "Noyau Gaussien, fenêtre 0.7", "Noyau épanechnikov, fenêtre 0.3", "Noyau épanechnikov, fenêtre 0.7"),
       col = c("black", "orange","red", "blue", "green"), lty = c(1,2,2,2,2), lwd = 1, cex = 0.75)
########
#echantillon de taille N=1000
plot(x, y, type = "l", lwd = 2, col = "black", xlab = "x", ylab = "density",
     main = paste("Exponential density N=1000"))
lines(density(e1, kernel = "gaussian", bw = 0.4), col = "red", lwd = 2)
lines(density(e1, kernel = "gaussian", bw = 0.7), col = "orange", lwd = 2)
lines(density(e1, kernel = "epanechnikov", bw = 0.3), col = "blue", lwd = 2, lty = 2)
lines(density(e1, kernel = "epanechnikov", bw = 0.6), col = "green", lwd = 2, lty = 2)
legend("topright", c("Densité Théorique", "Noyau normal, fenêtre 0.4",
                     "Noyau Gaussien, fenêtre 0.7", "Noyau épanechnikov, fenêtre 0.3", "Noyau épanechnikov, fenêtre 0.7"),
       col = c("black", "orange","red", "blue", "green"), lty = c(1,2,2,2,2), lwd = 1, cex = 0.75)




###############################################################################
#Loi Normale
g1<- rnorm(N1)
g2<- rnorm(N2)
g3<- rnorm(N3)
g4<- rnorm(N4)

#d) Tracer les histogramme en faisant l'amplitude des classes
par(mfrow=c(1,3))

#Sturges
hist(g1,
     main = "Histogram of Gaussian data N=50",
     xlab = "Classes (Sturges)",
     ylab = "Frequency",
     col = "#B2182B",
     border = "black",
     breaks ="Sturges")
hist(g1,
     main = "Histogram of Gaussian data N=50",
     xlab = "Classes (FD)",
     ylab = "Frequency",
     col = "#D6604D",
     border = "black",
     breaks = "FD")

hist(g1,
     main = "Histogram of Gaussian data N=50",
     xlab = "Classes (Scott)",
     ylab = "Frequency",
     col = "#FDDBC7",
     border = "black",
     breaks = "Scott")



hist(g2,
     main = "Histogram of Gaussian data N=100",
     xlab = "Classes (Sturges)",
     ylab = "Frequency",
     col = "#B2182B",
     border = "black",
     breaks ="Sturges")
hist(g2,
     main = "Histogram of Gaussian data N=100",
     xlab = "Classes (FD)",
     ylab = "Frequency",
     col = "#D6604D",
     border = "black",
     breaks = "FD")

hist(g2,
     main = "Histogram of Gaussian data N=100",
     xlab = "Classes (Scott)",
     ylab = "Frequency",
     col = "#FDDBC7",
     border = "black",
     breaks = "Scott")

hist(g3,
     main = "Histogram of Gaussian data N=500",
     xlab = "Classes (Sturges)",
     ylab = "Frequency",
     col = "#B2182B",
     border = "black",
     breaks ="Sturges")
hist(g3,
     main = "Histogram of Gaussian data N=500",
     xlab = "Classes (FD)",
     ylab = "Frequency",
     col = "#D6604D",
     border = "black",
     breaks = "FD")

hist(g3,
     main = "Histogram of Gaussian data N=500",
     xlab = "Classes (Scott)",
     ylab = "Frequency",
     col = "#FDDBC7",
     border = "black",
     breaks = "Scott")


hist(g4,
     main = "Histogram of Gaussian data N=1000",
     xlab = "Classes (Sturges)",
     ylab = "Frequency",
     col = "#B2182B",
     border = "black",
     breaks ="Sturges")
hist(g4,
     main = "Histogram of Gaussian data N=1000",
     xlab = "Classes (FD)",
     ylab = "Frequency",
     col = "#D6604D",
     border = "black",
     breaks = "FD")


hist(g4,
     main = "Histogram of Gaussian data N=1000",
     xlab = "Classes (Scott)",
     ylab = "Frequency",
     col = "#FDDBC7",
     border = "black",
     breaks = "Scott")

#question e
#estimateur par histogramme
par(mfrow=c(1,4))
hist(g1, main = "Estimateur par histogramme de la loi expo N=50",
     xlab = "x", ylab = "Densité", freq = FALSE, col = "#CC99FF")
d <- density(g1)
lines(d, col = "red", lwd = 2, lty = 1)
hist(g2, main = "Estimateur par histogramme de la loi expo N=100",
     xlab = "x", ylab = "Densité", freq = FALSE,col = "#9966CC")
d <- density(g2)
lines(d, col = "red", lwd = 2, lty = 1)
hist(g3, main = "Estimateur par histogramme de la loi expo N=500",
     xlab = "x", ylab = "Densité", freq = FALSE,col = "#663399")
d <- density(g3)
lines(d, col = "red", lwd = 2, lty = 1)
hist(g4, main = "Estimateur par histogramme de la loi expo N=1000",
     xlab = "x", ylab = "Densité", freq = FALSE, col = "#330066")
d <- density(g4)
lines(d, col = "red", lwd = 2, lty = 1)


#estimateur a noyau de la densité
#N=50
# Tracé de l'estimateur à noyau avec un noyau gaussien et une fenêtre de lissage de 0.2
plot(density(g1, kernel = "quarter-circle", bw = 0.2),
     main = "Estimateur à noyau avec un noyau normal et un noyau Epanechinkov",
     xlab = "Valeurs", ylab = "Densité relative", col="black", lwd=2)

# Tracé de l'estimateur à noyau avec un noyau normal et une fenêtre de lissage de 0.5
lines(density(g1, kernel = "gaussian", bw = 0.5), col = "red", lwd = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.2
lines(density(g1, kernel = "epanechnikov", bw = 0.2), col = "blue", lwd = 2, lty = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.5
lines(density(g1, kernel = "epanechnikov", bw = 0.5), col = "green", lwd = 2, lty = 2)
legend("topright", c("Noyau normal, fenêtre 0.2", "Noyau normal, fenêtre 0.5",
                     "Noyau épanechnikov, fenêtre 0.2", "Noyau épanechnikov, fenêtre 0.5"),
       col = c("black", "red", "blue", "green"), lty = c(1,1,2,2), lwd = 1, cex = 0.5)

#N=100
# Tracé de l'estimateur à noyau avec un noyau gaussien et une fenêtre de lissage de 0.2
plot(density(g2, kernel = "gaussian", bw = 0.2), 
     main = "Estimateur à noyau avec un noyau normal et un noyau Epanechinkov",
     xlab = "Valeurs", ylab = "Densité relative", col="black", lwd=2)

# Tracé de l'estimateur à noyau avec un noyau normal et une fenêtre de lissage de 0.5
lines(density(g2, kernel = "gaussian", bw = 0.5), col = "red", lwd = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.2
lines(density(g2, kernel = "epanechnikov", bw = 0.2), col = "blue", lwd = 2, lty = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.5
lines(density(g2, kernel = "epanechnikov", bw = 0.5), col = "green", lwd = 2, lty = 2)
legend("topright", c("Noyau normal, fenêtre 0.2", "Noyau normal, fenêtre 0.5",
                     "Noyau épanechnikov, fenêtre 0.2", "Noyau épanechnikov, fenêtre 0.5"),
       col = c("black", "red", "blue", "green"), lty = c(1,1,2,2), lwd = 1, cex = 0.75)

#N=500
# Tracé de l'estimateur à noyau avec un noyau gaussien et une fenêtre de lissage de 0.2
plot(density(g3, kernel = "gaussian", bw = 0.2), 
     main = "Estimateur à noyau avec un noyau normal et un noyau Epanechinkov",
     xlab = "Valeurs", ylab = "Densité relative", col="black", lwd=2)

# Tracé de l'estimateur à noyau avec un noyau normal et une fenêtre de lissage de 0.5
lines(density(g3, kernel = "gaussian", bw = 0.5), col = "red", lwd = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.2
lines(density(g3, kernel = "epanechnikov", bw = 0.2), col = "blue", lwd = 2, lty = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.5
lines(density(g3, kernel = "epanechnikov", bw = 0.5), col = "green", lwd = 2, lty = 2)
legend("topright", c("Noyau normal, fenêtre 0.2", "Noyau normal, fenêtre 0.5",
                     "Noyau épanechnikov, fenêtre 0.2", "Noyau épanechnikov, fenêtre 0.5"),
       col = c("black", "red", "blue", "green"), lty = c(1,1,2,2), lwd = 1, cex = 0.75)


#N=1000
# Tracé de l'estimateur à noyau avec un noyau gaussien et une fenêtre de lissage de 0.2
plot(density(g4, kernel = "gaussian", bw = 0.2), 
     main = "Estimateur à noyau avec un noyau normal et un noyau Epanechinkov",
     xlab = "Valeurs", ylab = "Densité relative", col="black", lwd=2)

# Tracé de l'estimateur à noyau avec un noyau normal et une fenêtre de lissage de 0.5
lines(density(g4, kernel = "gaussian", bw = 0.5), col = "red", lwd = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.2
lines(density(g4, kernel = "epanechnikov", bw = 0.2), col = "blue", lwd = 2, lty = 2)

# Tracé de l'estimateur à noyau avec un noyau épanechnikov et une fenêtre de lissage de 0.5
lines(density(g4, kernel = "epanechnikov", bw = 0.5), col = "green", lwd = 2, lty = 2)
legend("topright", c("Noyau normal, fenêtre 0.2", "Noyau normal, fenêtre 0.5",
                     "Noyau épanechnikov, fenêtre 0.2", "Noyau épanechnikov, fenêtre 0.5"),
       col = c("black", "red", "blue", "green"), lty = c(1,1,2,2), lwd = 1, cex = 0.7)




