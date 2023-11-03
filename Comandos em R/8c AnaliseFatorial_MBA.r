# SME0822 Análise Multivariada e Aprendizado Não-Supervisionado
# Prof. Cibele Russo - ICMC USP 
# Atualizado em 3/11/2023 com contribuição de Fernando Masumoto (@fernandohitmas)

# Dados MBA Car
# Aula 8c - Análise fatorial 
# --- Leitura dos dados em .csv --- #

dados<-read.csv("https://raw.githubusercontent.com/cibelerusso/AnaliseMultivariadaEAprendizadoNaoSupervisionado/master/Dados/MBA_CAR_ATTRIB.csv", sep=";")
View(dados)

# Uma possibilidade para lidar com dados faltantes é a exclusão. Existem outras. 
dados<-na.exclude(dados)

X <- as.matrix(dados[,3:18])

# Padronização sugerida por @fernandohitmas
Z <-  apply(X, 2, function(x) (x - mean(x))/sd(x))
mean(Z)
cov(Z)

# Instale o macote mvShapiroTest se necessário
#install.packages("mvShapiroTest")
library(mvShapiroTest)
mvShapiro.Test(Z)

# Como a normalidade foi rejeitada, estimação via Fatores principais
# Método das Componentes Principais (Fatores principais)
S<-cov(Z)
plot(eigen(S)$values, type="b")
eigen(S)$values

n <- nrow(Z)
p <- ncol(Z)

par(mfrow=c(2,8))
for(i in 1:16)
  boxplot(Z[,i], main=names(Z)[i])

par(mfrow=c(1,1))

# --- Critério: Scree plot --- #
plot(eigen(cor(Z))[[1]], type="b", pch=16, main="Scree plot", ylab="autovalor", xlab="ordem da componente")


# --- Estimação via componentes principais --- #
# --- Critério: Autovalores maiores do que um --- #
sum(princomp(Z, cor=TRUE)[[1]]>1)

# --- Cargas fatoriais --- #
L = cbind(sqrt(eigen(cov(Z))[[1]][1]) * eigen(cov(Z))[[2]][,1],
          sqrt(eigen(cov(Z))[[1]][2]) * eigen(cov(Z))[[2]][,2],
          sqrt(eigen(cov(Z))[[1]][3]) * eigen(cov(Z))[[2]][,3])

round(L, 2)

# --- Rotação varimax --- #
analise_fatorial_varimax <- varimax(cbind(sqrt(eigen(cor(Z))[[1]][1]) * eigen(cor(Z))[[2]][,1],
                                          sqrt(eigen(cor(Z))[[1]][2]) * eigen(cor(Z))[[2]][,2],
                                          sqrt(eigen(cor(Z))[[1]][3]) * eigen(cor(Z))[[2]][,3]))

L <- analise_fatorial_varimax$loadings
round(L, 2)

# Escores: Método da Regressão
escores <- t(L) %*% solve(cov(Z)) %*% t(Z)
escores <- t(escores)  

dados<-cbind(dados, escores)


boxplot(escores[,1] ~ dados$ID_carro)
boxplot(escores[,2] ~ dados$ID_carro)
boxplot(escores[,3] ~ dados$ID_carro)


#Verificando se a matriz de rotação é ortogonal
A<-varimax(cbind(sqrt(eigen(cor(Z))[[1]][1]) * eigen(cor(Z))[[2]][,1],
                 sqrt(eigen(cor(Z))[[1]][2]) * eigen(cor(Z))[[2]][,2],
                 sqrt(eigen(cor(Z))[[1]][3]) * eigen(cor(Z))[[2]][,3]))$rotmat

round(A %*% t(A),2)

# factanal só estima por máxima verossimilhança com a suposição de normalidade!! 
# Como saber se podemos fazer a análise dessa forma?
# Cuidado, pois a normalidade foi rejeitada!! Apenas para ilustrar:

analisefatorial <- factanal(Z, factors=3, rotation="varimax")
analisefatorial

#--- Cargas fatoriais ---#
cargas <- analisefatorial$loadings[]

#--- Comunalidades  ---#
comunalidades <- diag(cargas %*% t(cargas))

#--- Variância específica ---#
varespecifica <- analisefatorial$uniqueness

#--- Estimativa de R  ---#
Rchapeu <- cargas %*% t(cargas) + varespecifica

#--- Matriz de resíduos  ---#
residuos <- cov(Z) - Rchapeu


# --- Exercício: Os dados tem distribuição normal? --- #
# --- Posso estimar o modelo utilizando o método da máxima verossimilhança?--- #

#install.packages("mvShapiroTest") # Executar se não tiver o pacote
library(mvShapiroTest)
mvShapiro.Test(as.matrix(Z))

MediaZ<-apply(Z, 2, mean)
MediaZ
S<-cov(Z)


# Distâncias dj
par(mfrow=c(1,1))
d<-rep(0,n)

for (i in 1: n)
  d[i] <- t(as.matrix(Z[i,] - MediaZ)) %*% solve(S) %*% as.matrix(Z[i,] - MediaZ)

qqplot(d, qchisq(ppoints(n), p), pch=16)
abline(0,1)


# --- Envelope para as distâncias - executar código da função antes --- #

source("http://wiki.icmc.usp.br/images/2/23/Envelope.distancias.txt")
envelope.distancias(d, n, MediaZ, S)
