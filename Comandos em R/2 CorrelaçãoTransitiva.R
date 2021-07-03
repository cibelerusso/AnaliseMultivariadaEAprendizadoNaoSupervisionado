# SME0822 Análise Multivariada e Aprendizado Supervisionado
# Prof. Cibele Russo - ICMC USP

# Correlação é transitiva?

set.seed(1)

# Primeiro caso: (X,Y) e (X,Z) tem correlação forte

corXY <- rep(0,100)
corXZ <- rep(0,100)
corYZ <- rep(0,100)

for (i in 1:100)
{
X <- sample(1:1000,40,replace = FALSE)
Y <- 0.2*X + rnorm(40,0,4)
Z <- 0.2*X + rnorm(40,0,4)
corXY[i] <- cor(X,Y)
corXZ[i] <- cor(X,Z)
corYZ[i] <- cor(Y,Z)
}

boxplot(corXY, corXZ, corYZ)


# Segundo caso: (X,Y) e (X,Z) tem correlação moderada

corXY <- rep(0,100)
corXZ <- rep(0,100)
corYZ <- rep(0,100)

for (i in 1:100)
{
  X <- sample(1:1000,40,replace = FALSE)
  Y <- 0.01*X + rnorm(40,0,4)
  Z <- 0.01*X + rnorm(40,0,4)
  corXY[i] <- cor(X,Y)
  corXZ[i] <- cor(X,Z)
  corYZ[i] <- cor(Y,Z)
}

boxplot(corXY, corXZ, corYZ)


# Produto de Kronecker

# Exemplo tempo computacional com o uso do Produto de Kronecker e Propriedades

Sigma = matrix(c(3,1,2,1,2,1,2,1,4),byrow=T,nrow=3)
A = kronecker(diag(1000),Sigma)

start_time <- Sys.time()
B = solve(A)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
C = solve(kronecker(diag(1000),Sigma))
end_time <- Sys.time()
end_time - start_time

# Se você conhecer a propriedade que 
# (A \otimes B)^{-1} = A^{-1} \otimes B^{-1}
# em que \otimes indica o Produto de Kronecker

start_time <- Sys.time()
C = kronecker(solve(diag(1000)), solve(Sigma))
end_time <- Sys.time()
end_time - start_time




