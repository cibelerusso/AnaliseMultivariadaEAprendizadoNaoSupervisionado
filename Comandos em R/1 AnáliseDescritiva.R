# SME0822 Análise Multivariada e Aprendizado Supervisionado
# Prof. Cibele Russo - ICMC USP

# Aula 1 - Análise descritiva de dados multivariados

# Exemplo 1: Dados de vendas de livros

# install.packages("corrplot")

library(corrplot)
X <- matrix(c(42,52,48,58,2,3,2,3), nrow=4,
          ncol=2, byrow=FALSE)

X <- data.frame(X)
colnames(X) <- c('valor da nota','número de livros')

X

Xbarra<-apply(X, 2, mean)
Xbarra

S<-cov(X)
S

R<-cor(X)
R

corrplot(R, type="lower")

# Exemplo 2: Dados Iris

# Leitura e visualização dos dados
data(iris)
View(iris)

# Medidas-resumo
apply(iris[,1:4], 2, mean)

S<-cov(iris[,1:4])
S
round(S,2)

R<-cor(iris[,1:4])
round(R,2)

corrplot(R)

# Exercício:
Obter estatísticas descritivas separadas por espécie de flores.

# Tabelas de frequências absolutas e relativas
 table(iris[,5])
 table(iris[,5])/nrow(iris)

 # Gráfico de dispersão multivariado
 
 pairs(iris[1:4], pch=16, main="Matriz de dispersão")

# Exercício
# Obter gráficos de dispersão multivariados separadas por espécie de flores
 
# Exercício
# Incluir correlações no triângulo inferior e histogramas na diagonal
# (Ver referência “Gráficos estadísticos con R")
 
# Gráfico de coordenadas paralelas
library(lattice)
parallel(iris[,1:4]) 


# Faces de Chernoff - Exemplo de TeachingDemos

# install.packages('TeachingDemos')
require(TeachingDemos)
A = matrix(sample(1:1000,128,),16,8)
faces(A, main='random faces')


# Gráficos de estrelas - 'Radar'
publicfirms <- read.table('https://raw.githubusercontent.com/cibelerusso/AnaliseMultivariadaEAprendizadoNaoSupervisionado/master/Dados/Publicfirms.txt')

colnames(publicfirms) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "Empresa")
rownames(publicfirms) <- publicfirms[,9]

stars(publicfirms[,1:8],key.loc=c(11,2), main='Gráfico de estrelas para
dados de empresas públicas')

# Veja também https://pt.wikipedia.org/wiki/Gr%C3%A1fico_de_radar

# Exemplo de utilização
# https://www.pesmaster.com/pes-2016/search/?myclub=yes 

