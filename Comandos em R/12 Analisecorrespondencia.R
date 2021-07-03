# SME0822 Análise Multivariada e Aprendizado Supervisionado
# Prof. Cibele Russo - ICMC USP

# Exemplo Análise de Correspondência - Exemplo cerâmica e sítios arqueológicos
# Johnson e Wichern

  dados<-scan()
  30
  53
  73
  20
  46
  45
  16 
  10
  4
  1
  6
  36
  6
  28 
  10
  16
  41
  1
  37
  59
  169 
  39
  2
  1
  4
  13
  10
  5
  
  
  
X<-matrix(dados,nrow=7,byrow=F)

rownames(X)<-c("P0","P1","P2","P3","P4","P5","P6")
colnames(X)<-c("A","B","C","D")

library(vcd)
mosaic(X, shade = TRUE)

#library(xtable)
#xtable(round(X))

P<-round(X/781, 3)

#xtable(round(X/781,3), digits=3)

# Cálculo de r e c
r <- rowSums(P)
c <- colSums(P)

r
c

# Cálculo de r e c - outra forma
r<-rowSums(X)/sum(X)
c<-colSums(X)/sum(X)
r
c


#xtable(r %*% t(c), digits=2)


#install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
#install.packages("ca")
library(ca)

fit <- ca(X)

print(fit) # basic results 
summary(fit) # extended results 
plot(fit) # symmetric map


