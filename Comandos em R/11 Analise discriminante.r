# SME0822 Análise Multivariada e Aprendizado Supervisionado
# Prof. Cibele Russo - ICMC USP

rm(list=ls(all=TRUE))
gc(TRUE)

#set.seed(1) 

dados.banc<-read.table("https://raw.githubusercontent.com/cibelerusso/AnaliseMultivariadaEAprendizadoNaoSupervisionado/master/Dados/Dados_bancarios.csv", header=TRUE, sep=";", dec=",")

names(dados.banc)
attach(dados.banc)

View(dados.banc)


# Realize uma análise exploratoria dos dados
#install.packages("corrplot")
library(corrplot)
corrplot(cor(dados.banc))

boxplot(Idade ~Inadimplente )

boxplot(Salario.decl ~Inadimplente )

boxplot(Saldo.cc ~ Inadimplente )


# Desenvolva uma análise discriminante considerando o status Inadimplente 
# Utilize como covariáveis o Sexo, idade, empresa, salário declarado, saldo em conta corrente
# + saldo na poupança, saldo em conta há 1 e 2 anos.

# Separe 70% da base para treino e avalie o percentual de acertos na base de teste

amostra <- sample(nrow(dados.banc)*0.7, replace = F)

dados.treino <- dados.banc[amostra,]
dados.teste <- dados.banc[-amostra,]


# Modelo de regressão logística considerando a base de treino
modelo<-glm(Inadimplente ~ Sexo+Idade+Empresa+Salario.decl+Saldo.cc+Saldo.poup+Saldo.1ano+Saldo.2anos+Tempo.relac, 
            family=binomial(link = 'logit'), data=dados.treino)
summary(modelo)

# Predição na base de teste
predicao <- predict(modelo, newdata=dados.teste, type="response") 

dados.teste$classificacao <- 0
dados.teste$classificacao[predicao>0.5] = 1
  

# Calculo do percentual de acerto comparando o predito (classificação) e o observado

sum(1-abs(dados.teste$classificacao - dados.teste$Inadimplente))/nrow(dados.teste)

#como melhorar o modelo?


# Modelo de regressão logística considerando a base de treino
modelo<-glm(Inadimplente ~ Dev.cartao+Sexo+Idade+Empresa+Salario.decl+Saldo.cc+Saldo.poup+Saldo.1ano+Saldo.2anos+Tempo.relac, 
            family=binomial(link = 'logit'), data=dados.treino)
summary(modelo)

# Predição na base de teste
predicao <- predict(modelo, newdata=dados.teste, type="response") 

dados.teste$classificacao <- 0
dados.teste$classificacao[predicao>0.5] = 1


# Calculo do percentual de acerto comparando o predito (classificação) e o observado

sum(1-abs(dados.teste$classificacao - dados.teste$Inadimplente))/nrow(dados.teste)


# Sugestões para melhorar este modelo? 

