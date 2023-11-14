# Exemplo Análise de Correspondência - Dados banco
# Profa. Cibele Russo

#install.packages('pollster')
#install.packages('dplyr')
#install.packages('knitr')
#install.packages('ggplot2')
#install.packages(c('FactoMineR', 'factoextra'))
#install.packages('ca')
#install.packages('vcd')
#install.packages('table1')
#install.packages('pivottabler')
#install.packages('data.table')
library(pollster)
library(dplyr)
#library(knitr)
library(ggplot2)
library("FactoMineR")
library("factoextra")
library(ca)
library(dplyr)

dados<-read.csv('https://raw.githubusercontent.com/cibelerusso/Datasets/main/amostra_banco_distrito.csv', dec = ',')
colnames(dados)  


# Criar 5 faixas de renda a partir dos quantis

dados$Salario_cat <- cut(dados$Salario,
                         breaks=quantile(dados$Salario),
                         labels=c('D', 'C', 'B','A'))

tabela <- table(dados$Empresa, dados$Salario_cat)

nrow(tabela) 
ncol(tabela)
n <- sum(tabela)

X<-matrix(tabela, nrow=nrow(tabela) , byrow=F)

rownames(X)<-rownames(tabela)
colnames(X)<-colnames(tabela)

library('vcd') 
mosaic(X, 
       shade=TRUE,
       main = "Gráfico de mosaico"
) 

fit<-ca(X)

print(fit) # basic results 
summary(fit) # extended results 
plot(fit) # symmetric map

# Teste Qui-quadrado 
chisq.test(X)

## Análise de correspondência múltipla
dados = dados[,-1]

# Categorização das variáveis
dados$Salario_cat <- cut(dados$Salario,
                         breaks=quantile(dados$Salario),
                         labels=c('D', 'C', 'B','A'))

dados$Idade_cat <- cut(dados$Idade,
                         breaks=quantile(dados$Idade),
                         labels=c('Idade1', 'Idade2', 'Idade3','Idade4'))

# Seleção das variáveis categóricas
dados_categorizados = select(dados, Sexo, Idade_cat, Empresa, Salario_cat, Distrito)

res_mca <- MCA(dados_categorizados, graph = TRUE)

fviz_mca_var(res_mca, choice='mca.cor', shape.var = 2)

fviz_mca_var(res_mca,  
             shape.var = 2)

res_mca <- MCA(dados_categorizados, graph = TRUE)


fviz_mca_ind(res_mca, 
             label = "none", # hide individual labels
             habillage = c('Sexo', 'Salario_cat'), # color by groups 
             addEllipses = TRUE,
             ggtheme = theme_minimal()) 



