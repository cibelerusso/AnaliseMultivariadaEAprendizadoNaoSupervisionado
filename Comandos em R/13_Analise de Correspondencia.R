# Exemplo Análise de Correspondência - Dados banco
# Profa. Cibele Russo

install.packages('pollster')
install.packages('dplyr')
install.packages('knitr')
install.packages('ggplot2')
install.packages(c('FactoMineR', 'factoextra'))
install.packages('ca')
install.packages('vcd')
install.packages('table1')
install.packages('pivottabler')
install.packages('data.table')
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

# Categorização da variável Salario
# Criar 4 faixas de renda a partir dos quantis

dados$Salario_cat <- cut(dados$Salario,
                         breaks=quantile(dados$Salario),
                         include.lowest = TRUE,
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

# Teste Qui-quadrado 
chisq.test(X)

fit<-ca(X)

print(fit) # basic results 
summary(fit) # extended results 
plot(fit) # symmetric map



## Análise de correspondência múltipla
dados = dados[,-1]

# Categorização das variáveis
dados$Salario_cat <- cut(dados$Salario,
                         breaks=quantile(dados$Salario), include.lowest = TRUE,
                         labels=c('D', 'C', 'B','A'))

dados$Idade_cat <- cut(dados$Idade,
                       breaks=quantile(dados$Idade), include.lowest = TRUE,
                       labels=c('Idade1', 'Idade2', 'Idade3','Idade4'))

dados$Dev_cat <- cut(dados$Devedor_cartao,
                     breaks=quantile(dados$Devedor_cartao), include.lowest = TRUE,
                     labels=c('Dev1', 'Dev2', 'Dev3','Dev4'))

# Seleção das variáveis categóricas
dados_categorizados = select(dados, Sexo, Inadimplente, Empresa, Dev_cat, Idade_cat)

dados_categorizados <- lapply(dados_categorizados, as.factor) %>% 
                       data.frame


res_mca <- MCA(dados_categorizados, graph = TRUE)

fviz_mca_var(res_mca, repel=TRUE, choice='var.cat',
             shape.var = 2)

fviz_mca_var(res_mca, repel=TRUE, choice='mca.cor',
             shape.var = 2)

fviz_eig(res_mca)

res_mca <- MCA(dados_categorizados, graph = TRUE)


fviz_mca_ind(res_mca, 
             #label = "none", # hide individual labels
             habillage = c( 'Inadimplente'), # color by groups 
             addEllipses = TRUE,
             ggtheme = theme_minimal()) 



