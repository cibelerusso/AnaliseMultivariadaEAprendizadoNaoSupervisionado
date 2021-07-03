# Aplicação: Regressão Multivariada
#Exercício 7.26 de  Johnson \& Wichern (2007): Deseja-se explicar a resistência de alguns tipos de fibra de celulose. Em um experimento, foram obtidas n=62 medidas de fibras de celulose e papel. Esses dados estão disponíveis na library robustbase do R sob o nome de pulpfiber. As variáves são:
# $Y_1$: comprimento na quebra
# $Y_2$: módulo de elasticidade
# $Y_3$: estresse na falha
# $Y_4$: resistência à quebra
# $Z_1$: comprimento da fibra
# $Z_2$: fração de fibra grossa
# $Z_3$: fração de fibra fina
# $Z_4$: extensão à tração nula


#install.packages(robustbase)
library(robustbase)

fibras = read.csv('https://forge.scilab.org/index.php/p/rdataset/source/file/368b19abcb4292c56e4f21079f750eb76b325907/csv/robustbase/pulpfiber.csv')
fibras = fibras[,-1]

View(fibras)

#install.packages('GGally')
library(GGally)
ggpairs(pulpfiber, upper = "blank")


ggcorr(fibras)

names(fibras)

colnames(fibras)[1:4] = c('Z1','Z2','Z3','Z4')

attach(fibras)

modelo1 = lm(Y1 ~ Z1+Z2+Z3+Z4)
modelo2 = lm(Y2 ~ Z1+Z2+Z3+Z4)
modelo3 = lm(Y3 ~ Z1+Z2+Z3+Z4)
modelo4 = lm(Y4 ~ Z1+Z2+Z3+Z4)

summary(modelo1)
summary(modelo2)
summary(modelo3)
summary(modelo4)


# Installs ggResidpanel from CRAN
#install.packages("ggResidpanel")

library(ggResidpanel)

# Installs the development version of ggResidpanel from GitHub
#devtools::install_github("goodekat/ggResidpanel")

resid_panel(modelo1)
resid_panel(modelo2)
resid_panel(modelo3)
resid_panel(modelo4)


# Modelo multivariado
modelo_multivariado <- lm(cbind(Y1,Y2,Y3,Y4) ~ Z1+Z2+Z3+Z4, data=fibras)
summary(modelo_multivariado)


# Exercício: Ver Regressão de Posto reduzido
# Fonte: https://cran.r-project.org/web/packages/rrr/vignettes/rrr.html
