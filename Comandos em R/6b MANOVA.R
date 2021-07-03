# SME0822 Análise Multivariada e Aprendizado Supervisionado
# Prof. Cibele Russo - ICMC USP

# Aplicação: MANOVA: Comparação de médias multidimensionais em 5 grupos
# época: a época à qual o crânio foi atribuído, um fator ordenado com níveis c4000BC, c3300BC, c1850BC, c200BC e cAD150, onde os anos são dados apenas aproximadamente, é claro.
# MB: largura máxima do crânio.
# bh: altura basibregmática do crânio.
# bl: comprimento basialveolar do crânio.
# nh: altura nasal do crânio.


# Fontes: http://friendly.github.io/heplots/reference/Skulls.html
# https://cran.r-project.org/web/packages/heplots/vignettes/HE-examples.pdf
# Ver também: https://pt.wikipedia.org/wiki/An%C3%A1lise_multivariada_da_vari%C3%A2ncia

#install.packages('HSAUR')
#install.packages('car')
#install.packages('heplots')

library('car')
library('HSAUR')
library('heplots')

data(skulls)
View(skulls)

#install.packages('GGally')
library(GGally)

skulls_df = as.data.frame(skulls)

pm <- ggpairs(data = skulls_df, 
              mapping = aes(color = epoch),
              columns = c("mb","bh","bl","nh" ),
              upper = list(combo ='blank', continuous='blank')
             )
pm


# variable labels
vlab <- c("maxBreadth", "basibHeight", "basialLength", "nasalHeight")

skulls$epoch <- factor(skulls$epoch, labels=sub("c","",levels(skulls$epoch)))

means <- aggregate(cbind(mb, bh, bl, nh) ~ epoch, data=skulls, FUN=mean)[,-1]
rownames(means) <- levels(skulls$epoch)
means


pairs(means, vlab,panel = function(x, y) {
  text(x, y, levels(skulls$epoch))
  lines(x,y)})


# Teste de normalidade multivariada

library(mvShapiroTest)
mvShapiro.Test(as.matrix(skulls[,2:5]))


# fit manova model
sk.mod <- lm(cbind(mb, bh, bl, nh) ~ epoch, data=skulls)
summary(sk.mod)


manova(sk.mod)
summary(manova(sk.mod), test='Wilks')
summary(manova(sk.mod), test='Pillai')


# Fonte: https://cran.r-project.org/web/packages/heplots/vignettes/HE-examples.pdf

library(lattice)
library(reshape2) 

sklong <- melt(skulls, id="epoch")
bwplot(value ~ epoch | variable, data=sklong, scales="free",
            ylab="Variable value", xlab="Epoch",
            strip=strip.custom(factor.levels=paste(vlab, " (", levels(sklong$variable), ")", sep="")),
            panel = function(x,y, ...) {
                panel.bwplot(x, y, ...)
                panel.linejoin(x,y, col="red", ...)})

# Veja mais análises em https://cran.r-project.org/web/packages/heplots/vignettes/HE-examples.pdf


pairs(sk.mod, hypotheses=list(Lin="epoch.L", Quad="epoch.Q"), var.labels=vlab)

heplot(sk.mod, hypotheses=list(Lin="epoch.L", Quad="epoch.Q"), xlab=vlab[1], ylab=vlab[2])


