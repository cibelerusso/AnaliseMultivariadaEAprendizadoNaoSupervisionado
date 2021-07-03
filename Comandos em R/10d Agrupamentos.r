
# Alguns comandos básicos - Análise de Agrupamentos

dados<-read.csv("/home/cibele/Dados/ONU.csv", header=TRUE)

rownames(dados)<-dados[,1]

dados<-dados[,2:5]

dist(dados)

hclust(dist(dados))

par(mfrow=c(1,2))

plot(hclust(dist(dados), method = 'single'))
plot(hclust(dist(dados), method = 'ward'))



kmeans(dados, 4)

cl <- kmeans(dados, 4)

plot(dados, col = cl$cluster)

points(cl$centers, col = 1:5, pch = 8)

sort(cl$cluster)
