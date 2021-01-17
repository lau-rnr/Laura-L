#*************************************UNIVERSIDAD POLITÉCNICA SALESIANA********************************************************
#********************************HERRAMIENTAS PARA EL CONTROL DE LA PRODUCCIÓN****************************************************
#***********************************************CLUSTERS**************************************************************

#NOMBRE: LAURA LUGMANIA, ANDRES BETANCOURT, RICARDO SALAZAR, JOSE CÁRDENAS



library(readr)
segmentation_data <- read_csv("C:/Users/LAURA/Downloads/segmentation_data.csv")
View(segmentation_data)

#SELCCIÓN DE VARIABLES
seg <- segmentation_data[,-c(1),drop=FALSE]
View(seg)

#DISTANCIAS
ds = dist(seg[,1:3], method = "euclidean")
ds #matriz distancia o vector 


#CORRELACIÓN DE VARUABLES
cs=cor(seg)
cs
library(corrplot)
corrplot(cs)

#ESCALAMIENTO DIMENSIONAL
fits = cmdscale(ds,eig=TRUE, k=2) 
# k es el numero de dimensiones a las que se pasa en este caso las mismas 2
#eig=valores propios cambio de base

xs = fits$points[,1] 
ys = fits$points[,2]

#IDENTIFICACIÓN DE GRUPOS POR COLORES
plot(xs,ys, col=c("magenta","green3","orange","skyblue"), main = "clientes Original")

#*******************************CLUSTERING*********************************************
#MÉTODO 1.K-Means
grupos2 = kmeans(seg,4)
g1s = grupos2$cluster#valores diferentes porque el cluster es subjetivo
g2s = grupos2$size #TAMAÑO DE CADA GRUPO
g1s
g2s
plot(xs,ys, col=c("magenta","green3","orange","skyblue" )[g1s], main = "CLIENTES K-Means")

#******************************** DATOS OUT*************************************************
# PARA DEFINIR A QUE GRUPO PERTENECE CADA DATO SE EXPORTA UN ARCHIVO EXCEL CON LOS GRUPOS FORMADOS
# EN EL MPETODO K-MEANS

export <-cbind(seg,g1s)
View(export)
setwd("C:/Users/LAURA/Desktop")
#install.packages("xlsx")
library(xlsx)
write.xlsx(export, "T2 en xls_1.xlsx")

#******************************** MÉTODO JERARQUICO******************************************
#MÉTODO 2.DHC
library("dendextend")
hcs= hclust(ds, method = "complete" )#solo acepta la matriz de decisiones
clus3s = cutree(hcs, 4)#corte del arbol en 2 grupos 
dends = as.dendrogram(hcs)
dends = color_branches(dends, 4)
colors = c("magenta", "green3","orange","skyblue")
plot(dends, fill = colors[clus3s], cex = 0.1 , main = "CLIENTES DHC JERARQUICO")

#******************************NÚMERO DE GRUPOS********************************************
##en el codo se encuentran el numero de datos ideales
#Elbow 
#PARA DETERMINAR EL NÚMERO DE GRUPOS
wis = c()
for (i in 1:10) 
{
  gs = kmeans(seg,i) 
  wis[i] = gs$tot.withinss
}
plot((1:length(wis)),wis, xlab="Numero de Clusters", ylab="Suma Cuadrados Internos", pch=19, col="red", type = "b")

#*******************************VALIDACIÓNES***********************************************
##validacion interna
library(cluster)
library(clValid)
du1s = dunn(ds,g1s)
du2s= dunn(ds,clus3s)
du1s
du2s

sil1s = silhouette(g1s,ds)
plot(sil1s,col=1:4, border=NA)
sil2s = silhouette(clus3s,ds)
plot(sil2s,col=5:8, border=NA)
sil1s
sil2s

##validacion externa
library(aricode)
library(plyr)
ARI1= ARI(g1s,g1s)
ARI2= ARI(g1s,clus3s)
AMI1= AMI(g1s,g1s)
AMI2= AMI(g1s,clus3s)
NMI1= NMI(g1s,g1s,variant = c("joint"))
NMI2= NMI(g1s,clus3s,variant = c("joint"))


ARI1
ARI2
AMI1
AMI2
NMI1
NMI2

# COMPARACIÓN CON TIERRA VERDADERA

tierra = as.factor(export$g1s)         #GROUND

plot(xs,ys,col=c("magenta","green3","orange","skyblue")[tierra], main = "CLIENTES TIERRA VERDADERA")
    

##validacion externa

library(aricode)
library(plyr)
ARI1T= ARI(tierra,g1s)
ARI2T= ARI(tierra,clus3s)
AMI1T= AMI(tierra,g1s)
AMI2T= AMI(tierra,clus3s)
NMI1T= NMI(tierra,g1s,variant = c("joint"))
NMI2T= NMI(tierra,clus3s,variant = c("joint"))


ARI1T
ARI2T
AMI1T
AMI2T
NMI1T
NMI2T
