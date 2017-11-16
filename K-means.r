library("RJDBC")
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

driver<-JDBC(driverClass ="oracle.jdbc.OracleDriver","D:/DriverR/ojdbc7.jar")
conn<-dbConnect(driver,"jdbc:oracle:thin:@192.168.1.12:1521:XE","system","Admin12345")
sqlText <- paste("select c.cedula Cliente, sum(p.monto) total_pagos, c.salario salario_cliente
from  SGONZALEZ.PAGOS p
inner join SGONZALEZ.clientes c
on p.cedula = c.cedula
group by c.cedula , c.salario
order by c.cedula
")
data <- dbGetQuery(conn, sqlText)
summary(data)

#Remove rows with missing values on columns specified
data <- na.omit(data)
head(data)

set.seed(1)
k <-kmeans(data[,-c(1)], centers=2,  nstart = 25) #Create 2 clusters, Remove columns 1
k$centers #Display cluster centers
table(k$cluster) #Give a count of data points in each cluster
str(k)

#distancia
distance <- get_dist(data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#Averiguar k
fviz_nbclust(data, kmeans, method = "wss")
#Muestra Grafico
fviz_cluster(k, data = data,
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)
