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


-------------------------------------------------------------------------------------------------------------------------------------


library("RJDBC")
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(data.table)

driver<-JDBC(driverClass ="oracle.jdbc.OracleDriver","D:/DriverR/ojdbc7.jar")
conn<-dbConnect(driver,"jdbc:oracle:thin:@192.168.1.12:1521:XE","system","Admin12345")
sqlText <- paste('
  select * from (
    select Cedula, count(*) as Cantidad, sucursal, sum(monto) monto  from SGONZALEZ.pagos  group by sucursal,cedula order by cedula,sucursal)
  pivot(sum(monto), sum(cantidad)cantidad for sucursal in (1 "Alajuela",2 "San Jose" ,3 "Cartago",4 "Heredia",5 "Guanacaste",6 "Limon",7 "Puntarenas")) order by cedula
'
)

data <- dbGetQuery(conn, sqlText)
View(data)

data <- data[,-c(1,3,5,7,9,11,13,15)] 
data <- na.omit(data)


#Averiguar k
fviz_nbclust(data, kmeans, method = "wss")

#distancia
distance <- get_dist(data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


set.seed(1)
k <-kmeans(data, centers=2,  nstart = 25) #Create 2 clusters, Remove columns 1
k$centers #Display cluster centers
table(k$cluster) #Give a count of data points in each cluster
str(k)


#Muestra Grafico
fviz_cluster(k, data = data,
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)
plot(data,k$centers)


plot(data,col=k$cluster) #centers o cluster?
points(k$center,col=1:2,pch=8,cex=1)


---------------------------------------------------------------------------------------------------------------------------------------------

library("RJDBC")
driver<-JDBC(driverClass ="oracle.jdbc.OracleDriver","D:/DriverR/ojdbc7.jar")
conn<-dbConnect(driver,"jdbc:oracle:thin:@192.168.1.12:1521:XE","system","Admin12345")
set.seed(1)
sqlText <- paste("select c.cedula, sum(p.monto) total_pagos, c.salario salario_cliente
from  SGONZALEZ.PAGOS p
                 inner join SGONZALEZ.clientes c
                 on p.cedula = c.cedula
                 group by c.cedula, c.salario
                 order by c.cedula")
data <- dbGetQuery(conn, sqlText)
summary(data)

k <-kmeans(data[,-c(1)], centers=5) #Create 5 clusters, Remove columns 1
k$centers #Display cluster centers
table(k$cluster) #Give a count of data points in each cluster

plot(k$centers)

rng<-2:20 #K from 2 to 20
tries<-100 #Run the K Means algorithm 100 times
avg.totw.ss<-integer(length(rng)) #Set up an empty vector to hold all of points
for(v in rng){ # For each value of the range variable
  v.totw.ss<-integer(tries) #Set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp<-kmeans(data,centers=v) #Run kmeans
    v.totw.ss[i]<-k.temp$tot.withinss#Store the total withinss
  }
  avg.totw.ss[v-1]<-mean(v.totw.ss) #Average the 100 total withinss
}
plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")
----------------------------------------------------------------------------------------------------------------------------------------------



