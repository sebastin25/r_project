library(RJDBC)
library(rpart)
set.seed(1)
driver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="ojdbc6.jar")
conn <- dbConnect(driver,"jdbc:oracle:thin:@127.0.0.1:1521:XE","juan","Juandaniel18")
sqlText <- paste("SELECT
                 CLIENTES.CEDULA,
                 CLIENTES.SALARIO,
                 TRUNC(MONTHS_BETWEEN(MAX(FECHA), MIN(FECHA))) CANTIDAD_MESES,
                 SUM(MONTO) / TRUNC(MONTHS_BETWEEN(MAX(FECHA), MIN(FECHA))) PROMEDIOS_GASTOS,
                 SUM(PAGOS.MONTO) TOTAL_GASTOS
                 FROM PAGOS
                 INNER JOIN CLIENTES
                 ON CLIENTES.CEDULA = PAGOS.CEDULA
                 GROUP BY
                 CLIENTES.CEDULA,
                 CLIENTES.SALARIO
                 ORDER BY
                 CLIENTES.CEDULA")
gastos <- dbGetQuery(conn, sqlText)
gastos$promedio = (gastos$PROMEDIOS_GASTOS / gastos$SALARIO) * 100
gastos$aprobado <- ifelse(gastos$promedio < 60,TRUE,FALSE)
#gastos
number <- sample(107, 70)
train <- gastos[number,]
test <-gastos[-number,]
informacion <- rpart(aprobado ~ train$PROMEDIOS_GASTOS + train$CANTIDAD_MESES + train$SALARIO, data = train, method = "class")
informacion
#plot(informacion, uniform = TRUE)
#text(informacion, use.n = TRUE, all = TRUE)
plot(train$SALARIO, train$PROMEDIOS_GASTOS, col = ifelse(train$aprobado,'green','red'))