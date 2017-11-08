library(RJDBC)
library(rpart)
library(rpart.plot)
options(scipen = 999)
set.seed(40)
driver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="ojdbc6.jar")
conn <- dbConnect(driver,"jdbc:oracle:thin:@127.0.0.1:1521:XE","juan","Juandaniel18")
sqlText <- paste("SELECT
                  CLIENTES.CEDULA,
                  CLIENTES.SALARIO,
                  SUM(MONTO) / TRUNC(MONTHS_BETWEEN(MAX(FECHA), MIN(FECHA))) PROMEDIOS_GASTOS
                  FROM PAGOS
                  INNER JOIN CLIENTES ON CLIENTES.CEDULA = PAGOS.CEDULA
                  GROUP BY CLIENTES.CEDULA,
                  CLIENTES.SALARIO
                  ORDER BY CLIENTES.CEDULA")
gastos <- dbGetQuery(conn, sqlText)
gastos$promedio = (gastos$PROMEDIOS_GASTOS / gastos$SALARIO) * 100
gastos$aprobado <- ifelse(gastos$promedio < 40,TRUE,FALSE)
number <- sample(107, 70)
train <- gastos[number,]
test <-gastos[-number,]
informacion <- rpart(aprobado ~ SALARIO, data = train, method = "class")
rpart.plot(informacion)