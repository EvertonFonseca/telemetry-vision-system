library(DBI)
library(RPostgres)
library(dplyr)

# conexão origem
con_src <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = 'system',
  username = 'root',
  password = 'ssbwarcq',
  host = '127.0.0.1',
  port = 3306
)

# conexão destino
con_dst <- dbConnect(
  MariaDB(),
  host = "host_destino",
  user = "user",
  password = "senha",
  dbname = "destino_db"
)

# lê da origem
df <- dbReadTable(con_src, "tabela_origem")

# grava no destino
dbWriteTable(con_dst, "tabela_destino", df,
             append = TRUE, row.names = FALSE)
