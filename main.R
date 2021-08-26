# ler dataset .csv
names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.csv("cepagri.csv", header = FALSE, sep = ";", col.names = names)

# extrair dataframe entre 2015-01-01 e 2020-12-31
df <- data.frame(cepagri)
df <- df[
  as.Date(df$horario, format = "%d/%m/%Y") >= "2015-01-01" &
    as.Date(df$horario, format = "%d/%m/%Y") <= "2020-12-31",
]

# remover entradas com erro
df <- na.omit(df)

summary(df)
head(df)
tail(df)
