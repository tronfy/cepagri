names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.csv("cepagri.csv", header = FALSE, sep = ";", col.names = names)

df <- data.frame(cepagri)
df <- df[
  as.Date(df$horario, format = "%d/%m/%Y") >= "2015-01-01" &
    as.Date(df$horario, format = "%d/%m/%Y") <= "2020-12-31",
]

summary(df)
head(df)
tail(df)
