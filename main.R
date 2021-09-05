library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)

# ler dataset .csv
names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.csv("cepagri.csv", header = FALSE, sep = ";", col.names = names)

# criar dataframe, tratar tipos de coluna e erros
df <- data.frame(cepagri)
df <- df %>%
  mutate(horario = as.Date(df$horario, format = "%d/%m/%Y")) %>%
  mutate(temp = as.numeric(df$temp))
df <- na.omit(df)

# extrair dados entre 2015-01-01 e 2020-12-31
df <- df[
  df$horario >= "2015-01-01" &
  df$horario <= "2020-12-31",
]

salvar <- function(filename, plot) {
  if (!dir.exists("plot")) {
    dir.create("plot")
  }
  ggsave(paste("plot/", filename, ".png", sep=""), plot)
}



### MÉDIAS DIÁRIAS ###
diario <- df %>%
  mutate(dia = floor_date(df$horario)) %>%
  group_by(dia) %>%
  summarize(
    temp_media = mean(as.numeric(temp)),
    umid_media = mean(umid),
    vent_media = mean(vento),
  )

salvar("diario_temp", ggplot(diario, aes(dia, temp_media,)) + geom_point(colour = 'red') + labs(y = "temperatura média"))
salvar("diario_umid", ggplot(diario, aes(dia, umid_media,)) + geom_point(colour = 'blue') + labs(y = "umidade média"))
salvar("diario_vent", ggplot(diario, aes(dia, vent_media,)) + geom_point(colour = 'green') + labs(y = "vento médio"))
