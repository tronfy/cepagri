library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)

# fazer download do arquivo, se necessário
if (!file.exists("cepagri.csv")) {
  download.file(
    "http://ic.unicamp.br/~zanoni/cepagri/cepagri.csv",
    "cepagri.csv"
  )
}

# ler dataset .csv
names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.csv("cepagri.csv", header = FALSE, sep = ";", col.names = names)

# criar dataframe, tratar tipos de coluna e erros
df <- data.frame(cepagri)
# ignorar warning de leituras N/A, vamos corrigir isso na próxima linha
suppressWarnings({
  df <- df %>%
    mutate(horario = as_datetime(df$horario, format = "%d/%m/%Y-%H:%M")) %>%
    mutate(temp = as.numeric(df$temp))
})
df <- na.omit(df)

# extrair dados entre 2015-01-01 e 2020-12-31
df <- df[
  df$horario >= "2015-01-01" &
    df$horario <= "2020-12-31",
]

salvar <- function(filename, plot, aspect_ratio = 2.5) {
  if (!dir.exists("plot")) {
    dir.create("plot")
  }
  ggsave(
    paste("plot/", filename, ".png", sep = ""),
    plot + theme(text = element_text(size = 20)),
    height = 7, width = 7 * aspect_ratio
  )
}


### Estações do Ano ###

estacao_cores <- c(
  verão = "#DB7268",
  primavera = "#91F57F",
  outono = "#8AF5E1",
  inverno = "#7B9DF0"
)
estacao <- function(data) {
  data <- as.Date(data)
  ndata <- 100 * month(data) + day(data)
  cuts <- base::cut(ndata, breaks = c(0, 319, 0620, 0921, 1220, 1231))
  levels(cuts) <- c("verão", "outono", "inverno", "primavera", "verão")
  return(as.character(cuts))
}

diario <- df %>%
  mutate(data = floor_date(df$horario, unit = "day")) %>%
  group_by(data) %>%
  summarize(
    temp_media = mean(temp),
    umid_media = mean(umid),
    vent_media = mean(vento)
  )
diario <- within(diario, estacao <- estacao(data))

salvar("diario_temp", ggplot(diario, aes(data, temp_media)) +
  geom_point(aes(colour = estacao)) +
  labs(y = "temperatura média") +
  ggtitle("temperatura média por dia") + 
  scale_color_manual(values = estacao_cores))

salvar("diario_umid", ggplot(diario, aes(data, umid_media)) +
  geom_point(aes(colour = estacao)) +
  labs(y = "umidade média") +
  ggtitle("umidade média por dia") + 
  scale_color_manual(values = estacao_cores))

salvar("diario_vent", ggplot(diario, aes(data, vent_media)) +
  geom_point(aes(colour = estacao)) +
  labs(y = "vento médio") +
  ggtitle("vento médio por dia") + 
  scale_color_manual(values = estacao_cores))


### MÉDIAS POR HORA ###
porHora <- df %>%
  mutate(hora = hour(horario)) %>%
  group_by(hora) %>%
  summarize(
    temp_media = mean(temp),
    umid_media = mean(umid),
    vent_media = mean(vento),
  )

salvar("hora_temp", ggplot(porHora, aes(x = hora, y = temp_media)) + geom_bar(stat="identity"))
salvar("hora_umid", ggplot(porHora, aes(x = hora, y = umid_media)) + geom_bar(stat="identity"))
salvar("hora_vent", ggplot(porHora, aes(x = hora, y = vent_media)) + geom_bar(stat="identity"))
