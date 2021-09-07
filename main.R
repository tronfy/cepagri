library(ggplot2)
library(ggcorrplot)
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

# função para facilitar salvamento de gráficos
salvar <- function(filename, plot, aspect_ratio = 2.5) {
  # criar diretório de gráficos, se não existir
  if (!dir.exists("plot")) {
    dir.create("plot")
  }
  # salvar o gráfico, aplicando tema e tamanho
  ggsave(
    paste("plot/", filename, ".png", sep = ""),
    plot + theme(text = element_text(size = 20)),
    height = 7, width = 7 * aspect_ratio
  )
}



### Estações do Ano ###

# cores para os gráficos de dispersão
estacao_cores <- c(
  verão = "#DB7268",
  primavera = "#91F57F",
  outono = "#8AF5E1",
  inverno = "#7B9DF0"
)

# dado uma data, retorna a estação do ano da mesma
estacao <- function(data) {
  data <- as.Date(data)
  
  # separa mês do dia, ex: 22/11 -> 1122
  ndata <- 100 * month(data) + day(data)
  
  # determina em qual corte a data está
  # 20/03 - equinócio de outono
  # 21/06 - solstício de inverno
  # 22/09 - equinócio de primavera
  # 21/12 - solstício de verão
  cuts <- base::cut(ndata, breaks = c(0, 0320, 0621, 0922, 1221, 1231))
  levels(cuts) <- c("verão", "outono", "inverno", "primavera", "verão")
  
  # retorna o nome da estação
  return(as.character(cuts))
}

diario <- df %>%
  # arrendondar dados no nível de dia, e agrupá-los
  mutate(data = floor_date(df$horario, unit = "day")) %>%
  group_by(data) %>%
  # extrair médias por dia
  summarize(
    temp_media = mean(temp),
    umid_media = mean(umid),
    vent_media = mean(vento),
    sens_media = mean(sensa)
  )
# computar estações para cada data
diario <- within(diario, estacao <- estacao(data))

# gerar e salvar uma matriz de correlação entre todos os dados
matcor <- cor(df[,c("temp", "umid", "vento", "sensa")])
salvar("matriz_cor", ggcorrplot(matcor, method ="circle"))

# (tabela) extrair médias por estação
diario %>% 
  group_by(estacao) %>% 
  summarise(
    temp = mean(temp_media),
    umid = mean(umid_media),
    vent = mean(vent_media)
  )

# salvar gráficos de média diária
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



### Ao Longo do Dia ###

porHora <- df %>%
  # extrair a hora de cada medição, e agrupa por ela
  mutate(hora = hour(horario)) %>%
  group_by(hora) %>%
  # calcula a média por hora
  summarize(
    temp_media = mean(temp),
    umid_media = mean(umid),
    vent_media = mean(vento),
  )

# salvar gráficos de média por hora
salvar("hora_temp", ggplot(porHora, aes(x = hora, y = temp_media)) + geom_bar(stat="identity"))
salvar("hora_umid", ggplot(porHora, aes(x = hora, y = umid_media)) + geom_bar(stat="identity"))
salvar("hora_vent", ggplot(porHora, aes(x = hora, y = vent_media)) + geom_bar(stat="identity"))



### Correlação entre Dados ###

# (tabela) correlação geral
cor(df[,c("temp", "umid", "vento", "sensa")])

# extrair médias mensais
mes <- df %>%
  # arrendondar dados no nível de mês, e agrupá-los
  mutate(data = floor_date(df$horario, unit = "month")) %>%
  group_by(data) %>%
  # extrair médias por mês
  summarize(
    temp_media = mean(temp),
    umid_media = mean(umid),
    vent_media = mean(vento),
    sens_media = mean(sensa)
  )

# salvar comparação temperatura x umidade
salvar("comp_temp_umid", ggplot(mes, aes(data, umid_media)) +
         geom_bar(stat="identity", fill="#7B9DF0") +
         geom_line(aes(data, temp_media, colour="#DB7268")) +
         geom_point(aes(data, temp_media, colour="#DB7268")) +
         labs(y = "temperatura/umidade") +
         ggtitle("temperatura e umidade médias por dia") +
         theme(legend.position="none"))

# salvar comparação temperatura x vento
salvar("comp_temp_vento", ggplot(mes, aes(data, vent_media)) +
         geom_bar(stat="identity", fill="#7B9DF0") +
         geom_line(aes(data, temp_media, colour="#DB7268")) +
         geom_point(aes(data, temp_media, colour="#DB7268")) +
         labs(y = "temperatura/vento") +
         ggtitle("temperatura e vento médios por dia") +
         theme(legend.position="none"))



### Média Anual ###

# extrair médias anuais
porAno <- df %>%
  mutate(ano = year(horario)) %>%
  group_by(ano) %>%
  summarize(
    temp_media = mean(temp),
    umid_media = mean(umid),
    vent_media = mean(vento),
  )

# salvar gráficos de médias anuais
salvar("ano_temp", ggplot(porAno, aes(x = ano, y = temp_media)) + geom_line(color = "red") 
       + geom_point(shape = 21, color = "black", fill = "red", size = 4) 
       + ggtitle("Temperatura Média no Ano"))
salvar("ano_umid", ggplot(porAno, aes(x = ano, y = umid_media)) + geom_line(color = "blue") 
       + geom_point(shape = 21, color = "black", fill = "blue", size = 4) 
       + ggtitle("Umidade Média no Ano"))
salvar("ano_vent", ggplot(porAno, aes(x = ano, y = vent_media)) + geom_line(color = "green") 
       + geom_point(shape = 21, color = "black", fill = "green", size = 4) 
       + ggtitle("Velocidade do Vento Média no Ano"))
