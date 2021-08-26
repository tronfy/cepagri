names <- c('horario', 'temp', 'vento', 'umid', 'sensa')
cepagri <- read.csv('cepagri.csv', header = FALSE, sep = ';', col.names = names)

summary(cepagri)
