# Importando Pacotes ###########################################################

library(dplyr)
library(forecast)
library(xts)

# Lendo os Dados ###############################################################

df <- read.delim("Chicago-65ate74.txt", sep = " ", header = F)
head(df)
str(df)

# Pré-Processamento dos Dados ##################################################

# Arrumando os tipos das variáveis
df$V1 <- as.Date(df$V1, format = "%Y-%m-%d")

# Conhecendo nossos dados:
# 
# Observações diárias de mortes em Chicago, desde 1 de Janeiro de 1987 até
# 31 de Dezembro de 2000.

plot(x = df$V1,
     y = df$V2, 
     type = 'l', 
     main = "Mortes Diárias - Chicago", 
     xlab = "Data",
     ylab = "Mortes")

summary(df)

# Temos 3 dias e 10 valores de mortes faltando... Precisamos arrumar isso.
# Quais os dias que estão com valores faltando?
df %>% filter(is.na(V2))

# Esses 3 dias faltando são no final da série ou no meio? Se for no meio, 
# precisamos preencher.
df %>% 
  tidyr::complete(V1) %>% 
  filter(is.na(V1))

# Como após completar as datas os NA ainda estão nos dados, provavelmente foram 
# um erro de leitura. Olhando o txt, percebemos que as linhas 4240, 4998 e 5000 
# possuem 2 espaços como separador. Vamos arrumar isso.
df_raw <- readLines("Chicago-65ate74.txt")
df_raw <- gsub("  ", " ", df_raw)
write.table(df_raw, 
            "Chicago-65ate74_2.txt", 
            row.names = F, 
            col.names = F, 
            quote = F)

# Checando os Dados 2 ##########################################################

df2 <- read.delim("Chicago-65ate74_2.txt", sep = " ", header = F)

# Arrumando os tipos das variáveis
df2$V1 <- as.Date(df2$V1, format = "%Y-%m-%d")
summary(df2)

plot(x = df2$V1,
     y = df2$V2, 
     type = 'l', 
     main = "Mortes Diárias - Chicago", 
     xlab = "Data",
     ylab = "Mortes")

# Os dados estão com 4 valores faltantes nas mortes, mas agora estão ok.
# Vamos remover os outros dados e só manter df2.
rm(df, df_raw)

# Salvo isso como um checkpoint.
save.image("~/Series Temporais -2020/Trabalhos/T1Series/checkpoint1.RData")

# Começando o Trabalho #########################################################

load("~/Series Temporais -2020/Trabalhos/T1Series/checkpoint1.RData")






