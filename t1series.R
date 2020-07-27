# Importando Pacotes ###########################################################

library(dplyr)
library(forecast)
library(xts)
library(imputeTS)
library(anomalize)

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

# Antes de transformar a série diária em semanal, precisamos tratar os valores
# faltantes. De acordo com o enunciado, "consideramos a média das observações 
# vizinhas (observações anterior e posterior ao NA)".
# Fiz isso imputação via médias móveis:
df2$V2 <- na_ma(df2$V2, k = 1, weighting = "simple", maxgap = Inf)

# Agrupando os dados em semanas
df2$Week <- cut.Date(df2$V1, breaks = "weeks")
df_ts <- df2 %>% group_by(Week) %>% summarise(Mortes=sum(V2))
df_ts$Week <- as.Date(df_ts$Week, format = "%Y-%m-%d")

# Questão 1 ####################################################################

# Transformando o dataframe em série xts
serie <- xts(df_ts$Mortes, order.by = df_ts$Week, frequency = 52)

# Características empíricas da série
plot(serie, main = "Chicago - Mortes Semanais")
plot(decompose(ts(df_ts$Mortes, frequency = 52)))

# Tendência
# Aparentemente, a série apresenta uma tendência decrescente (não se sabe se
# linear, quadrática ou outro).
# Pela decomposição, confirmamos a presença de tendência decrescente 
# possivelmente linear.
# Vemos que a partir de 1995, as mortes decrescem mais rapidamente.

# Sazonalidade
# A série parece ter observações com um comportamento muito parecido ao longo 
# de semanas/meses Isso é um indício de presença de sazonalidade.
# Pela decomposição feita, percebemos uma amplitude baixa da sazonalidade em
# relação à tendência e ao resíduo. Para checarmos a influência da sazonalidade
# na série, usamos:
serie <- ts(df_ts$Mortes, frequency = 52)
dec <- decompose(serie)
Rt <- dec$random 
Rt <- Rt[-c(1:26, 706:731)]
St <- dec$seasonal
St <- St[-c(1:26, 706:731)]
Tt <- dec$trend
Tt <- Tt[-c(1:26, 706:731)]

# De 0 a 1, a sazonalidade possui valor 0.3
# É considerável...
FS <- max(0,1-(var(Rt)/var(St+Rt)))
FS

# A tendência possui um valor maior, de 0.55
FT <- max(0,1-(var(Rt)/var(Tt+Rt)))
FT

# Outliers
# Pelo pacote anomalize, conseguimos detectar anomalias na nossa série,
# baseados no resíduo de uma decomposição.
df_ts %>% 
  time_decompose(Mortes) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

# Quem são as anomalias?
df_ts %>% 
  time_decompose(Mortes) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes')

# A série começa com um ponto aberrante na semana do dia 29/Dez/86. Isso se 
# deve pelo fato de que nossas observações começam no dia 01/Jan/87 (uma 
# quarta-feira), logo o número de mortes não leva em consideração domingo, 
# segunda e terça dessa semana.
# Para entender o motivo da anomalia do dia 06/Mar/89, precisamos entender
# melhor de onde vieram nossos dados (coisa que não tenho).

# Questão 2 ####################################################################
