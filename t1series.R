# Importando Pacotes ###########################################################

library(dplyr)
library(forecast)
library(xts)
library(imputeTS)
library(anomalize)
library(tseries)
library(TSA)

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

# Questão 1a ###################################################################

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

# Questão 1b ###################################################################

# Como vimos na questão 1a, nossa série possui tendência e sazonalidade. Dado
# isso, a série não é estacionária.
# Teste Dick Fuller para raiz unitária:
adf.test(serie, k = 200)

# Pela fac, percebemos um decaimento lento do valor das autocorrelações ao longo
# das defasagens, o que indica presença de tendência. Além disso, a sazonalidade
# é evidenciada pelos picos na fac de 52 em 52 defasagens, aproximadamente.
acf(serie, lag.max = 200)

# Nosso modelo terá que levar em consideração isso.
# Separamos os últimos 3 meses dos dados para verificar as previsões.
# Vou retirar a primeira observação do treino... sabemos que é outlier.

# Transformando a série para ficar mais homogênea:
serie<- 10/serie

treino <- serie[2:(length(serie)-13)]
teste <- serie[(length(serie)-12):length(serie)]

# Agora com os dados prontos, vamos começar modelando a tendência.
# Pela decomposição feita anteriormente, uma tendência linear já deve resolver.
detrend_fit <- lm(treino ~ seq(1:length(treino)))
summary(detrend_fit)

plot(treino, type = "l")
lines(detrend_fit$fitted.values, col='red')

# Acho que pode melhorar... queremos um comportamento ondulado leve
detrend_fit2 <- lm(treino ~  cos(2/(365*3)*3.14*seq(1:length(treino))) +
                     sin(2/(365*0.8)*3.14*seq(1:length(treino)))+
                                    sqrt(seq(1:length(treino))^3))
summary(detrend_fit2)

plot(treino, type = "l")
lines(detrend_fit2$fitted.values, col='red')

# Precisamos verificar a sazonalidade agora.
# Tentando por um modelo de dummys:
deseas_fit <- lm(treino ~ cos(2/(365*3)*3.14*seq(1:length(treino))) +
                   sin(2/(365*0.8)*3.14*seq(1:length(treino)))+
                   sqrt(seq(1:length(treino))^3)+
                   seasonaldummy(ts(treino, frequency = 52)))
summary(deseas_fit)

plot(treino, type = "l")
lines(deseas_fit$fitted.values, col='red')

# Regressão harmônica:
deseas_fit2 <- lm(treino ~ cos(2/(365*3)*3.14*seq(1:length(treino))) +
                    sin(2/(365*0.8)*3.14*seq(1:length(treino)))+
                    sqrt(seq(1:length(treino))^3)+
                    harmonic(ts(treino, frequency = 52), 1))
summary(deseas_fit2)

plot(treino, type = "l")
lines(deseas_fit2$fitted.values, col='red')

# Checando a estrutura de dependência:
# modelo deseas_fit:
acf(deseas_fit$residuals, lag.max = 200)
pacf(deseas_fit$residuals, lag.max = 200)

# modelo deseas_fit2:
acf(deseas_fit2$residuals, lag.max = 200)
pacf(deseas_fit2$residuals, lag.max = 200)

# Vemos pela FAC e FACP que os dois modelos ainda não conseguiram captar toda 
# a dependência dos dados...
# Pelo summary do modelo 'deseas_fit2', as componentes de tendência e 
# sazonalidade são significativas, 

residuo <- ts(deseas_fit2$residuals, frequency = 52)
plot(residuo)

