# carregando pacotes
pacman::p_load(readr, tidyverse)

# carregando dados
df <- readRDS('dados_transf.rds')

# resumo dados
summary(df)

# Univariaviado ----

## variavel resposta ----
summary(df$wt)
plot(df$wt)
boxplot(df$wt)
hist(df$wt)
# os dados parecem distribuidos normalmente, com a presenca de outliers

## demais demais variaveis ----

### pluralty, so valores de feto unico
### outcome, so valores de nascido vivo q sobreviveu a 28 dias
### date, data de nascimento
### sex, so sexo masculino
# nao faz sentido usar

### gestation ----
# duracao da gestacao em dias
summary(df$gestation)
plot(df$gestation)
boxplot(df$gestation)
hist(df$gestation)
# 13 NA's, assimetria a esquerda, presenca de outliers
# concentracao entre 272 e 288

### parity ----
# numero total de gestações anteriores
summary(df$parity) 
hist(df$parity)
boxplot(df$parity)
# sem NA, forte assimetria a direita (lembra log), presenca de outliers

### race ----
# raca da mae
summary(df$race)
table(df$race)
prop.table(table(df$race)) * 100
# sem NA, 71% são brancas, seguido de quase 20% pretas

### age ----
# idade da mae ao final da gravidez, em anos
summary(df$age)
hist(df$age)
boxplot(df$age)
# 2 NA's, leve assimetria a direita, pouca presenca de outliers

### ed ----
# educacao da mae
summary(df$ed)
table(df$ed)
prop.table(table(df$ed)) * 100
# 1 NA, 36% formou no High School, 
# 24% High School + Ensino superior incompleto, 18% Ensino superior completo

### ht ----
# altura da mae em polegadas
summary(df$ht)
hist(df$ht)
boxplot(df$ht)
# 22 NA's, bem leve assimetria a esquerda, presenca de outliers

### wt ----
# peso da mae antes da gravidez em libras
summary(df$wt.1)
hist(df$wt.1)
boxplot(df$wt.1)
# 36 NA's, assimetria a direita e presenca de outliers

### drace ----
# raca do pai
summary(df$drace)
table(df$drace)
prop.table(table(df$drace)) * 100
# 15 NA's, 70% branco, 20% preto

### dage ----
# idade do pai
summary(df$dage)
hist(df$dage)
boxplot(df$dage)
# 7 NA's, assimetria a direita, presenca de outliers

### ded ----
# educacao do pai
summary(df$ded)
table(df$ded)
prop.table(table(df$ded)) * 100
# 13 NA's, 28% formou no High School e 28% Ensino superior completo
# 21% High School + Ensino superior incompleto

### dht ----
# altura do pai
summary(df$dht)
hist(df$dht)
boxplot(df$dht)
# 492 NA's, leve assimetria a esquerda, presenca de outliers

### dwt ----
# peso do pai
summary(df$dwt)
hist(df$dwt)
boxplot(df$dwt)
# 499 NA's , leve assimetria a direita, presenca de outliers

### marital ----
# estado civil
summary(df$marital)
table(df$marital)
prop.table(table(df$marital)) * 100
# 2 NA's 98% casados, 1% legalmente separados

### inc ----
# renda familiar anual, em incrementos de US$ 2.500
summary(df$inc)

### smoke ----
# mae fuma?
summary(df$smoke)
table(df$smoke)
prop.table(table(df$smoke)) * 100
# 10 NA's, 44% nunca fumou, 40% fuma, 8% até a gravidez
# 8% ja fumou, mas não mais

### time ----
# se a mae parou, a quanto tempo?
summary(df$time)
table(df$time)
prop.table(table(df$time)) * 100
# 15 NA's, 44% nunca fumou, 40% ainda fuma, 8% durante a gravidez

### number
# numero de cigarros fumados por dia por ex-fumantes e fumantes atuais 
summary(df$number)
table(df$number)
prop.table(table(df$number)) * 100
# 21 NA's, 44% nunca fumou, 16% 20-29 cigs/day, 14% 5-9 cigs/day, 13% 1-4 cigs/day

# Bivariaviado ----

## variaveis quantitativas ----
### variavel resposta x gestation ----
plot(wt ~ gestation, data = df)
# nao aparenta relcao linear, presenca de outliers

### variavel resposta x parity ----
plot(wt ~ parity, data = df)
# nao aparenta relacao linear, relacao decrescente

### variavel resposta x age ----
plot(wt ~ age, data = df)
# nao aparenta relacao linear, constante

### variavel resposta x ht ----
plot(wt ~ ht, data = df)
# nao aparenta relcao linear, presenca de outliers

### variavel resposta x wt.1 ----
plot(wt ~ wt.1, data = df)
# nao aparenta relacao linear, constante

### variavel resposta x dage ----
plot(wt ~ dage, data = df)
# nao aparenta relacao linear, constante

### variavel resposta x dht ----
plot(wt ~ dht, data = df)
# nao aparenta relacao linear, constante

### variavel resposta x dwt ----
plot(wt ~ dwt, data = df)
# nao aparenta relacao linear, constante

## correlograma
variav <- df[, c('gestation','parity','age','ht','wt.1','dage','dht','dwt')]
pairs(variav)
library(corrplot)
variav <- na.omit(variav)
corrplot(cor(variav), method = "square", type = "upper", 
         addCoef.col = "black", tl.col = "black", 
         tl.cex = 0.8, number.cex = 0.7)
# alta correlacao entre age e dage, podendo corresponder a multicolinearidade
# relacao moderada entre parity e age, dht e dwt, ht e wt.1

## variaveis qualitativas ----
### variavel resposta x race ----
boxplot(wt ~ race, data = df)
# mediana proxima, maior dispersao para brancos e pretos

### variavel resposta x ed ----
boxplot(wt ~ ed, data = df)
# medianas proximas, dispersao um pouco maiores em outras

### variavel resposta x drace ----
boxplot(wt ~ drace, data = df)
# semelhante ao da mae

### variavel resposta x ded ----
boxplot(wt ~ ded, data = df)
# o ultimo se destoa mais, maior dispersao dos dados

### variavel resposta x marital ----
boxplot(wt ~ marital, data = df)
# maior dispersao para os casados (concentracao tambem)

### variavel resposta x smoke ----
boxplot(wt ~ smoke, data = df)
# distribuicoes semelhantes entre as situacoes das fumantes

### variavel resposta x time ----
boxplot(wt ~ time, data = df)
# leve tentencia entre o tempo que parou

### variavel resposta x number ----
boxplot(wt ~ number, data = df)
# medianas proximas e distribuicoes relativamente semelhantes

