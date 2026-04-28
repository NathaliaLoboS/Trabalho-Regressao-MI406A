# carregando pacotes
pacman::p_load(readr, tidyverse)

# carregando dados
dados <- read.table("dados.txt", header = T, check.names = T) 

# resumo dados
summary(dados)

# Univariaviado ----

## variavel resposta ----
summary(dados$wt)
plot(dados$wt)
boxplot(dados$wt)

## demais variaveis ----

### pluralty
summary(dados$pluralty) # so valores 5 = feto unico

### outcome
summary(dados$outcome) # so valores 1 = nascido vivo q sobreviveu a 28 dias

### date 
# data de nascimento, nao peguei

### gestation
# duracao da gestacao em dias
summary(dados$gestation) # 999 deve ser desconhecido
plot(dados$gestation)
boxplot(dados$gestation)

### sex
# 1 = M, 2 = F, 9 = desconhecido
summary(dados$sex) # so tem M

### parity
# numero total de gestações anteriores
summary(dados$parity) # nao teve nenhum desconhecido
hist(dados$parity)
boxplot(dados$parity)

### race 
# raca da mae
summary(dados$race)

### age
# idade da mae ao final da gravidez, em anos
summary(dados$age) # 2 desconhecidos
hist(dados$age)
boxplot(dados$age)

### ed
# educacao da mae
summary(dados$ed)

### ht
# altura da mae em polegadas
summary(dados$ht)
hist(dados$ht)
boxplot(dados$ht)

### wt
# peso da mae antes da gravidez em libras
summary(dados$wt.1) # 1 desconhecido
hist(dados$wt.1)
boxplot(dados$wt.1)

### drace
# raca do pai
summary(dados$drace)

### dage
# idade do pai
summary(dados$dage) # valores desconhecidos
hist(dados$dage)
boxplot(dados$dage)

### ded
# educacao do pai
summary(dados$ded)

### dht
# altura do pai
summary(dados$dht)
hist(dados$dht)
boxplot(dados$dht)

### dwt
# peso do pai
summary(dados$dwt) 
hist(dados$dwt)
boxplot(dados$dwt)

### marital
# estado civil
summary(dados$marital)

### inc
# renda familiar anual, em incrementos de US$ 2.500
summary(dados$inc)

### smoke
# mae fuma?
summary(dados$smoke)

### time
# se a mae parou, a quanto tempo?
summary(dados$time)

### number
# numero de cigarros fumados por dia por ex-fumantes e fumantes atuais 
summary(dados$number)

### transformacoes ----
#### gestation
dados$gestation <- replace(dados$gestation, dados$gestation == 999, NA)
plot(dados$gestation)
boxplot(dados$gestation)

#### sex
dados$sex <- as.factor(dados$sex)
summary(dados$sex) 

#### race
dados$race <- dados$race %>% 
  replace(dados$race == 99, NA) %>% 
  factor(
    levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    labels = c("White", "White", "White", "White", "White", 
               "White", "Mexican", "Black", "Asian", "Mixed"))
table(dados$race)
prop.table(table(dados$race)) * 100

#### ed
dados$ed <- dados$ed %>% 
  replace(dados$ed == 9, NA) %>% 
  factor(
    levels = c(0, 1, 2, 3, 4, 5, 6, 7),
    labels = c("Less than 8th grade",
               "8th-12th grade (did not graduate)",
               "HS graduate (no other schooling)",
               "HS + trade school",
               "HS + some college",
               "College graduate",
               "Trade school HS unclear",
               "Trade school HS unclear"))
table(dados$ed)
prop.table(table(dados$ed)) * 100

#### age
dados$age <- replace(dados$age, dados$age == 99, NA)
hist(dados$age)
boxplot(dados$age)

#### ht
dados$ht <- replace(dados$ht, dados$ht == 99, NA)
hist(dados$ht)
boxplot(dados$ht)

#### wt
dados$wt.1 <- replace(dados$wt.1, dados$wt.1 == 999, NA)
hist(dados$wt.1)
boxplot(dados$wt.1)

#### drace
dados$drace <- dados$drace %>% 
  replace(dados$drace == 99, NA) %>% 
  factor(
    levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    labels = c("White", "White", "White", "White", "White", 
               "White", "Mexican", "Black", "Asian", "Mixed"))
table(dados$drace)
prop.table(table(dados$drace)) * 100

#### dage
dados$dage <- replace(dados$dage, dados$dage == 99, NA)
hist(dados$dage)
boxplot(dados$dage)

#### ded
dados$ded <- dados$ded %>% 
  replace(dados$ded == 9, NA) %>% 
  factor(
    levels = c(0, 1, 2, 3, 4, 5, 6, 7),
    labels = c("Less than 8th grade",
               "8th-12th grade (did not graduate)",
               "HS graduate (no other schooling)",
               "HS + trade school",
               "HS + some college",
               "College graduate",
               "Trade school HS unclear",
               "Trade school HS unclear"))
table(dados$ded)
prop.table(table(dados$ded)) * 100

#### dht
dados$dht <- replace(dados$dht, dados$dht == 99, NA)
hist(dados$dht)
boxplot(dados$dht)

#### dwt
dados$dwt <- replace(dados$dwt, dados$dwt == 999, NA)
hist(dados$dwt)
boxplot(dados$dwt)

#### marital
dados$marital <- dados$marital  %>% 
  factor(
    levels = c(1, 2, 3, 4, 5),
    labels = c("Married", "Legally Separated", "Divorced", 
               "Widowed", "Never Married"))
table(dados$marital)
prop.table(table(dados$marital)) * 100

#### inc
dados$inc <- dados$inc %>% 
  replace(dados$inc %in% c(98, 99), NA) %>% 
  factor(
    levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    labels = c("Under 2500", "2500-4999", "5000-7499", "7500-9999", 
               "10000-12499", "12500-14999", "15000-17499", "17500-19999",
               "20000-24999", "25000+"))
table(dados$inc)
prop.table(table(dados$inc)) * 100

#### smoke
dados$smoke <- dados$smoke %>% 
  replace(dados$smoke == 9, NA) %>% 
  factor(
    levels = c(0, 1, 2, 3),
    labels = c("Never", "Smokes now", "Until current pregnancy", 
               "Once did, not now"))
table(dados$smoke)
prop.table(table(dados$smoke)) * 100

#### time
dados$time <- dados$time %>% 
  replace(dados$time %in% c(9, 98, 99), NA) %>% 
  factor(
    levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8),
    labels = c("Never smoked", "Still smokes", "During current pregnancy", 
               "Within 1 year", "1 to 2 years ago", "2 to 3 years ago", 
               "3 to 4 years ago", "5 to 9 years ago", "10+ years ago"))
table(dados$time)
prop.table(table(dados$time)) * 100

### number 
dados$number <- dados$number %>% 
  replace(dados$number %in% c(9, 98, 99), NA) %>% 
  factor(
    levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8),
    labels = c("Never", "1-4 cigs/day", "5-9 cigs/day", "10-14 cigs/day",
               "15-19 cigs/day", "20-29 cigs/day", "30-39 cigs/day", 
               "40-60 cigs/day", "60+ cigs/day"))
table(dados$number)
prop.table(table(dados$number)) * 100

### salvando e exportando dados ----
saveRDS(dados, 'dados_transf.rds')
