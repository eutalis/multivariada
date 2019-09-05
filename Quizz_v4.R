install.packages("haven")
library(haven)
install.packages("car")
library(car)
install.packages("pwr")
library(pwr)
install.packages("lmtest")
library(lmtest)
setwd("C:/Users/Dell-pc/Downloads/1 - Mestrado/Aulas/2 semestre/Análise múltivariada")
hbat <- haven::read_sav("HBAT.sav")
hbat_quizz1<-hbat[,7:19] #selecionar variaveis escolhidas

#testes de suposição

hist(hbat$x19) #para analisar a normalidade da variável

shapiro.test(hbat$x19) #teste de normalidade para n's grandes

#analisar se a amostra é suficiente
#u = grau de liberdade da regressao (variáveis independentes + dependente) e v = grau de liberdade do modelo de regressao
# (tam da amostra - variaveis) 100-14 = v
pwr::pwr.f2.test(u = 14 , v = 86 ,f2 = NULL, sig.level = 0.05, power = 0.8)

#teste para homoscedasticidade
lmtest::bptest(lm1) # deve-se analisar o p-value
# P-value tenta fornnecer uma medida da força dos resultados de um teste,

car::plot(cooks.distance(lm1)) #grafico de outliers
title(main="Distância de Cook (lm1)")
abline(h=(4/(100-13-1)),col=gray(0.2),lty=2)

#montar modelo inicial
lm1<-lm(x19 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17+ x18,data = hbat)
step(lm1,direction = "both") #rodar regressão

#novo modelo (1)
modelo1<-lm(formula = x19 ~ x6 + x7 + x9 + x11 + x12 + x16, data = hbat)
summary(modelo1) #obter valor das variáveis e do R quadrado (e ajustado)

#realizar validação
sample_50<-hbat[sample(nrow(hbat),50),]
lm2<-lm(x19 ~ x6 + x7 + x9 + x11 + x12 + x16, data = sample_50)
step(lm2, direction = "both")

#novo modelo (2)
lm3<-(lm(x19 ~ x6 + x7 + x9 + x11 + x12, data = hbat))
summary(lm3) #obter valor das variáveis e do R quadrado (e ajustado)









