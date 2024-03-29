install.packages("haven")
library(haven)
setwd("C:/Users/b3001802/Downloads")
hbat <- haven::read_sav("HBAT.sav")
hbat_quizz1<-hbat[,7:19]
names(hbat_quizz1) #saber se selecionou as vari�veis corretas
summary(hbat_quizz1)
hist(hbat_quizz1$x9) #para ver a normalidade da vari�vel
corvar<-cor(x=hbat_quizz1,y=hbat[,20]) #comando para fazer a correla��o
corvar
max(corvar)
which.max(corvar)


lm1<-lm(x19 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17+ x18,data = hbat)
lm1
shapiro.test(lm1$residuals)
#regressao
step(lm1,direction = "both")
summary(lm1)

#melhor modelo
mmodelo<-lm(formula = x19 ~ x6 + x7 + x9 + x11 + x12 + x16, data = hbat)
summary(mmodelo)



install.packages("pwr")
library(pwr)

#analisar se a amostra � suficiente
#u grau de liberdade da regressao e v grau de liberdade do modelo de regressao
#100-14 = v
pwr.f2.test(u = 14 , v = 86 ,f2 = NULL, sig.level = 0.05, power = 0.8)



install.packages("e0171")
install.packages("car")

hist(hbat$x19)
boxplot(hbat$x19)
plot(lm1)

#teste de normalidade para amostra pequena
ks.test(hbat$x19,y=pnorm, mean=mean(hbat$x19), sd=sd(hbat$x19))


shapiro.test(hbat$x19) #teste de normalidade para n's grandes

install.packages("lmtest")
library(lmtest)

install.packages("olsrr")
library(olsrr)
install.packages("devtools")
library(devtools)

ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE)

#teste de homoscedasticidade

## 
##     Bartlett's Test of Homogenity of Variances    
## ------------------------------------------------
## Ho: Variances are equal across groups
## Ha: Variances are unequal for atleast two groups

summary(bptest(lm1))

step(lm1,direction = "both")