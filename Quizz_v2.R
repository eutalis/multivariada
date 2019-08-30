install.packages("haven")
library(haven)
setwd("C:/Users/Dell-pc/Downloads/1 - Mestrado/Aulas/2 semestre/Análise múltivariada")
hbat <- haven::read_sav("HBAT.sav")
hbat_quizz1<-hbat[,7:19]
names(hbat_quizz1) #saber se selecionou as variáveis corretas
summary(hbat_quizz1)
hist(hbat_quizz1$x9) #para ver a normalidade da variável
corvar<-cor(x=hbat_quizz1,y=hbat[,20]) #comando para fazer a correlação
corvar
max(corvar)
which.max(corvar)


lm1<-lm(x19 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17+ x18,data = hbat)
shapiro.test(lm1$residuals)
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