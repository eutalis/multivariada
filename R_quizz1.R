install.packages("haven")
library(haven)
setwd("C:/Users/Dell-pc/Downloads/1 - Mestrado/Aulas/2 semestre/An�lise m�ltivariada")
hbat<-read_sav("hbat.sav")
hbat_quizz1<-hbat[,7:19]
names(hbat_quizz1) #saber se selecionou as vari�veis corretas
summary(hbat_quizz1)
hist(hbat_quizz1$x9) #para ver a normalidade da vari�vel
corvar<-cor(x=hbat_quizz1,y=hbat[,20]) #comando para fazer a correla��o
corvar
max(corvar)
which.max(corvar)

#regress�o linear entre x19 e x9

lm1<-lm(x19 ~ x6 + x7 + x9 + x11 + x12 + x16 + x17,data = hbat)
step(lm1,direction = "both")

#intercepto � a constante onde cruza o eixo y
# erro padr�o ou vari�vel madr�o: erro da variavel dependente 0.9554
# a m�dia � o proprio intercept

