install.packages("haven")
library(haven)
install.packages("REdaS")
install.packages("psych")
library(REdaS)
library(grid)


setwd("C:/Users/Dell-pc/Downloads/1 - Mestrado/Aulas/2 semestre/An�lise m�ltivariada")
hbat <- haven::read_sav("HBAT.sav")
hbat_quizz2<-hbat[,7:19]

#Analise descritiva

#desvio padr�o
sapply(hbat_quizz2,sd)



#Est�gio 3
library(psych)
KMO(hbat_quizz2)
bart_spher(hbat_quizz2)

library(psych)
covar <- cor(hbat_quizz2)

#Extra��o x15
hbat_quizz2_1 <- hbat_quizz2[,-10]
names(hbat_quizz2_1)
KMO(hbat_quizz2_1)
cortest.bartlett(hbat_quizz2_1)


#Extra��o x17 criterio de ,50 para eliminar as variaveis
hbat_quizz2_2 <- hbat_quizz2_1[,-11]
names(hbat_quizz2_2)
KMO(hbat_quizz2_2)
cortest.bartlett(hbat_quizz2_2)


#Est�gio 4 determina��o de valores e ajuste geral
eigen(hbat_quizz2_2,TRUE,TRUE)



#Scree plot
scree(hbat_quizz2_2, factors = TRUE, pc = TRUE, main = "Scree Plot", hline = NULL,add = FALSE)
VSS.scree(hbat_quizz2_2, main = "scree plot")

#Escolher somente 4 fatores (autovalor > 1)
princomp(hbat_quizz2_2)
fit<-princomp(hbat_quizz2_2, cor = TRUE)
Summary(fit)
loadings(fit)

fit<-principal(hbat_quizz2_2, nfactors = 4)
fit #print results sem rota��o


fit<-principal(hbat_quizz2_2, nfactors = 4, rotate = "varimax")
fit #print results com rota��o








