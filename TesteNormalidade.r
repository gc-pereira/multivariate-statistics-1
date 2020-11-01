# Testes de Normalidade

# Dados de radiacao (Exemplo 4.10, Tab 4.1)
radiacao <- scan(nmax = 42)
0.15 0.09 0.18 0.10
0.05 0.12 0.08 0.05
0.08 0.10 0.07 0.02
0.01 0.10 0.10 0.10
0.02 0.10 0.01 0.40
0.10 0.05 0.03 0.05
0.15 0.10 0.15 0.09
0.08 0.18 0.10 0.20
0.11 0.30 0.02 0.20
0.20 0.30 0.30 0.40
0.30 0.05

# Histograma com Densidade
hist(radiacao, xlab = "Radiacaoo", breaks = "FD", border = 0, col = "tomato",
     main = "Histograma para radiacao de microondas com a porta fechada\n n=42", 
     )
lines(density(radiacao), lwd=2)

# qqplot
qqnorm(radiacao, main = "Q-Q Plot para radiacao")
qqline(radiacao)

# qqplot da biblioteca car
car::qqPlot(radiacao, pch=19, main = "Q-Q Plot para radiação")

# Testes de normalidade
library(nortest)
shapiro.test(radiacao)
lillie.test(radiacao)
ad.test(radiacao)



# Dados Iris, (Fisher, 1937)
install.packages("ellipse")
install.packages("doBy")
dadosiris <- iris
head(dadosiris)
summary(dadosiris)
doBy::summaryBy(dadosiris[,-5] ~ Species, data = dadosiris, FUN = c(mean, sd))

ellipse::pairs(dadosiris[,-5], col=c(dadosiris[,5]), pch=19 )
ellipse::plotcorr(cor(dadosiris[,-5]), type = "lower")



# Chi-square Plot Multivariado:
install.packages("MVN")
library(MVN)      
mvn(dadosiris[,-5], mvnTest = "mardia", multivariatePlot = "qq",
    univariateTest = "AD")

mvn(dadosiris[,-5], mvnTest = "royston", multivariatePlot = "qq")


# separando por especies...
mvn(data = dadosiris, mvnTest = "royston", multivariatePlot = "qq",
    subset = "Species")