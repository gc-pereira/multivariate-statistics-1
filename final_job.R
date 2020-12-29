library(ggplot2)
library(ggpubr)
library(rstatix)
library(nortest)
library(car)
library(broom)
library(kableExtra)
library(readxl)
library(ggthemes)
library(magrittr)
library(dplyr)
library(tidyverse)
library(MASS)
library(gghighlight)
library(lmtest)
library(MVN)
library(ggcorrplot)
library(reshape2)
library(Hotelling)
library(GGally)
library(vegan)

# importando os dados 

BAHIA_BF <- read_excel("Desktop/multivariate/datasets/BAHIA_BF.xlsx")

COD_IBGE = as.factor(BAHIA_BF$COD_IBGE)
COD_IBGE2 = as.factor(BAHIA_BF$COD_IBGE2)
NOME = as.character(BAHIA_BF$NOME)
TAXA_BF = as.numeric(BAHIA_BF$TAXA_BF)
T_MULHERES = as.numeric(BAHIA_BF$T_MULHERES)
T_BRANCOS = as.numeric(BAHIA_BF$T_BRANCOS)
T_UNIRESPH = as.numeric(BAHIA_BF$T_UNIRESP_CONJ_FILHOS_PARENTES)
T_ANALF = as.numeric(BAHIA_BF$T_ANALF)
E_ANOSESTUDO = as.numeric(BAHIA_BF$E_ANOSESTUDO)
P_FORMAL = as.numeric(BAHIA_BF$P_FORMAL)
T_DES18M = as.numeric(BAHIA_BF$T_DES18M)
T_P_PF = as.numeric(BAHIA_BF$T_PESSOAS_PFAMILIA)
PROP_MC = as.numeric(BAHIA_BF$PROP_MULHERCHEFE)
T_PEA = as.numeric(BAHIA_BF$T_PEA)
T_DEF_AUD = as.numeric(BAHIA_BF$T_DEF_AUD)
T_DEF_MOT = as.numeric(BAHIA_BF$T_DEF_MOT)
T_DEF_VIS = as.numeric(BAHIA_BF$T_DEF_VIS)
T_DEF_MENT = as.numeric(BAHIA_BF$T_DEF_MENT)
T_AGUA = as.numeric(BAHIA_BF$T_AGUA)
T_BANAGUA = as.numeric(BAHIA_BF$T_BANAGUA)
T_DENS = as.numeric(BAHIA_BF$T_DENS)
T_LIXO = as.numeric(BAHIA_BF$T_LIXO)
T_LUZ = as.numeric(BAHIA_BF$T_LUZ)
AGUA_ESGOTO = as.numeric(BAHIA_BF$AGUA_ESGOTO)
PAREDE = as.numeric(BAHIA_BF$PAREDE)
EMP = as.numeric(BAHIA_BF$EMP)
POP2010 = as.numeric(BAHIA_BF$POP2010)
GINI = as.numeric(BAHIA_BF$GINI)


dados = data.frame(COD_IBGE, COD_IBGE2, NOME, Y1 = TAXA_BF, Y2 = P_FORMAL, X1 = T_MULHERES, 
                   X2 = T_BRANCOS, X3 = T_UNIRESPH, X4 = T_ANALF, X5 = E_ANOSESTUDO, 
                   X6 = T_DES18M, X7 = T_P_PF, X8 = PROP_MC, X9 = T_PEA,
                   X10 = T_DEF_AUD, X11 = T_DEF_MOT, X12 = T_DEF_VIS, X13 = T_DEF_MENT,
                   X14 = T_AGUA, X15 = T_BANAGUA, X16 = T_DENS, X17 = T_LIXO, X18 = T_LUZ,
                   X19 = AGUA_ESGOTO, X20 = PAREDE, X21 = EMP, X22 = POP2010, X23 = GINI)

# Explorando os dados
summary(dados)

# Correlação entre variáveis
library(corrgram)
matrizcor<-ggcorr(dados[,-c(1:3)], 
                  method = c("everything", "pearson")) 

round(cor(dados[,4:28]), digits = 5)

corr_cross(dados, # name of dataset
           max_pvalue = 0.9, # display only significant correlations (at 5% level)
           top = 10, # display top 10 couples of variables (by correlation coefficient)
           grid = F
)

# Boxplot de todas as variáveis originais
library(RColorBrewer)

boxplots = reshape2::melt(scale(dados[,-c(1,2,3)]))
boxtodas <- boxplots %>%
  ggplot(aes(x = Var2, y = value))+
  geom_boxplot(aes(fill = Var2))+
  scale_colour_manual(values = c("#8dd3c7","#ffffb3","#bebada","#fb8072",
                                 "#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5",
                                 "#ffed6f","#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c",
                                 "#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99")) +
  labs(title = "Boxplot das Variáveis",
       y = NULL,
       x = "Valor da Variável")+
  coord_flip() +
  theme_light()+
  theme( plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 12), 
         axis.text = element_text(size = 10), title =  element_text(size = 16) )

## Averiguemos as hipóteses de normalidade e homogeneidade de variâncias 
## a partir dos resíduos do modelo linear multivariado associado a matriz de 
## variável resposta Y, tal que Y1 = TAXA_BF e Y2 = P_FORMAL. 
## Obtém-se primeiramente a matriz de resíduos:
modelo1 <- lm(cbind(Y1, Y2) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10
              + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + 
                X21 + X22 + X23, data = dados)
res1 <- as.data.frame(resid(modelo1))
head(res1)
## Testemos a normalidade univariada e multivariada para tais resíduos, 
## tendo primeiramente os qqplots:
ggplot.data = reshape2::melt(res1)
qqnorm1<-ggplot.data %>%
  ggqqplot("value", facet.by = "variable", panel.labs = list(variable=c("TAXA_BF","P_FORMAL")),
           ylab = "Resíduos", xlab = "Quantis Teóricos", ggtheme = theme_classic(),
           scale = "free")+
  theme( plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 12), 
         axis.text = element_text(size = 10), title =  element_text(size = 16))+
  stat_qq_line( size = 1, col = "red", lty = 1)+
  stat_qq(alpha = 0.8, col = "Black")


## Nota-se pelos qqplots um certo desvio de diversos pontos no canto superior 
## direito do gráfico, principalmente nos resíduos associados a variável 
## "P_FORMAL", o que pode nos dar indicios de certos problemas de normalidade 
## com relação a tal variável.Já a outra variável parece apresentar um bom 
##comportamento, apenas com pequenos desvios no canto superior direito do gráfico. Realiza-se os testes de normalidade univariada de anderson-darling, shapiro-wilk,lilliefors, cramer von-misses, shapiro-francia como abaixo, tomando nivel de significância $\alpha = 0.05$:

taxa_bf.ad = ad.test(res1$Y1)$p.value; taxa_bf.ad
taxa_bf.sw = shapiro.test(res1$Y1)$p.value; taxa_bf.sw
taxa_bf.lillie = lillie.test(res1$Y1)$p.value; taxa_bf.lillie
taxa_bf.cvm =  cvm.test(res1$Y1)$p.value; taxa_bf.cvm
taxa_bf.sf = sf.test(res1$Y1)$p.value; taxa_bf.sf

p_formal.ad = ad.test(res1$Y2)$p.value; p_formal.ad
p_formal.sw = shapiro.test(res1$Y2)$p.value; p_formal.sw
p_formal.lillie = lillie.test(res1$Y2)$p.value; p_formal.lillie
p_formal.cvm =  cvm.test(res1$Y2)$p.value; p_formal.cvm
p_formal.sf = sf.test(res1$Y2)$p.value; p_formal.sf

tests.data = data.frame(test = rep(c("Anderson-Darling", "Shapiro-Wilk",
                                     "Lilliefors", "Cramer Von Misses", "Shapiro-Francia"), 2),
                        variable = c(rep("TAXA_BF", 5), rep("P_FORMAL", 5)),
                        p_values = c(taxa_bf.ad, taxa_bf.sw, taxa_bf.lillie, taxa_bf.cvm,
                                     taxa_bf.sf, p_formal.ad, p_formal.sw, p_formal.cvm,
                                     p_formal.lillie, p_formal.sf))
tests.data

## Ou seja, pelos testes de normalidade univariada, parece nao haver 
## indicios de normalidade univariada em um aspecto geral para a variável
## "P_FORMAL", sendo interessante a realização de certa transformação, 
## enquanto a variável "TAXA_BF", apesar de rejeição em 2 dos 5 testes, 
## parece não ter tantos desvios de normalidade, ainda mais que um dos 
## testes teve uma margem de rejeição curta, estando proximo de $0.05$, 
## além do qqplot também ter um comportamento relativamente adequado. 
## Pode-se averiguar também a normalidade multivariada dos resíduos, 
## primeiramente pelo seguinte gráfico:

mvn(res1, mvnTest = "royston", multivariatePlot = "qq")

## Apesar de um relativo bom comportamento, nota-se que a medida que se avança 
## mais para a direita do gráfico, os pontos tendem a se dispersar 
## consideravelmente da reta, tendo um comportamento semelhante ao
## observados nos qqplots anteriormente. Assim, parece haver certos indicios 
## de normalidade multivariada devido ao excelente comportamento da maioria dos 
## pontos, porém os pontos divergentes podem contrariar tais indicios devido a
## seus desvios. Investiga-se melhor tal analise a partir dos testes de 
## normalidade multivariada de Mardia, Henze-Zinkler, Royston e Doornik-Hansen, obtendo-se:

# Mardia
mardia = mvn(res1, mvnTest = "mardia")$multivariateNormality
colnames(mardia) = c("Teste", "Estatistica do teste", "p-valor", "MVN")
mardia$`Estatistica do teste` = as.numeric(as.character(mardia$`Estatistica do teste`))
mardia$`p-valor` = as.numeric(as.character(mardia$`p-valor`))
mardia = mardia %>%
  filter(Teste != "MVN")

# Henze-Zinkler
hz = mvn(res1, mvnTest = "hz")$multivariateNormality
colnames(hz) = c("Teste", "Estatistica do teste", "p-valor", "MVN")


# Royston
royston = mvn(res1, mvnTest = "royston")$multivariateNormality
colnames(royston) = c("Teste", "Estatistica do teste", "p-valor", "MVN")


# Doornik-Hansen:

dh = mvn(res1, mvnTest = "dh")$multivariateNormality
colnames(dh) = c("Teste", "Estatistica do teste", "Graus de liberdade", 
                 "p-valor", "MVN")
dh

## adicionar dh !!
all_data = rbind(mardia, royston)
all_data = rbind(all_data, hz)
row.names(all_data) = 1:nrow(all_data)
all_data

## Ou seja, nota-se pelos testes que parece não termos normalidade multivariada
## para o conjunto de dados da maneira que este se apresenta. 
## Para contornar tal problema, pode-se optar pela transformação de box-cox 
## em Y1 e Y2 separadamente, obtendo-se:

## obtendo lambda para a transformação de P_FORMAL e de TAXA_BF
lambda_p_formal = powerTransform(dplyr::select(dados, c(Y2)), 
                                 family = "bcnPower")$lambda
lambda_taxa_bf = powerTransform(dplyr::select(dados, c(Y1)), 
                                family = "bcnPower")$lambda
## transformando
new_p_formal = bcnPower(dados$Y2, lambda_p_formal, gamma = 50)
new_taxa_bf = bcnPower(dados$Y1, lambda_taxa_bf, gamma = 10)
dados$Y2 = new_p_formal
dados$Y1 = new_taxa_bf
modelo2 <- lm(cbind(Y1, Y2) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10
              + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + 
                X21 + X22 + X23, data = dados)
res2 = as.data.frame(resid(modelo2))
colnames(res2) = c("Y1", "Y2")
head(res2)
summary(modelo2)

Manova(modelo2, type = "II", test.statistic = "Wilks")

ggplot.data = reshape2::melt(res2)
qqnorm2<-ggplot.data %>%
  ggqqplot("value", facet.by = "variable",
           ylab = "Residuos", xlab = "Quantis teóricos", panel.labs = list(variable=c("Y1","Y2")), ggtheme = theme_classic(),
           scale = "free")+
  theme( plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 12), 
         axis.text = element_text(size = 10), title =  element_text(size = 16))+
  stat_qq_line( size = 1, col = "red", lty = 1)+
  stat_qq(alpha = 0.8, col = "Black")


## Com os testes:

taxa_bf.ad = ad.test(res2$TAXA_BF)$p.value; taxa_bf.ad
taxa_bf.sw = shapiro.test(res2$TAXA_BF)$p.value; taxa_bf.sw
taxa_bf.lillie = lillie.test(res2$TAXA_BF)$p.value; taxa_bf.lillie
taxa_bf.cvm =  cvm.test(res2$TAXA_BF)$p.value; taxa_bf.cvm
taxa_bf.sf = sf.test(res2$TAXA_BF)$p.value; taxa_bf.sf

p_formal.ad = ad.test(res2$P_FORMAL)$p.value; p_formal.ad
p_formal.sw = shapiro.test(res2$P_FORMAL)$p.value; p_formal.sw
p_formal.lillie = lillie.test(res2$P_FORMAL)$p.value; p_formal.lillie
p_formal.cvm =  cvm.test(res2$P_FORMAL)$p.value; p_formal.cvm
p_formal.sf = sf.test(res2$P_FORMAL)$p.value; p_formal.sf


tests.data = data.frame(test = rep(c("Anderson-Darling", "Shapiro-Wilk",
                                     "Lilliefors", "Cramer Von Misses", "Shapiro-Francia"), 2),
                        variable = c(rep("TAXA_BF", 5), rep("P_FORMAL", 5)),
                        p_values = c(taxa_bf.ad, taxa_bf.sw, taxa_bf.lillie, taxa_bf.cvm,
                                     taxa_bf.sf, p_formal.ad, p_formal.sw, p_formal.cvm,
                                     p_formal.lillie, p_formal.sf))
tests.data


mvn(res2, mvnTest = "royston", multivariatePlot = "qq")


# mardia
mardia = mvn(res2, mvnTest = "mardia")$multivariateNormality
colnames(mardia) = c("Teste", "Estatistica do teste", "p-valor", "MVN")
mardia$`Estatistica do teste` = as.numeric(as.character(mardia$`Estatistica do teste`))
mardia$`p-valor` = as.numeric(as.character(mardia$`p-valor`))
mardia = mardia %>%
  filter(Teste != "MVN")

# hz
hz = mvn(res2, mvnTest = "hz")$multivariateNormality
colnames(hz) = c("Teste", "Estatistica do teste", "p-valor", "MVN")


# royston
royston = mvn(res2, mvnTest = "royston")$multivariateNormality
colnames(royston) = c("Teste", "Estatistica do teste", "p-valor", "MVN")

## Doornik-Hansen;

dh = mvn(res2, mvnTest = "dh")$multivariateNormality
colnames(dh) = c("Teste", "Estatistica do teste", "Graus de liberdade", "p-valor", "MVN")
dh

## adicionar dh !!!
all_data = rbind(mardia, royston)
all_data = rbind(all_data, hz)
row.names(all_data) = 1:nrow(all_data)
all_data

## Assim, finaliza-se tal analise a partir do gráfico de residuos contra
## preditos para cada variável resposta, com o intuito de se analizar a 
## presença de homogeneidade marginal com relação a matriz de covariância dos
## resíduos, tendo:

ggplot.data = reshape2::melt(res2)
preds = reshape2::melt(modelo2$fitted.values)$value
ggplot.data$pred = preds
res_pred<-ggplot.data %>%
  ggplot(aes(x = preds, y = value))+
  geom_point(color = "SteelBlue")+
  labs(title = "Resíduos x Valores Preditos",
       x = "Valores Preditos",
       y = "Resíduos")+
  geom_hline(yintercept = 0, linetype = "longdash", color = "red", size = 1.1) +
  theme_classic()+
  facet_wrap(~variable, scale = "free")+
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10), title =  element_text(size = 16))

## Modelo ideal
library(carData)
library(car)
modelo3 <- update(modelo2, . ~ . - X2 - X3 - x5 - X7 - X8 - X11 - X12
                  - X13 - X14 - X17 - X18 - X19 - X20 - X22)

modelo3 <- update(modelo2, . ~ . - X2 - X3 - X7 - X8 - X11
                  - X13 - X14 - X17 - X18 - X19 - X20)

modelo3 <- update(modelo2, . ~ . - X1 - X2 - X3 - X6 - X7 - X8 - X9 - X10 - X11 - X12
                  - X13 - X14 - X17 - X18 - X19 - X20 - X22)

modelo3 <- update(modelo2, . ~ . - X2 - X3 - X11
                  - X13 - X14 - X17 - X18 - X19 - X20) ## da significativo

modelo3 <- update(modelo2, . ~ . - X2 - X3 - X8 - X11
                  - X13 - X14 - X17 - X18 - X19 - X20)

modelo3 <- lm(cbind(Y1, Y2) ~ X1+X10 + X15+X16 + (X4*X9) +X6+X21+X22+X23, data = dados)

modelo3 <- lm(cbind(Y1,Y2)~X1+X4+X5+X6+X8+X9+X10+X12+X14+X15+X16+X19+X21+X22+X23, data = dados) ## da certo com 0.68

modelo3 <- lm(cbind(Y1,Y2)~(X1*X10*X12)+X14+X15+X16+X19+X22+X4+X5+X6+X8+X9+X21+X23, data = dados) ## da significativo 0.93

modelo3 <- lm(cbind(Y1,Y2)~X1+X10+X15+X16+X22+X4+X9+X8+X6+X21+X23, data = dados) ## da significativo mas da 0.06
modelo3 <- lm(cbind(Y1,Y2)~(X1*X10)+X15+X16+X22+(X4*X9)+X8+X6+X21+X23, data = dados) ## da significativo mas da 0.06
modelo3 <- lm(cbind(Y1,Y2)~(X1*X10)+X15+X16+X22+(X4*X9*X6)+X8+X21+X23, data = dados) ## da significativo mas da 1
########################################################################
library(rstatix) ## multicolinearidade
dados %>% cor_test(Y1,Y2)

########################################################################
modelo31 <- lm(cbind(Y1,Y2)~X1+X4+X5+X6+X8+X9+X12+X15+X16+X21+X22+X23, data=dados) ## roda 0.23

modelo31 <- lm(cbind(Y1,Y2)~X1+X4+X5+X6+X8+X9+X15+X16+X21+X22+X23, data=dados) ## roda 0.15 

modelo31 <- lm(cbind(Y1,Y2)~X1+X4+X5+X6+X8+X9+(X15*X16)+X21+X22+X23, data=dados) ## roda 0.54

modelo31 <- lm(cbind(Y1,Y2)~X1+X4+X8+(X6*X9)+X5+X22+X15+X16+X21+X23, data=dados) ## roda 0.57

modelo31 <- lm(cbind(Y1,Y2)~X1+X4+X8+X6+X9+X5+X22+X15+X16+(X21*X23), data=dados) ## roda 0.51

#modelo31 <- lm(cbind(Y1,Y2)~X1+X4+X5+X6+X8+X9+(X15*X16*X22)+X21+X23, data=dados) ## não roda

modelo31 <- lm(cbind(Y1,Y2)~X1+(X4*X8)+X6+X22+X5+X9+(X15*X16)+(X21*X23), data=dados) ## roda 1

modelo31 <- lm(cbind(Y1,Y2)~X1+(X4*X8)+(X6*X21)+X5+X9+(X15*X16)+X22+X23, data=dados) ## roda 0.98

modelo31 <- lm(cbind(Y1,Y2)~X1+(X4*X8)+(X6*X9)+X5+X21+(X15*X16)+X22+X23, data=dados) ## roda 0.99

modelo31 <- lm(cbind(Y1,Y2)~X1+(X4*X8)+(X6*X9)+X5+X21+(X15*X16)+X22+X23, data=dados) ## roda 0.99

modelo31 <- lm(cbind(Y1,Y2)~X1+X4+X8+(X6*X9)+X5+X22+(X15*X16)+(X21*X23), data=dados) ## roda 1
###
modelo31 <- lm(cbind(Y1,Y2)~X1+X4+X8+(X6*X9)+X5+X22+(X15*X16)+X21+X23, data=dados) ## roda 0.94 MODELO Q VAMOS USAR
###
#modelo31 <- lm(cbind(Y1,Y2)~X1*X4*X5*X6*X8*X9*X12*X15*X16*X21*X22*X23, data=dados) ## não roda 

#modelo31 <- lm(cbind(Y1,Y2)~X1+X4+X5+X6+X8+X9+X12+(X15*X16)+X21+X22+X23, data=dados) ## não roda 

#modelo31 <- lm(cbind(Y1,Y2)~(X1*X10*X12)+(X4*X5*X6*X8*X9*X21*X23)+(X15*X16*X22), data=dados) ## não roda

#modelo31 <- lm(cbind(Y1,Y2)~(X1*X10*X12)+X4+X5+X6+X8+X9+X21+X23+X15+X16+X22, data=dados) ## nao roda 

### com alpha de 1%
modelo31 <- lm(cbind(Y1,Y2)~X4+X6+X8+X9+X15+X16+X21+X22+X23, data=dados) ## roda 0.01

modelo31 <- lm(cbind(Y1,Y2)~X4+(X6*X9)+X8+(X15*X16)+X21+X22+X23, data=dados) ## roda 0.32

modelo31 <- lm(cbind(Y1,Y2)~X4+(X6*X9)+X8+(X15*X16)+X22+(X21*X23), data=dados) ## roda 0.77

modelo31 <- lm(cbind(Y1,Y2)~X4+(X6*X9)+(X8*X22)+(X15*X16)+(X21*X23), data=dados) ## não roda

modelo31 <- lm(cbind(Y1,Y2)~(X4*X8)+(X6*X9)+X22+(X15*X16)+(X21*X23), data=dados) ## roda 0.98

Manova(modelo31, type = "II", test.statistic = "Wilks")

anova(modelo31, modelo2, test = "Wilks")
########################################################################
summary(modelo3)

anova(modelo3, modelo2, test = "Wilks")

lh.out <- linearHypothesis(modelo2, 
                           hypothesis.matrix = c("X2 = 0", "X3 = 0",
                                                 "X7 = 0", "X11 = 0","X13 = 0",
                                                 "X17 = 0", "X18 = 0","X20 = 0"))

lh.out <- linearHypothesis(modelo2, 
                           hypothesis.matrix = c("X2 = 0", "X3 = 0", "X5 = 0",
                                                 "X7 = 0", "X11 = 0", "X12 = 0","X13 = 0",
                                                 "X14 = 0", "X17 = 0", "X18 = 0", "X19 = 0",
                                                 "X20 = 0"))
### o q vamos usar 
lh.out <- linearHypothesis(modelo2, 
                           hypothesis.matrix = c("X2 = 0", "X3 = 0","X7 = 0", "X10 = 0", "X11 = 0", 
                                                 "X12 = 0","X13 = 0","X14 = 0", "X17 = 0",
                                                 "X18 = 0", "X19 = 0","X20 = 0"))

E <- lh.out$SSPE
H <- lh.out$SSPH
det(E)/det(E + H)


e.out <- eigen(H %*% solve(E))
max(e.out$values)

## modelo final 
summary(modelo3)