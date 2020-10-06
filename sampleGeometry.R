data(iris)
iris

#vetores coluna das variáveis como a espécie setosa é as 50 primeiras observações, 
#segue que
sepalLength <- matrix(iris[1:50,]$Sepal.Length, ncol = 1)
sepalWidth <- matrix(iris[1:50,]$Sepal.Width, ncol = 1)
petalLength <- matrix(iris[1:50,]$Petal.Length, ncol = 1)
petalWidth <- matrix(iris[1:50,]$Petal.Width, ncol = 1)

#matriz de 1's
matrizDeUm <- matrix(rep(1, length(sepalLength)), ncol = 1)

#A matriz X, é tal que nas colunas temos as variáveis e nas linhas temos
#as observações de cada variável
x <- matrix(
  c(sepalLength,sepalWidth,petalLength, petalWidth), 
  nrow = length(sepalLength)
  )

x

#vetor de médias, a título de curiosidade fiz a multiplicação utilizando
#a matriz de 1 e depois utilizei o metodo mean(args) para conferir
xBarraTeste <- matrix(
  c(mean(sepalLength),mean(sepalWidth),mean(petalLength), mean(petalWidth)), 
  ncol = 1
  )

xBarra <- (1/length(sepalLength))*t(x)%*% matrizDeUm
xBarra #Devemos considerar este
xBarraTeste

#Achando a matriz de variâncias e covariâncas e a matriz R
dSL <- sepalLength -  xBarra[1,1]*matrizDeUm
dSW <- sepalWidth - xBarra[2,1]*matrizDeUm
dPL <- petalLength - xBarra[3,1]*matrizDeUm
dPW <- petalWidth - xBarra[4,1]*matrizDeUm

s <- var(x)*49/50
R <- cor(x)

#Variancia generalizada
varGeneralizada <- det(s)
varTotal <- sum(diag(s))



###### QUESTÃO 4 #######
c1 = (7/10)*iris[1:50,]$Sepal.Length + (7/10)*iris[1:50,]$Sepal.Width + (1/10)*iris[1:50,]$Petal.Length + (1/10)*iris[1:50,]$Petal.Width
c2 = (6/10)*iris[1:50,]$Sepal.Length - (6/10)*iris[1:50,]$Sepal.Width + (5/10)*iris[1:50,]$Petal.Length + (1/10)*iris[1:50,]$Petal.Width
c3 = (4/10)*iris[1:50,]$Sepal.Length - (3/10)*iris[1:50,]$Sepal.Width - (8/10)*iris[1:50,]$Petal.Length - (2/10)*iris[1:50,]$Petal.Width
c4 = (2/10)*iris[1:50,]$Sepal.Length - (1/10)*iris[1:50,]$Petal.Width

c = cbind(c1,c2,c3,c4)
mediac = colMeans(c)
varc1 = matrix(c(var(c1), var(c2), var(c3), var(c4)), ncol = 1)
varc = cbind(c("c1", "c2", "c3","c4"),varc1)
covc = cov(c)
corc = cor(c)

###### INTEPRETAÇÃO VARIANCIA GENERALIZADA ##########
'''
A variância generalizada é a o volume de um elipsoide centrado na média, tal que o volume
do elipsoide ao quadrado é igual a uma constante multiplicada pelo valor da variancia 
generalizada e os autovalores e autovetores da matriz de variâncias e covariâncias descrevem
o padrão no gráfico de pontos. Para um tamanho de amostra fixo o volume, ou S, aumentará 
a medida que os desvios ao quadrado forem aumentado.

'''
