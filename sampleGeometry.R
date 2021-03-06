data(iris)
iris

#vetores coluna das vari�veis como a esp�cie setosa � as 50 primeiras observa��es, 
#segue que
sepalLength <- matrix(iris[1:50,]$Sepal.Length, ncol = 1)
sepalWidth <- matrix(iris[1:50,]$Sepal.Width, ncol = 1)
petalLength <- matrix(iris[1:50,]$Petal.Length, ncol = 1)
petalWidth <- matrix(iris[1:50,]$Petal.Width, ncol = 1)

#matriz de 1's
matrizDeUm <- matrix(rep(1, length(sepalLength)), ncol = 1)

#A matriz X, � tal que nas colunas temos as vari�veis e nas linhas temos
#as observa��es de cada vari�vel
x <- matrix(
  c(sepalLength,sepalWidth,petalLength, petalWidth), 
  nrow = length(sepalLength)
  )

x

#vetor de m�dias, a t�tulo de curiosidade fiz a multiplica��o utilizando
#a matriz de 1 e depois utilizei o metodo mean(args) para conferir
xBarraTeste <- matrix(
  c(mean(sepalLength),mean(sepalWidth),mean(petalLength), mean(petalWidth)), 
  ncol = 1
  )

xBarra <- (1/length(sepalLength))*t(x)%*% matrizDeUm
xBarra #Devemos considerar este
xBarraTeste

#Achando a matriz de vari�ncias e covari�ncas e a matriz R
dSL <- sepalLength -  xBarra[1,1]*matrizDeUm
dSW <- sepalWidth - xBarra[2,1]*matrizDeUm
dPL <- petalLength - xBarra[3,1]*matrizDeUm
dPW <- petalWidth - xBarra[4,1]*matrizDeUm

s <- var(x)*49/50
R <- cor(x)

#Variancia generalizada
varGeneralizada <- det(s)
varTotal <- sum(diag(s))



###### QUEST�O 4 #######
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

###### INTEPRETA��O VARIANCIA GENERALIZADA ##########
'''
A vari�ncia generalizada � a o volume de um elipsoide centrado na m�dia, tal que o volume
do elipsoide ao quadrado � igual a uma constante multiplicada pelo valor da variancia 
generalizada e os autovalores e autovetores da matriz de vari�ncias e covari�ncias descrevem
o padr�o no gr�fico de pontos. Para um tamanho de amostra fixo o volume, ou S, aumentar� 
a medida que os desvios ao quadrado forem aumentado.

'''
