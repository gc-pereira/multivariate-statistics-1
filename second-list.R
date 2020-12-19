# bibliotecas a serem utilizadas
library(tidyverse) # manipulacao de dataframes
library(MVN) # testes de normalidade
library(heplots) # teste de homogeneidade

# importando conjunto de dados
df1 <- read.csv("~/Desktop/multivariate/datasets/table_6_12.txt", header=TRUE, sep=";")

# renomeando as culunas com a nomenclatura do livro
df1 = df1 %>%
  rename(
    'X1' = colnames(df1)[2],
    'X2' = colnames(df1)[3],
    'X3' = colnames(df1)[4],
    'X4' = colnames(df1)[5],
  ) 

# verificando como ficou o cabeçalho do data frame
head(df1)

#dividindo o primeiro data frame em dois, por genero.
masc = df1 %>% 
  filter(
    gender == 1
  ) %>%
  select(!'gender')

fem  = df1 %>%
  filter(
    gender == 2
  ) %>%
  select(!'gender')

# teste de normalidade
mvn(data = df1[,-1], mvnTest = 'mardia', multivariatePlot = 'qq')
mvn(data = masc, mvnTest = 'royston', multivariatePlot = 'qq')$multivariateNormality
mvn(data = fem, mvnTest = 'mardia', multivariatePlot = 'qq')$multivariateNormality

# teste de homogeneidade
homogeneidade <- boxM(df1[,-1], df1[, "gender"])
homogeneidade

# matrizes de variancias e covariancias
s1 = cov(masc)
s2 = cov(fem)

# funcao para criar um vetor com as medias
mean_vector <- function(df){
  p <- length(colnames(df))
  v <- matrix(0, ncol = 1, nrow = p)
  i = 1
  while(i <= p){
    v[i] = colMeans(df)[i]
    i = i + 1
  }
  return(v)
}

mu_masc <- mean_vector(masc)
mu_fem <- mean_vector(fem)

# estatística do teste t quadrado
t2 <- t(mu_masc - mu_fem) %*% solve((1/25)*s1 + (1/25)*s2) %*% (mu_masc - mu_fem)
t2

tr <- function(m){
  return(sum(diag(m)))
}

# funcao para encontrar o valor de v presente no ajuste dos graus de liberdade
# para o teste de diferença de medias para duas populacoes de tamanho de amostra
# pequenos.
v_two_pop <- function(s1, s2, n1, n2){
  p <- ncol(s1)
  n <- c(n1,n1)
  soma <- c()
  a <- ((1/n[1]) * s1) %*% solve((1/n[1])*s1 + (1/n[1])*s2)
  b <- ((1/n[2]) * s2) %*% solve((1/n[2])*s1 + (1/n[2])*s2)
  soma[1] <- (1/n[1])*(tr(a)^2 + tr(a %*% a))
  soma[2] <- (1/n[1])*(tr(b)^2 + tr(b %*% b))
  numerador <- p + p^2
  denominador <- sum(soma)
  v <- numerador/denominador
  return(v)
}

v <- v_two_pop(s1,s2,25,25)
v

c <- (v*4)/(v - 4 + 1)*qf(0.95, 4, v - 4 + 1)
c

# spooled para regioes de confianca de bartllet
spooled = (1/2)*(s1+s2)
spooled

# diferenaca das medias
dif <- mu_masc - mu_fem

# autovalores e autovetores de spooled
lambda = eigen(spooled)$values
lambda = as.matrix(lambda)

# intervalos de confiança
ic <- function(dif, lambda, n1, n2, p){
  c_squared <- (((n1 + n2 - 2)*p)/(n1 + n2 - p - 1))*qf(0.95, p, n1 + n2 - p - 1)
  c <- sqrt(c_squared)
  l_inf <- c()
  l_sup <- c()
  for(i in 1:4){
    inf <- dif[i] - c*sqrt((1/n1) + (1/n2))*sqrt(spooled[i,i])
    sup <- dif[i] + c*sqrt((1/n1) + (1/n2))*sqrt(spooled[i,i])
    if(inf > sup){
      l_sup[i] = inf
      l_inf[i] = sup
    } else{
      l_sup[i] = sup
      l_inf[i] = inf
    }
  }
  return(matrix(c(l_inf, l_sup), ncol = 2))
}

ic_bonferroni <- function(dif, lambda, n1, n2, p, alpha, spooled){
  sig <- alpha/(2*p)
  l_inf <- c()
  l_sup <- c()
  for(i in 1:p){
    inf <- dif[i] - qt(sig, n1+n2-2)*sqrt((1/n1) + (1/n2))*sqrt(spooled[i,i])
    sup <- dif[i] + qt(sig, n1+n2-2)*sqrt((1/n1) + (1/n2))*sqrt(spooled[i,i])
    if(inf > sup){
      l_sup[i] = inf
      l_inf[i] = sup
    } else{
      l_sup[i] = sup
      l_inf[i] = inf
    }
  }
  return(matrix(c(l_inf, l_sup), ncol = 2))
}

ic(dif = dif, lambda = lambda, n1 = 25, n2 = 25, p = 4)
ic_bonferroni(dif = dif, lambda = lambda, n1 = 25, n2 = 25, p = 4, alpha = 0.05, spooled)

# coeficiente de combinacao linear
solve(spooled)%*%(mu_fem - mu_masc)

df3 <- read.csv('~/Desktop/multivariate/datasets/table_6_17.txt', sep = ';', header = T)
head(df3)

df_location_2 = df3 %>%
  filter(location == 2) %>%
  select(!'variety')
df_location_2  = df_location_2[, -1]

df_variety_1 = df3 %>%
  filter(variety == 5) %>%
  select(!'location')
df_variety_1 = df_variety_1[,-2]

df_variety_2 = df3 %>%
  filter(variety == 6) %>%
  select(!'location')
df_variety_2 = df_variety_2[,-2]

df_variety_3 = df3 %>%
  filter(variety == 8) %>%
  select(!'location')
df_variety_3 = df_variety_3[,-2]

mu_location2 <- mean_vector(df_location_2)
mu_variety1 <- mean_vector(df_variety_1)
mu_variety2 <- mean_vector(df_variety_2)
mu_variety3 <- mean_vector(df_variety_3)

s2_loc <- cov(df_location_2)
s5_variety <- cov(df_variety_1)
s6_variety <- cov(df_variety_2)
s8_variety <- cov(df_variety_3)

spooled_loc <- function(s1,s2,n1,n2){
  coef1 <- (n1-1)/(n1+n2-2)
  coef2 <- (n2-1)/(n1+n2-2)
  spooled <- (coef1 * s1) + (coef2 * s2)
  return(spooled)
}

spooled_loc(s2_loc, s5_variety, 6, 4)
ic_bonferroni(
              dif = mu_location2 - mu_variety1, 
              lambda = eigen(spooled_loc(s2_loc, s5_variety, 6, 4))$values, 
              n1 = 6, 
              n2 = 4, 
              p = 3, 
              alpha = 0.05, 
              spooled = spooled_loc(s2_loc, s5_variety, 6, 4)
              )
