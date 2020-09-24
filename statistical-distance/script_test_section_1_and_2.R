dados <- matrix(c(
  337260.0,    -21924.0,	52496.0,
  120470.0,	2406.4,	    10362.3,
  88275.0,	    219.0,	    30464.0,
  67736.3,	    1251.2,	    3174.4,
  65525.0,	    1760.0,  	4827.0,
  65092.7,	    1351.6,  	4911.3,
  46031.4,	    726.3,   	4883.2,
  42546.3,	    1488.4,  	5091.9,
  39083.0,	    292.0,   	3791.0,
  38079.8,	12362.0,	    18219.1), 
  nrow = 10, ncol = 3, byrow = TRUE)

Empresas <- c("Petrobras", "JBS", "Vale", "Ultrapar", 
              "GPA", "Raizen", "Braskem", "Gerdau", 
              "Cosan", "AmBev")

X <- data.frame(Empresa = Empresas, 
                ReceitaLiq = dados[,1]/1000,
                LucroLiq = dados[,2]/1000,
                EBITDA = dados[,3]/1000
)

cor(X[,4],X[,3])

s11 = var(X[,3])*((length(X[,3])-1)/length(X[,3]))
s22 = var(X[,4])*((length(X[,4])-1)/length(X[,4]))
s12 = cov(X[,4],X[,3])

descobrirAnguloCosseno <- function(cor){
  angulo <- acos(cor)
  return(angulo)
}

matrizDeA <- function(angulo, s11,s12,s22){

  aUmUM <- function(angulo, s11,s12,s22){
    numerador1 <- cos(angulo)^2
    denominador1 <- (cos(angulo)^2)*s11 + 2*sin(angulo)*cos(angulo)*s12 + s22*sin(angulo)^2
    numerador2 <- sin(angulo)^2
    denominador2 <- (s22*cos(angulo)^2) - 2*sin(angulo)*cos(angulo)*s12 + s11*sin(angulo)^2
    resultado1 <- (numerador1/denominador1) + (numerador2/denominador2)
    return(resultado1)
  }
  
  aDoisDois<- function(angulo, s11,s12,s22){
    numerador1 <- sin(angulo)^2
    denominador1 <- (cos(angulo)^2)*s11 + 2*sin(angulo)*cos(angulo)*s12 + s22*sin(angulo)^2
    numerador2 <- cos(angulo)^2
    denominador2 <- (s22*cos(angulo)^2) - 2*sin(angulo)*cos(angulo)*s12 + s11*sin(angulo)^2
    resultado2 <- (numerador1/denominador1) + (numerador2/denominador2)
    return(resultado2)
  }
  
  aUmDois<- function(angulo, s11,s12,s22){
    numerador1 <- sin(angulo)*cos(angulo)
    denominador1 <- (cos(angulo)^2)*s11 + 2*sin(angulo)*cos(angulo)*s12 + s22*sin(angulo)^2
    numerador2 <- sin(angulo)*cos(angulo)
    denominador2 <- (s22*cos(angulo)^2) - 2*sin(angulo)*cos(angulo)*s12 + s11*sin(angulo)^2
    resultado3 <- (numerador1/denominador1) - (numerador2/denominador2)
    return(resultado3)
  }
  A <- matrix(
    c(
      aUmUM(angulo,s11,s12,s22),
      aUmDois(angulo,s11,s12,s22),
      aUmDois(angulo,s11,s12,s22),
      aDoisDois(angulo,s11,s12,s22)
      ),
    nrow = 2,
    byrow = TRUE
    )
  return(A)
}

distancia <- function(x, y){ 
  return(sqrt(
    matrizDeA(descobrirAnguloCosseno(2/sqrt(12)),6,4,8)[1,1]*((1.76-1.3516)^2) + 
    matrizDeA(descobrirAnguloCosseno(2/sqrt(12)),6,4,8)[1,2]*((1.76-1.3516)*(4.827-4.9113))+
    matrizDeA(descobrirAnguloCosseno(2/sqrt(12)),6,4,8)[2,2]*((4.827-4.9113)^2)
  ))
}

########### Exercício 2 ############
brasken=c(65.525,1.76,4.827)
ultrapar=c(67.7363,1.2512,3.1744)
v <- ultrapar-brasken
sum(abs(v))