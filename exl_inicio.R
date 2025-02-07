
dEXL <- function(x, mu, sigma, log=FALSE){
  if(any(x<0))     stop("Parameter x has to be positive or zero")
  if(any(mu<0))    stop("Parameter mu has to be positive or zero")
  if(any(sigma<0)) stop("Parameter sigma has to be positive or zero")
  
  p1 <- (1 + ((mu*x)/(1+mu)^2))
  p2 <- exp(-mu*x)
  p3 <- (1-(p1*p2))^(sigma-1) 
  p4 <- ((sigma*(mu^2))*(2+mu+x)) / ((1+mu)^2)
  p5 <- exp(-mu*x)
  
  pdf <- p4*p5*p3
  
  if(log)
    pdf <- log(pdf)
  else
    pdf <- pdf
  
  return(pdf)
}
dEXL_vec <- Vectorize(dEXL)

par(mfrow=c(1,3))
curve(dEXL_vec(x, mu=0.5, sigma=0.5), 
      from=0.001, to=5,
      ylim=c(0, 1), 
      col="blue", lwd=2, 
      main="Función de densidad",
      xlab="x", ylab="f(x)")
curve(dEXL_vec(x, mu=1, sigma=0.5),
      col="red", 
      lwd=2,
      add=TRUE)
curve(dEXL_vec(x, mu=1.5, sigma=0.5),
      col="green",
      lwd=2,
      add=TRUE)
legend("topright", legend=c("mu=0.5, sigma=0.5", 
                            "mu=1.0, sigma=0.5",
                            "mu=1.5, sigma=0.5"),
       col=c("blue", "red", "green"), lwd=2, cex=0.6)


curve(dEXL_vec(x, mu=0.5, sigma=1), 
      from=0.001, to=5,
      ylim=c(0, 1), 
      col="blue", lwd=2, 
      main="Función de densidad",
      xlab="x", ylab="f(x)")
curve(dEXL_vec(x, mu=1, sigma=1),
      col="red", 
      lwd=2,
      add=TRUE)
curve(dEXL_vec(x, mu=1.5, sigma=1),
      col="green",
      lwd=2,
      add=TRUE)
legend("topright", legend=c("mu=0.5, sigma=1", 
                              "mu=1.0, sigma=1",
                              "mu=1.5, sigma=1"),
       col=c("blue", "red", "green"), lwd=2, cex=0.6)


curve(dEXL_vec(x, mu=0.5, sigma=1.5), 
      from=0.001, to=8,
      ylim=c(0, 1), 
      col="blue", lwd=2, 
      main="Función de densidad",
      xlab="x", ylab="f(x)")
curve(dEXL_vec(x, mu=1, sigma=1.5),
      col="red", 
      lwd=2,
      add=TRUE)
curve(dEXL_vec(x, mu=1.5, sigma=1.5),
      col="green",
      lwd=2,
      add=TRUE)
legend("topright", legend=c("mu=0.5, sigma=1.5", 
                              "mu=1.0, sigma=1.5",
                              "mu=1.5, sigma=1.5"),
       col=c("blue", "red", "green"), lwd=2, cex=0.6)




pEXL <- function(x, mu, sigma, log.p=FALSE, lower.tail=TRUE){
  if(any(x<0))     stop("Parameter x has to be positive or zero")
  if(any(mu<0))    stop("Parameter mu has to be positive or zero")
  if(any(sigma<0)) stop("Parameter sigma has to be positive or zero")
  
  p1 <- (1 + ((mu * x) / (1 + mu)^2))
  p2 <- exp(-mu * x)
  p3 <- 1 - (p1 * p2)
  cdf <- p3^sigma
  
  if (lower.tail == TRUE){
    cdf <- cdf
  } else {
    cdf <- 1 - cdf
  }
  
  if (log.p == FALSE){
    cdf <- cdf
  } else {
    cdf <- log(cdf)
  }
  
  return(cdf)
}
pEXL_vec<- Vectorize(pEXL)

par(mfrow=c(1,2))
curve(pEXL_vec(x, mu=0.5, sigma=0.5), 
      from=0.001, to=5,
      ylim=c(0, 1), 
      col="blue", lwd=2, 
      main="Cumulative Distribution Function",
      xlab="x", ylab="f(x)")
curve(pEXL_vec(x, mu=1, sigma=0.5),
      col="red", 
      lwd=2,
      add=TRUE)
curve(pEXL_vec(x, mu=1.5, sigma=0.5),
      col="green",
      lwd=2,
      add=TRUE)
legend("bottomright", legend=c("mu=0.5, sigma=0.5", 
                              "mu=1.0, sigma=0.5",
                              "mu=1.5, sigma=0.5"),
       col=c("blue", "red", "green"), lwd=2, cex=0.5)
curve(pEXL_vec(x, mu=0.5, sigma=0.5, lower.tail=FALSE), 
      from=0.001, to=5,
      ylim=c(0, 1), 
      col="blue", lwd=2, 
      main="Cumulative Distribution Function",
      xlab="x", ylab="f(x)")
curve(pEXL_vec(x, mu=1, sigma=0.5, lower.tail=FALSE),
      col="red", 
      lwd=2,
      add=TRUE)
curve(pEXL_vec(x, mu=1.5, sigma=0.5, lower.tail=FALSE),
      col="green",
      lwd=2,
      add=TRUE)
legend("topright", legend=c("mu=0.5, sigma=0.5", 
                               "mu=1.0, sigma=0.5",
                               "mu=1.5, sigma=0.5"),
       col=c("blue", "red", "green"), lwd=2, cex=0.5)

# Una pruebita
pEXL_vec(x=4.7, mu=1.3, sigma=2.5)
integrate(dEXL, lower=0, upper=4.7, mu=1.3, sigma=2.5)

pEXL(x=2, mu=1.3, sigma=2.5) + pEXL(x=2, mu=1.3, sigma=2.5, lower.tail=FALSE)
#notese que esta bien, pues la suma de las probabilidades es 1
#-----------------------------------------------------------
#funcion hazard sobre la EXL
#La función hazard, también conocida como función de riesgo
#o tasa de falla, es un concepto clave en análisis de supervivencia
#, confiabilidad y procesos estocásticos. Describe la probabilidad
#instantánea de que ocurra un evento (por ejemplo, falla, muerte, etc.)
#en un tiempo específico, dado que el sujeto o sistema ha sobrevivido
#hasta ese momento.

hEXL <- function(x, mu, sigma, log=FALSE){
  p1 <- dEXL(x,mu,sigma,log=log)
  p2 <- 1 - pEXL(x,mu,sigma, log.p=log)
  return(p1/p2)
}
hazard_vec <- Vectorize(hazard)
par(mfrow=c(1,3))
curve(hazard_vec(x, 0.5, 0.5), 
      from=0.001, to=10,
      ylim=c(0, 4), #para mirar que el eje Y sea entre 0,1
      col="blue", lwd=2, 
      main="Función Hazard ",
      xlab="x", ylab="f(x)")
curve(hazard_vec(x, 1, 0.5),
      col="red", 
      lwd=2,
      add=TRUE)
curve(hazard_vec(x, 1.5, 0.5),
      col="green",
      lwd=2,
      add=TRUE)
legend("topright", legend=c("mu=0.5, sigma=0.5", 
                              "mu=0.5, sigma=1",
                              "mu=0.5, sigma=1.5"),
       col=c("blue", "red", "green"), lwd=2,cex=0.8)



curve(hazard_vec(x, 0.5, 1), 
      from=0.001, to=10,
      ylim=c(0, 2), #para mirar que el eje Y sea entre 0,1
      col="blue", lwd=2, 
      main="Función Hazard ",
      xlab="x", ylab="f(x)")
curve(hazard_vec(x, 1, 1),
      col="red", 
      lwd=2,
      add=TRUE)
curve(hazard_vec(x, 1.5, 1),
      col="green",
      lwd=2,
      add=TRUE)
legend("topright", legend=c("mu=1, sigma=0.5", 
                              "mu=1, sigma=1",
                              "mu=1, sigma=1.5"),
       col=c("blue", "red", "green"), lwd=2, cex=0.8)

curve(hazard_vec(x, 0.5, 1.5), 
      from=0.001, to=10,
      ylim=c(0, 2), #para mirar que el eje Y sea entre 0,1
      col="blue", lwd=2, 
      main="Función de Densidad ",
      xlab="x", ylab="f(x)")
curve(hazard_vec(x, 1, 1.5),
      col="red", 
      lwd=2,
      add=TRUE)
curve(hazard_vec(x, 1.5, 1.5),
      col="green",
      lwd=2,
      add=TRUE)
legend("topright", legend=c("mu=1.5, sigma=0.5", 
                              "mu=1.5, sigma=1",
                              "mu=1.5, sigma=1.5"),
       col=c("blue", "red", "green"), lwd=2, cex=0.8)

#--------------------------------------------------------------
#funcion del u-esimo cuantil

#install.packages("lamW")
# tiene que ser con el logaritmo?
require(lamW)
qEXL <- function(p, mu, sigma, lower.tail=TRUE, log.p=FALSE){
  if(any(p < 0 | p > 1)) stop("Parameter p has to be beetwen 0 and 1")
  if(any(mu<=0))         stop("Parameter mu has to be positive")
  if(any(sigma<=0))      stop("Parameter sigma has to be positive")
  p1 <- -(1+mu)^2 / mu
  p2 <- 1/mu
  temp <- (1+mu)^2  * (p^(1/sigma)-1) / exp((1+mu)^2)
  p3 <- lambertWm1(temp)
  result <- p1 - p2 * p3
  return(result)
}
qEXL <- Vectorize(qEXL)

# una pruebita con la funcion quantile
p <- 0.7
cuantil <- qEXL(p=p, mu=2.3, sigma=1.7) #se guarda el valor que me da una
#proba de p, a la izquierda de el
cuantil
integrate(dEXL, lower=0, upper=cuantil, mu=2.3, sigma=1.7)#verificamos
#que si sea cierto y que muestre la probabilidad de estar 
#por debajo de ese valor

#recordemos que la funcion del quantile, me retorna
#el valor que deja una probabilidad a izquierda de P
#o a la derecha en caso de lowe.tail= FALSE 
p <- seq(from=0, to=0.99999, length.out=100)
plot(x=qEXL(p, mu=2.3, sigma=1.7), y=p, xlab="Quantile",
     las=1, ylab="Probability")
curve(pEXL(x, mu=2.3, sigma=1.7), 
      from=0, add=TRUE, col="red", lwd=2.5)


# generador
rEXL <- function(n, mu, sigma){
  if(any(mu<=0))    stop("Parameter mu has to be positive ")
  if(any(sigma<=0)) stop("Parameter sigma has to be positive")
  u <- runif(n)
  return(qEXL(p=u, mu=mu, sigma=sigma))
}
#recordemos que esta funcón me genera números aleatorios
#de esta función, por tanto, la mayor cantidad de números
#generados es los que mas tienen probabilidad
#por tanto deben seguir un la funcióin de distribución


# probando el generador
mu <- 1.5
sigma <- 2.5
x <- rEXL(n=10000, mu=mu, sigma=sigma)
hist(x, freq=FALSE)
curve(dEXL(x, mu=mu, sigma=sigma), from=0, to=20, 
      add=TRUE, col="tomato", lwd=2)


#-------------------------------------------------------------
#esperana y varianza
x_fx <- function(x, mu, sigma) x * dEXL(x, mu, sigma)
x2_fx <- function(x, mu, sigma) x^2 * dEXL(x, mu, sigma)

media <- function(mu, sigma) {
  res <- integrate(x_fx, lower=0, upper=Inf, mu=mu, sigma=sigma)
  return(res$value)
}

momento_2 <- function(mu, sigma){
  res <- integrate(x2_fx, lower=0, upper=Inf, mu=mu, sigma=sigma)
  return(res$value)
}



media(mu=0.25, sigma=0.2)

varia <- function(mu, sigma) {
  esp <- media(mu, sigma)
  esp_2 <- momento_2(mu, sigma)
  return(esp_2 - esp^2)
}
varia(mu=0.25, sigma=0.5)



# Applications ------------------------------------------------------------

x <- c(14.918, 10.656, 12.274, 10.289, 10.832, 7.099, 5.928, 13.211, 
       7.968, 7.584, 5.555, 6.027, 4.097, 3.611, 4.960, 7.498, 6.940, 
       5.307, 5.048, 2.857, 2.254, 5.431, 4.462, 3.883,
       3.461, 3.647, 1.974, 1.273, 1.416, 4.235)


logLik_EXL <- function(logparam=c(0, 0), x){
  return(sum(dEXL(x = x,
                  mu    = exp(logparam[1]),
                  sigma = exp(logparam[2]),
                  log=TRUE)))
}

estim_mu_sigma_EXL <- function(y) {
  mod <- optim(par=c(0, 0),
               fn=logLik_EXL,
               method="Nelder-Mead",
               control=list(fnscale=-1, maxit=100000),
               x=y)
  res <- c(mu_hat    = exp(mod$par[1]),
           sigma_hat = exp(mod$par[2]))
  names(res) <- c("mu_hat", "sigma_hat")
  return(res)
}

estim_mu_sigma_EXL(x)[1]
estim_mu_sigma_EXL(x)[2]

