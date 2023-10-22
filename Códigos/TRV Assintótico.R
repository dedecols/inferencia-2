# EXEMPLO 1
# Hipótese nula
theta0 <- 5

# Nível de significância
alpha0 <- 0.05 

# Função para calcular a estatística de teste
estatistica.teste <- function(x){
  lambda.x=exp(-n*(5-mean(x)))*(5/mean(x))^(sum(x))
  w.x=-2*log(lambda.x)
  return(w.x)
}

# Procedimento de teste
# Rejeito H0 se W(X)>=c (encontrar o valor de c)
# W(X) converge em distribuição para uma Qui-Quadrado com 1 grau de liberdade. Portanto c é o quantil de 1-alpha de uma qui-quadrado com 1 grau de liberdade.
c <- qchisq(1-alpha0,df=1)


# Valores de uma Poisson(theta0)
n=50
x <- rpois(n,lambda=theta0)
estatistica.teste(x)

# Conclusão para os valores de uma Poisson(theta0)
ifelse(estatistica.teste(x)>=c,"Há evidências para rejeitar H0",
       "Não há evidências para rejeitar H0")


# Valores de uma Poisson(theta0)
n=50
theta=6
x <- rpois(n,lambda=theta)
estatistica.teste(x)

# Conclusão para os valores de uma Poisson(theta0)
ifelse(estatistica.teste(x)>=c,"Há evidências para rejeitar H0",
       "Não há evidências para rejeitar H0")














# EXEMPLO 2
# Hipótese nula





















