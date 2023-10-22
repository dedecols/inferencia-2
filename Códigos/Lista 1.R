# QUESTÃO 3
#item e)
theta=1/3
1-pbinom(0.9,2,(1/3))-(1-pbinom(1.1,2,(1/3)))


# QUESTÃO 4
# item c
qgamma(0.9,1,2)

theta=seq(0:10)
poder <- function(x){
  poder=1-pgamma(1.15,shape=1,rate=theta)
  
  return(poder)
}

# Vemos que a função poder é decrescente em theta
poder(theta)


# QUESTÃO 5
# item a
n=10
theta=seq(1:5)
poder <- ifelse(theta>1.5,
                (1.5/theta)^n,
                1)

poder <- function(theta,n){
  poder <- ifelse(theta>1.5, (1.5/theta)^n, 1)
  
  return(poder)
}

poder(theta,n=5)



# QUESTÃO 6
# item a)

# Função poder de delta
poder <- function(x){
  resultado=1-pbinom(6,20,p)+pbinom(1,20,p)
  
  return(resultado)
}

# Alguns resultados
p=c(0,0.1,0.6)
poder(p)

# Gráfico da função poder
plot(function(x)poder(x))

pbinom(1,20,0.2)+1-pbinom(6,20,0.2)


# QUESTÃO 7

qnorm(0.025,0,1)
qnorm(0.975,0,1)/5


# QUESTÃO 8
theta=seq(from=0.1,to=1,by=0.1)
1-pbeta(0.5,shape1=theta,shape2=1)
1-pbeta(0.5,shape1=1.5,shape2=1)
1-pbeta(0.5,shape1=2,shape2=1)
1-pbeta(0.5,shape1=3,shape2=1)


# QUESTÃO 9

theta=seq(from=0,to=0.11,by=0.01)
1-pbinom(1,size=10,prob=theta)



