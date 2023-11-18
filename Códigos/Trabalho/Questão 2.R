# QUESTÃO 2
# item b

# Gerar um valor para X1
set.seed(1)
rbinom(n=1,size=200,prob=0.2)

# Gerar um valor para X2
set.seed(1)
rbinom(n=1,size=300,prob=0.2)

p1.hat <- 42/200
p2.hat <- 62/300

pbarra <- (42+62)/(200+300)
qbarra <- 1-pbarra

p1.hat-p2.hat
qnorm(0.05/2)*sqrt(pbarra*qbarra*((1/200)+(1/300)))

# Teste com uma observação para X1 e uma observação para X2
condicao <- p1.hat-p2.hat<=qnorm(0.05/2)*sqrt(pbarra*qbarra*((1/200)+(1/300)))
ifelse(condicao,"Rejeito H0", "Não rejeito H0")


# Item C

# Valor de X1
alpha <- 0.05
n1 <- 200
n2 <- 300

x1 <- rbinom(n=1,size=n1,prob=0.2)
x2 <- rbinom(n=1,size=n2,prob=0.2)

p1.hat <- x1/n1
p2.hat <- x2/n2

pbarra <- (x1+x2)/(n1+n2)
qbarra <- 1-pbarra

p1.hat-p2.hat
qnorm(alpha/2)*sqrt(pbarra*qbarra*((1/n1)+(1/n2)))

(p1.hat-p2.hat<=qnorm(alpha/2)*sqrt(pbarra*qbarra*((1/n1)+(1/n2))))
resultado <- NULL

for (i in 1:100) {
  x1 <- rbinom(n=1,size=n1,prob=0.2)
  x2 <- rbinom(n=1,size=n2,prob=0.2)
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  
  pbarra <- (x1+x2)/(n1+n2)
  qbarra <- 1-pbarra
  
  condicao <- p1.hat-p2.hat<=qnorm(alpha/2)*sqrt(pbarra*qbarra*((1/n1)+(1/n2)))
  ifelse(condicao,resultado[i] <- 0, resultado[i] <- 1)
}

sum(resultado)/100


