N <- 100 # Liczba kolorów
M <-100000
alpha <- sample(c(1:100), size = N, replace = TRUE)#, prob=c(1:100))
alpha <- rep(1,N)
alpha1 <- alpha

for(i in 1:M)
{
  index <- sample(c(1:N), size=1, prob = alpha)
  alpha[index] = alpha[index] + 1
}

plot((alpha-alpha1)/sum(alpha), type="l")