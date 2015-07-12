n <- 10000
mean <- -0.2
variance <- 3
b <- 15
u <- 10
M <- 10000

ruinStrategy = function()
{
  Y <- rnorm(n, mean, variance)
  Sn <- Y[1]      #Sume Sn obliczamy na biezaco
  iter <- 1       #Iterator
  countRuin <- 0  #Zliczanie liczby ruin
  
  while(((u - Sn)<=b) && ((u - Sn)>= 0)) # Sprawdzamy warunki z zadania
  {
    iter <- iter+1
    Sn <- Sn + Y[iter]  #Zliczanie sumy na biezco
  }
  if(u-Sn < 0)          #Jesli ruina to zaznaczamy
  {
    countRuin <- 1
  }
  return (countRuin)
}

result <- replicate(M, ruinStrategy())
p <- 1/M * sum(result)  #Prawdopodobienstwo
2*sd(result)/sqrt(M)    #Odchylenie standardowe

expRuinStrategy = function()
{
  Y <- rnorm(n, mean, variance)
  Sn <- Y[1]        #Sume Sn obliczamy na biezaco
  iter <- 1         #Iterator
  countRuin <- 0    #Liczba ruin
  
  while(((u - Sn)<=b) && ((u - Sn)>= 0))    #Warunki  zadania
  {
    iter <- iter+1
    Sn <- Sn + Y[iter]
  }
  if(u-Sn < 0)                              #Jesli zaszła ruina to patrzymy na to
  {
    countRuin <- exp(-2*mean*S)             #Miara wykladnicza
  }
  return (countRuin)
}

expResult <- replicate(M, expRuinStrategy())
p<-(1/M)* sum(expResult)                    #Prawdopodobienstwo
2*sd(expResult)/sqrt(M)                     #Odchylenie
