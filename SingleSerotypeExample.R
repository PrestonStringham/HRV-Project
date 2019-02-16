# ------ The following script is an example project for the Diversity of viruses project -------#
b <- 1
g <- 0.1
N <- 200
mu <- 0
m <- 1
t <- 1
I <- 10
S <- N-I
Time <- 5

S_vals <- list(S=S)

I_vals <- list(I=I)

t_vals <- list(t=t)

i <- 2
dt <- 0
while (t < Time){
  w1 <- N-S + g*I - (b*I*S)/N
  w2 <- (b*I*S)/N - g*I
  W <- w1 + w2
  dt <- -log2(runif(1))/W
  t <- t + dt
  
  if(runif(1) < w1/W){
    S <- S-1
    I <- I+1
  }
  else{
    I <- I-1
    S <- S+1
  }
  S_vals <- c(S_vals, S)
  I_vals <- c(I_vals, I)
  t_vals <- c(t_vals, t)
}

plot(t_vals, I_vals)