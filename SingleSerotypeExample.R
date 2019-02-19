# ------ The following script is an example project for the Diversity of viruses project -------#
b <- 0.65
g <- 1
N <- 200
mu <- 0
m <- 0.5
t <- 1
I <- 10
S <- N-I
Time <- 5

S_vals <- list(S)

I_vals <- list(I)

t_vals <- list(t)

i <- 2
dt <- 0
while (t < Time){
  R1 <- (b*I*S)/N
  R2 <- g*I
  R <- R1 + R2
  dt <- -log2(runif(1))/R
  t <- t + dt
  
  if(runif(1) < R1/R){
    S <- S-1
    I <- I+1
  }
  else if(runif(1) < R2/R){
    I <- I-1
    S <- S+1
  }
  else{
    I <- I
    S <- S
  }
  S_vals <- c(S_vals, S)
  I_vals <- c(I_vals, I)
  t_vals <- c(t_vals, t)
}

plot(t_vals, I_vals, type="l")