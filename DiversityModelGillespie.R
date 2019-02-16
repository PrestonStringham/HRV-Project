library(googlesheets)

#ACCESS SPREADSHEET#

master_sheet <- gs_title("HRV-Experiment")


#SET CONSTANTS USING SPREADSHEET #

N <- as.numeric(as.list(gs_simplify_cellfeed(gs_read_cellfeed(master_sheet, ws = 1, range=cell_cols(4)))))
mu <- 3


#SET INFECTION AND RECOVERY RATES (bn and gn, respectively)#

b<-gs_read_cellfeed(master_sheet, ws = 1, range=cell_cols(1))
bn <- as.numeric(as.list(gs_simplify_cellfeed(b)))

g<-gs_read_cellfeed(master_sheet, ws = 1, range=cell_cols(2))
gn <- as.numeric(as.list(gs_simplify_cellfeed(g)))


#SET INITIAL INFECTED POPULATION DATA#

#I<-gs_read_cellfeed(master_sheet, ws = 1, range=cell_cols(3))
#In <- as.numeric(as.list(gs_simplify_cellfeed(I)))
In = c(1, 1);

if(length(bn) != length(gn) && length(bn) != length(In)){
  stop("Not enough infection rates, recovery rates, or initial infected population... Please fix...")
}else{
  
  m <- length(gn)
  t <- 0
  
  S <- sum(N-sum(In))
  
  Time <- 10 
  
  S_vals <- list(S)
  
  I_vals <- c(In[1:m])
  
  t_vals <- list(t)
  
  i <- 2
  while (t < Time){
    w1 <- N-S + sum(gn*In) + sum(-((bn*In*S)/N));
    w2 <- (bn*In*S)/N - gn*In;
    for(i in c(1:m)){
      W <- w1 + w2[[i]]
      dt <- -log2(runif(1))/W
      t <- t + dt
      
      if(runif(1) < w1/W){
        S <- S-1
        In[[i]] <- In[[i]]+1
      }
      else{
        In[[i]] <- In[[i]]-1
        S <- S+1
      }
    }
    S_vals <- c(S_vals, S)
    I_vals <- c(I_vals, I)
    t_vals <- c(t_vals, t)
  }
}
plot(t_vals, S_vals)
