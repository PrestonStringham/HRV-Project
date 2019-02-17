library(googlesheets)

#ACCESS SPREADSHEET#

master_sheet <- gs_title("HRV-Experiment")


#SET CONSTANTS USING SPREADSHEET #

N <- as.numeric(as.list(gs_simplify_cellfeed(gs_read_cellfeed(master_sheet, ws = 1, range=cell_cols(4)))))
mu <- as.numeric(as.list(gs_simplify_cellfeed(gs_read_cellfeed(master_sheet, ws = 1, range=cell_cols(5)))))


#SET INFECTION AND RECOVERY RATES (bn and gn, respectively)#

b<-gs_read_cellfeed(master_sheet, ws = 1, range=cell_cols(1))
bn <- as.numeric(as.list(gs_simplify_cellfeed(b)))

g<-gs_read_cellfeed(master_sheet, ws = 1, range=cell_cols(2))
gn <- as.numeric(as.list(gs_simplify_cellfeed(g)))


#SET INITIAL INFECTED POPULATION DATA#

I<-gs_read_cellfeed(master_sheet, ws = 1, range=cell_cols(3))
In <- as.numeric(as.list(gs_simplify_cellfeed(I)))

if(length(bn) != length(gn) && length(bn) != length(In)){
  stop("Not enough infection rates, recovery rates, or initial infected population... Please fix...")
}else{
  
  m <- length(gn)
  t <- 0
  
  S <- N-sum(In)
  
  Time <- 10
  
  S_vals <- list(S)
  
  I_vals <-list()
  
  for(i in c(1:m)){
    test <- list(In[i])
    I_vals[i] <- list(test)
  }
  
  t_vals <- list()
  
  for(i in c(1:m)){
     val <- list(t)
     t_vals[i] <- list(val)
  }
  
  for(i in c(1:m)){
    
    t <- 0
    
    while (t < Time){
      
      w1 <- (mu/(m-1))*(bn[i]*In[i]*S)/N;
      w2 <- gn[i]*In[i];
      W <- w1 + w2
      
      dt <- -log(runif(1))/W
      
      t <- t + dt
        
      if(runif(1) < w1/W){
        S <- S-1
        In[i] <- In[i]+1
      }
      
      else{
        In[i] <- In[i]-1
        S <- S+1
      }
      
      #S_vals <- c(S_vals, S)
      I_vals[[i]] <- c(I_vals[[i]], In[i])
      t_vals[[i]] <- c(t_vals[[i]], t)
    }
  }
}
#plot(t_vals[[1]], S_vals)
#for(i in c(1:m)){
par(mfrow=c(2,2))
  plot(t_vals[[1]], I_vals[[1]])
  plot(t_vals[[2]], I_vals[[2]])
  plot(t_vals[[3]], I_vals[[3]])
  plot(t_vals[[4]], I_vals[[4]])
#}