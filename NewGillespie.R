#Preston Stringham - January 2019#
# Rhinovirus Diversity Experiment. Required packages: googlesheets, ggplot2#
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
  
  #m SEROTYPES
  m <- length(gn)
  
  #SET NAMES
  Inms <- paste0("I",1:m)
  nms <- c("S",Inms)
  #names(state) <- nms
  dnms <- paste0("d",nms)
  
  #INITIAL TIME
  time <- 0
  
  #SIMULATION DURATION
  duration <- 25
  
  #INITIAL SUSCEPTIBLE POPULATION
  S <- N-sum(In)
  
  #SELECTED SEROTYPE FOR MUTATION
  mutated <- 3
  
  #CREATE DATAFRAME
  df <- data.frame(time, S, I=t(In))
  names <- c("time", nms)
  colnames(df) <- names
  
  #CREATE GILLESPIE MATRIX
  A <- diag(m+1)
  A[1,1] <- 0
  A[(1:m+1),1] <- -1
  
  B = do.call(rbind, replicate(1, A, simplify=FALSE))*(-1)
  
  M <- rbind(A[1:m+1,], B[1:m+1,])
  
  #SET DATAFRAME ACCESS INDEX
  index <- nrow(df) + 1
  
  while (time < duration){
    
    #SUM OF THE RATE OF THE EVENTS
    r1 <- (mu/m)*(bn*In*S)/N
    r1[mutated] <- (1-mu)*(bn[mutated]*In[mutated]*S)/N
    #r1 <- (1 - mu)*bn*In*S + (mu/m)*sum(bn*In)*S
    r2 <- gn*In
    rtot <- sum(r1 + r2)
    
    # GENERATE TIME STEP dt
    dt <- -log(runif(1))/rtot
    
    #ADD dt TO time
    time <- time + dt
    
    #CHOOSE OUTCOME BASED ON RANDOM NUMBER AND SUM OF RATE OF EVENTS
    rand <- runif(1)
    
    #UPDATE POPULATIONS FROM MATRIX
    var <- sample(1:(2*m), 1, prob=c(r1, r2))
    In <- In + M[var,1:m+1]
    S <- S + M[var, 1]
    
    #APPEND TO DATA FRAME
    df[index,] <- c(time, S, In) 
    
    index <- index + 1
    
  }
  
  #Save mean bn and gn values of serotypes after simulation.
  extinct <- c()
  survivors <- c()
  for(i in 3:ncol(df)){
    if(df[nrow(df),i] < 1){
      extinct <- c(extinct, i-2)
    }
    else{
      survivors <- c(survivors, i-2)
    }
  }
  bn_mean = c(mean(bn[extinct]), mean(bn[survivors]))
  gn_mean = c(mean(gn[extinct]), mean(gn[survivors]))
  
  #PLOT USING GGPLOT2
  require(ggplot2)
  require(reshape2)
  p <- melt(df, id.vars = 'time', variable.name = 'series')
  ggplot(p, aes(time,value)) + geom_line() + facet_wrap(.~series, ncol=3)
  
}