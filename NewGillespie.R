#Preston Stringham - January 2019
# Rhinovirus Diversity Experiment. Required packages: reshape2, ggplot2

#NUMBER OF SEROTYPES
m <- 10

#SET gn TO GET m
gn <- c(runif(m, 0.3, 0.3))

#m SEROTYPES
m <- length(gn)

N <- 1000
mu <- 0.01

#UNIFORMLY SELECT bn VALUES FOR m SEROTYPES
bn_lower = 0.5
bn_upper = 0.8
bn <- runif(m, bn_lower, bn_upper)

#SET INITIAL INFECTED POPULATION DATA#
In <- c(runif(m, 5, 5))

#MAKE SURE CONDITIONS HAVE SAME LENGTH
if(length(bn) != length(gn) && length(bn) != length(In)){
  stop("Not enough infection rates, recovery rates, or initial infected population... Please fix...")
}else{
  
  #SET NAMES
  Inms <- paste0("I",1:m)
  nms <- c("S", Inms)
  
  #INITIAL TIME
  time <- 0
  
  #SIMULATION DURATION
  duration <- 100
  
  #INITIAL SUSCEPTIBLE POPULATION
  S <- N-sum(In)
  
  #SIMPSON INDEX
  simp_num = In[1:m]^2
  simp_den = sum(In)^2
  simp = sum(simp_num/simp_den)
  #CREATE DATAFRAME
  df <- data.frame(time, S, I=t(In), simp)
  names <- c("time", nms, "Diversity")
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
    r1 <- (1-mu)*(bn*In*S)/N + (mu/m)*sum(bn*In)*S/N
    #r1[selected] <- (1-mu)*(bn[selected]*In[selected]*S)/N
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
    
    #RECALCULATE SIMPSON INDEX
    simp_num = In[1:m]^2
    simp_den = sum(In)^2
    simp = sum(simp_num/simp_den)
    
    #APPEND TO DATA FRAME
    df[index,] <- c(time, S, In, simp) 
    
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
  bn_means <- c(mean(bn[extinct]), mean(bn[survivors]))
  gn_means <- c(mean(gn[extinct]), mean(gn[survivors]))
  df_means <- data.frame(bn_means[1], bn_means[2])
  df_means <- rbind(df_means, gn_means)
  
  mean_names <- c("Extinct Mean", "Surivivor Mean")
  rate_names <- c("bn", "gn")
  rownames(df_means) <- rate_names
  colnames(df_means) <- mean_names
  
  #PLOT USING GGPLOT2
  require(ggplot2)
  require(reshape2)
  
  p <- melt(df, id.vars = 'time', variable.name = 'series')
  
  #PLOT FOR EACH SEROTYPE
  #ggplot(p, aes(time,value)) + geom_line() + facet_wrap(.~series, ncol=3)

  #ALL SEROTYPES ON ONE PLOT
  print(ggplot(p, aes(time, value)) + geom_line(aes(colour = series)))
  
  #PLOT DIVERSITY
  #lines(df[,1], df[,ncol(df)])
  #p2 <- ggplot(p, aes(time, value)) + geom_line(data=subset())
  print(ggplot(df, aes(x=time, y = Diversity)) + geom_line())
  
  
  print(plot(bn,unlist(df[nrow(df),Inms])))
}