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
  
  #INITIAL TIME
  time <- 0
  
  #SIMULATION DURATION
  duration <- 0.1
  
  #INITIAL SUSCEPTIBLE POPULATION
  S <- N-sum(In)
  S_vals <- list()
  for(i in c(1:m)){
    val <- list(S)
    S_vals[i] <- list(val)
  }
  
  #GENERATE LIST OF LISTS FOR INITIAL POPULATION OF EACH SEROTYPE
  I_vals <-list()
  
  for(i in c(1:m)){
    val <- list(In[i])
    I_vals[i] <- list(val)
  }
  
  #GENERTATE LIST OF LISTS FOR TIME STEPS OF EACH SEROTYPE IN SIMULATION
  time_vals <- list()
  
  for(i in c(1:m)){
     val <- list(time)
     time_vals[i] <- list(val)
  }
  
  count_1 <- 0
  count_2 <- 0
  count_3 <- 0
  
  #RUN SIMULATION m TIMES
  for(i in c(1:m)){
    
    time <- 0
    
    while (time < duration){
      
      #SUM OF THE RATE OF THE EVENTS
      E1 <- (mu/(m-1))*(bn[i]*In[i]*S)/N;
      E2 <- gn[i]*In[i];
      E <- E1 + E2
      
      # GENERATE TIME STEP dt
      dt <- -log(runif(1))/E
      
      #ADD dt TO time
      time <- time + dt
      
      #CHOOSE OUTCOME BASED ON RANDOM NUMBER AND SUM OF RATE OF EVENTS
      rand <- runif(1)
      if(rand < E1/E){
        S <- S-1
        In[i] <- In[i]+1
        count_1 <- count_1 + 1
      }
      else if(rand < E2/E){
        In[i] <- In[i]-1
        S <- S+1
        count_2 <- count_2 + 1
      }
      else{
        S <- S
        In[i] <- In[i]
        count_3 <- count_3 + 1
      }
      
      #APPEND VALUES TO PLOT
      S_vals[[i]] <- c(S_vals[[i]], S)
      I_vals[[i]] <- c(I_vals[[i]], In[[i]])
      time_vals[[i]] <- c(time_vals[[i]], time)
    }
  }
}

#PLOT DATA
par(mfrow=c(1,m/2))
plot(time_vals[[1]], S_vals[[1]], type="l")
plot(time_vals[[1]], I_vals[[1]], type="l")