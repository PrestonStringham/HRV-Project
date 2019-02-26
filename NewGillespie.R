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
  duration <- 1
  
  #INITIAL SUSCEPTIBLE POPULATION
  S <- N-sum(In)
  
  #CREATE DATAFRAME
  df <- data.frame(time, S, t(Inms[1:m])=t(In))
  
  #CREATE MATRIX
  A <- diag(m)
  A[1,1] <- 0
  A[(2:m),1] <- -1
  
  #SET DATAFRAME ACCESS INDEX
  index <- nrow(df) + 1
  
  while (time < duration){
    #SUM OF THE RATE OF THE EVENTS
    E1 <- (mu/(m-1))*(bn*In*S)/N;
    E2 <- gn*In;
    E <- sum(E1 + E2)
    
    # GENERATE TIME STEP dt
    dt <- -log(runif(1))/E
    
    #ADD dt TO time
    time <- time + dt
    
    #CHOOSE OUTCOME BASED ON RANDOM NUMBER AND SUM OF RATE OF EVENTS
    rand <- runif(1)
    
    
    
    index <- index + 1
  }
  
}
