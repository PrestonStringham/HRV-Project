#Preston Stringham - 2019#
# Rhinovirus Diversity Experiment. Required packages: googlsheets, deSolve, GillespieSSA#

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

parameters <- c(m, mu, N)


#INITIAL POPULATION DATA#

state <- c(S = N-sum(In), In)


#NAMES

Inms <- paste0("I",1:m)
nms <- c("S",Inms)
names(state) <- nms
dnms <- paste0("d",nms)


#CHECK TO SEE IF THERE IS ENOUGH RATES. MAKE SURE DATA IS ENTERED CORRECTLY#

if(length(bn) != length(gn) && length(bn) != length(In)){
  stop("Not enough infection or recovery rates... Please fix...")
}else{
  
  #SET NUMBER OF SEROTYPES AFTER CONFIRMING DATA IS ENTERED CORRECTLY#
  
  m <- length(gn)
  
  
  #BEGIN GENERATING SYSTEM OF ODEs#
  
  model<-function(t, state, parameters) {
    
    with(as.list(parameters),{
      
      S <- state["S"]
      In <- state[Inms]
      #SUSCEPTIBLE COMPARMTENT#
      dS <- sum(gn*In) - sum(((bn*In*S)/N));
      
      #INFCETED COMPARMENTS FOR GENERATED m SEROTYPES#
      dIn <- (bn*In*S)/N - gn*In;
      res <- c(dS,dIn)
      names(res) <- dnms
      #RETURN LIST OF DIFFERENTIALS#
      return(list(res));
      
    })
  };
  
  #TIMES STEPS#
  tmax <- 25
  tstep <- 0.1
  times <- seq(0, tmax, by = tstep);
  
  #SOLVE ODEs#
  require(deSolve);
  out <- ode(y = state, times = times, func = model, parms = parameters);
  head(out);
  
  #CONVERT OUT TO DATA FRAME IN ORDER TO PLOT USING GGPLOT2 AND RESHAPE2
  df <- as.data.frame(out)
  
  #PLOT USING GGPLOT2
  require(ggplot2)
  require(reshape2)
  p <- melt(df, id.vars = 'time', variable.name = 'series')
  ggplot(p, aes(time,value)) + geom_line() + facet_wrap(.~series, ncol=3)
}