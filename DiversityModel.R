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
  tmax <- 150
  tstep <- 0.1
  times <- seq(0, tmax, by = tstep);
  
  #SOLVE ODEs#
  require(deSolve);
  out <- ode(y = state, times = times, func = model, parms = parameters);
  nu <- matrix(c(-1,0,+1,-1,0,+1),nrow=3,byrow=TRUE);
  #out1 <- ssa(state, c("N-S + sum(gn*In) + sum(-((bn*In*S)/N))","(bn*In*S)/N - gn*In"), nu, parms = parameters, tf=100, simName="Diversity Model");
  head(out);
  #ssa.plot(out1);
  #PLOT FIGURES NUMERICALLY#
  par(oma = c(0, 0, 3, 0));
  plot(out, xlab = "time", ylab = "-");
  mtext(outer = TRUE, side = 3, "Diversity Model", cex = 1.5);

}