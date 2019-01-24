#Preston Stringham - 2019#
# Rhinovirus Diversity Experiment. Required packages: googlsheets, deSolve#

library(googlesheets)

#ACCESS SPREADSHEET#

master_sheet <- gs_title("HRV-Experiment")


#SET CONSTANTS USING SPREADSHEET#

N <- as.numeric(as.list(gs_simplify_cellfeed(gs_read_cellfeed(master_sheet, ws = 1, range=cell_cols(4)))))
mu <- as.numeric(as.list(gs_simplify_cellfeed(gs_read_cellfeed(master_sheet, ws = 1, range=cell_cols(5)))))


#SET INFECTION AND RECOVERY RATES (bn and gn, respectively)#

b<-gs_read_cellfeed(master_sheet, ws = 1, range=cell_cols(1))
bn <- as.numeric(as.list(gs_simplify_cellfeed(b)))

g<-gs_read_cellfeed(master_sheet, ws = 1, range=cell_cols(2))
gn <- as.numeric(as.list(gs_simplify_cellfeed(g)))

parameters <- c(m, mu);


#INITIAL POPULATION DATA#

state <- c(S = 5,  I= 5);


#CHECK TO SEE IF THERE IS ENOUGH RATES. MAKE SURE DATA IS ENTERED CORRECTLY#

if(length(bn) != length(gn)){
  stop("Not enough infection or recovery rates... Please fix...")
}else{
  
  #SET NUMBER OF SEROTYPES AFTER CONFIRMING DATA IS ENTERED CORRECTLY#
  
  m <- length(gn)
  
  
  #BEGIN GENERATING SYSTEM OF ODEs#
  
  model<-function(t, state, parameters) {
    
    with(as.list(c(state, parameters)),{
  
      dS <- N-S;
      
      for (i in 1:m){
        dI[i] = (bn[i]*I[i]*S)/N - gn[i]*I[i];
      }
      
      list(c(dS, dI));
      
    })
  };
  
  times <- seq(0, 10, by = 0.01);
  
  library(deSolve);
  out <- ode(y = state, times = times, func = model, parms = parameters);
  head(out);
  
  par(oma = c(0, 0, 3, 0));
  plot(out, xlab = "time", ylab = "-");
  plot(out[, "S"], pch = ".");
  mtext(outer = TRUE, side = 3, "Diversity Model", cex = 1.5);
   
}