N <- 0;
m <- 10;
g <- c(1:m);
bn <- c(1:m);

A <- matrix(0, nrow = m, ncol = m)

x <- as.list(rnorm(10));
names(x) <- paste("a", 1:length(x), sep = "")
list2env(x , envir = .GlobalEnv)

for(index in 1:m){
  A[index, index] <- 3
}

d <- A*bn

