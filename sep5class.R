p <- 1000
n <- 50
set.seed(03875)
X <- matrix(nrom(n*p, mean = 10, sd = 3), n, p)
group <- rep(1:2, each = n/2)



computeT_for <- function(X, group)
{
  Tstats <- rep(0,p)
  for(j in 1:p)
  {
    Tstats[j] <- t.test(X[,j] ~ group)$stat
  }
  return(Tstats)
}



computeT_for2 <- function(X,group)
{
  Tstats <- rep(0,p)
  for (j in 1:p)
  {
    Tstat[j] <- t.test(X[group == 1,j],X[group == 2,j])$stat
  }
  return(Tstats)
}


library(microbenchmark)
microbenchmark(computeT_for, computeT_for2)





computeT_for3 <- function(X, group)
{
  n1 = sum(group == 1)
  n2 = sum(group == 2)
  Tstats = rep(0,p)
  for (j in 1:p)
  {
    m1 = mean(X[group == 1,j])
    m2 = mean(X[group == 2,j])
    var1 = var(X[group == 1,j])
    var2 = var(X[group == 2,j])
   Tstats[j] = (m1 - m2) / sqrt( var/n1 + var2/n2)
  }
}