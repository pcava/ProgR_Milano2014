
## ----profiling-001-------------------------------------------------------
f1 <- function(x, s1 = 1 , s2 = 2){
  n <- length(x)
  y <- NULL
  for ( i in 1:n){
    if (x[i] %% 2 == 0 ) tmp <- x[i]+rnorm(1, 0 , s2)
    else tmp <- x[i]+rnorm(1, 0, s1)
    y <- c(y, tmp)
  }
  sum(y)  
}  
f1(x = 1:5)


## ----profiling-002-------------------------------------------------------
x <- sample(1:10^5)
system.time(f1(x))


## ----profiling-003-------------------------------------------------------
x <- sample(1:10^5)
Rprof("f1.Rprof")
f1(x)
Rprof(NULL)
summaryRprof("f1.Rprof")$by.self


## ----profiling-004-------------------------------------------------------
f2 <- function(x, s1 = 1 , s2 = 2){
  n <- length(x)
  y <- numeric(n)
  for ( i in 1:n){
    if (x[i] %% 2 == 0 ) tmp <- x[i]+rnorm(1, 0 , s2)
    else tmp <- x[i]+rnorm(1, 0, s1)
    y[i] <- tmp
  }
  sum(y)  
}  
f2(x = 1:5)


## ----profiling-005-------------------------------------------------------
Rprof("f2.Rprof")
f2(x)
Rprof(NULL)
summaryRprof("f2.Rprof")$by.self


## ----profiling-006-------------------------------------------------------
f3 <- function(x, s1 = 1 , s2 = 2){
  f31 <- function(x, s1 , s2){
    ifelse(x %% 2 == 0, x+rnorm(1, 0 , s2) , x+rnorm(1, 0, s1))
  }
sum(vapply(x, f31, FUN.VALUE=numeric(1), s1, s2 ))
}
f3(x = 1:5)


## ----profiling-007-------------------------------------------------------
Rprof("f3.Rprof")
f3(x)
Rprof(NULL)
summaryRprof("f3.Rprof")$by.self


## ----profiling-008-------------------------------------------------------
f4 <- function(x, s1 = 1 , s2 = 2){
  n <- length(x)
  s1 <- rnorm(n, 0 , s1)
  s2 <- rnorm(n, 0 , s2)
  sum(ifelse(x %% 2 == 0, x+s1 , x+s2))
}
f4(x = 1:5)


## ----profiling-009-------------------------------------------------------
Rprof("f4.Rprof")
f4(x)
Rprof(NULL)
summaryRprof("f4.Rprof")$by.self


## ----profiling-010-------------------------------------------------------
summaryRprof("f1.Rprof")$sampling.time
summaryRprof("f2.Rprof")$sampling.time
summaryRprof("f3.Rprof")$sampling.time
summaryRprof("f4.Rprof")$sampling.time


## ----profiling-011-------------------------------------------------------
summaryRprof("f2.Rprof")


## ----profiling-012-------------------------------------------------------
x <- sample(1:10^5)
Rprof("f1.Rprof", memory.profiling = TRUE)
f1(x)
Rprof(NULL)
summaryRprof("f1.Rprof", memory = "both")$by.self


## ----profiling-012A------------------------------------------------------
f_loop <- function(n = 10, k = 1e+05){
  z = numeric(k)
  for (i in 1:k) {
    x = rnorm(n, 0, 1)
    y = rnorm(n, 0, 1)
    z[i] = cor(x, y)
  }
quantile(z, 0.95)
}


## ----profiling-012B------------------------------------------------------
f_rep <- function(n = 10, k = 1e+05){
  z <- replicate(k, cor(rnorm(n), rnorm(n)))
  quantile(z, .95)
}


## ----profiling-012C------------------------------------------------------
f_mat <- function(n = 10, k = 1e+05){
  z <- replicate(k, cor(matrix(rnorm(n*2), nrow = n, ncol = 2))[1,2])
  quantile(z, .95)
}


## ----profiling-012D------------------------------------------------------
require(rbenchmark)
benchmark(f_loop(), f_rep(), f_mat(), replications = 10, order = "elapsed")


## ----profiling-013-------------------------------------------------------
x <- 1:100
i <- 66
x[i]
x[[i]]


## ----profiling-014-------------------------------------------------------
benchmark(x[i], x[[i]], replications = 1000)


## ----profiling-015-------------------------------------------------------
library(microbenchmark)
test <- microbenchmark(x[i] , x[[i]], times = 1000)
test


## ----profiling-016-------------------------------------------------------
library(ggplot2)
autoplot(test)


