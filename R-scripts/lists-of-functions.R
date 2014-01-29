
## ----list-of-functions-001-----------------------------------------------
fun_list <- list(m = mean , s = sd)


## ----list-of-functions-002-----------------------------------------------
with (fun_list, m( x = 1:10))


## ----list-of-functions-003-----------------------------------------------
fun_list$m( x = 1:10)


## ----list-of-functions-004-----------------------------------------------
attach(fun_list)
m( x = 1:10)
detach(fun_list)


## ----list-of-functions-005-----------------------------------------------
fun <- function(f, ...){f(...)}


## ----list-of-functions-006-----------------------------------------------
fun(mean, x = 1:10, na.rm = TRUE)


## ----list-of-functions-007-----------------------------------------------
lapply(fun_list, fun, x = 1:10)



## ----list-of-functions-008-----------------------------------------------
lapply(fun_list, do.call, list(x = 1:10, na.rm = T))


## ----list-of-functions-009-----------------------------------------------
require(truncgof, quietly = TRUE)
nor_test <-  list (ad2.test = ad2.test, ad2up.test = ad2up.test,
                   ad.test = ad.test,adup.test = adup.test)


## ----list-of-functions-010-----------------------------------------------
x <- rnorm(100, 10, 1)
m <-  mean(x)
s <- sd(x)
lapply(nor_test, fun, x , distn = "pnorm", list(mean = m, sd = s), sim = 100)


## ----list-of-functions-011-----------------------------------------------
this_summary <- as.data.frame(rbind(
  vapply(trees , mean, FUN.VALUE = numeric(1)),
  vapply(trees , sd, FUN.VALUE = numeric(1)),
  vapply(trees , function(x, ...){diff(range(x))}, FUN.VALUE = numeric(1)))
)

row.names(this_summary) <- c("mean", "sd", "range")
this_summary


## ----list-of-functions-012-----------------------------------------------
my_summary <- function(x, flist){
  f <- function(f,...)f(...)
  g <- function(x, flist){vapply(flist, f , x, FUN.VALUE = numeric(1))}
  df <- as.data.frame(lapply(x, g , flist))
  row.names(df) <- names(flist)
  df
}

my_summary(cars, 
  flist = list(mean = mean, 
    stdev = sd, 
    range =  function(x,...){sd(x,...)/mean(x,...)}
               )
           )



## ----list-of-functions-013-----------------------------------------------
fapply <- function(X, FUN, ...){
  lapply(FUN, function(f, ...){f(...)}, X, ...)
}


## ----list-of-functions-014-----------------------------------------------
basic_stat <- list(mean = mean, median = median, sd = sd)
fapply(1:10, basic_stat)


## ----list-of-functions-015-----------------------------------------------
x <- rlnorm(n = 10^4, meanlog = 2 , sdlog = 1)


## ----list-of-functions-016-----------------------------------------------
system.time(
lapply(nor_test, fun, x , distn = "plnorm",   
       fit = list(meanlog = mean(log(x)), sd = sd(log(x))), sim = 100)
)


## ----list-of-functions-017-----------------------------------------------
require(parallel)
system.time(
mclapply(nor_test, fun, x = x, distn = "plnorm", 
  fit = list(meanlog = mean(log(x)), sd = sd(log(x))),
  sim = 100, mc.cores = 4L)
)


