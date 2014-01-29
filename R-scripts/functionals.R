
## ----functionals-001-----------------------------------------------------
lapply(list(one = 1, a = "a"), is.numeric)


## ----functionals-002-----------------------------------------------------
n <- ncol(airquality)
out <- numeric(n)
for (i in 1:n){
 out[i] <- max(airquality[,i], na.rm = TRUE)
}
out


## ----functionals-003-----------------------------------------------------
lapply(X=airquality, FUN = max, na.rm = TRUE)


## ----functionals-004-----------------------------------------------------
mean(1:100, trim = 0.1)
mean(0.1, x = 1:100)


## ----functionals-005-----------------------------------------------------
x <- rnorm(100)
lapply(X = c(0.1, 0.2, 0.5), mean, x = x)


## ----functionals-006-----------------------------------------------------
sapply(X=airquality, FUN = max, na.rm = TRUE)


## ----functionals-007-----------------------------------------------------
sapply(list(), is.numeric)


## ----functionals-008-----------------------------------------------------
vapply(list(), is.numeric, FUN.VALUE = logical(1))
vapply(X=airquality, FUN = max, na.rm = TRUE, FUN.VALUE = numeric(1))


## ----functionals-009-----------------------------------------------------
df_list <- list(cars, airquality, trees) 


## ----functionals-010-----------------------------------------------------
sapply(df_list, ncol)


## ----functionals-011-----------------------------------------------------
df_list <- list(df1 = cars, df2 = NULL, df3 = trees) 


## ----functionals-012-----------------------------------------------------
sapply(df_list, ncol)


## ----functionals-013-----------------------------------------------------
vapply(df_list, ncol, FUN.VALUE = numeric(1))


## ----functionals-014-----------------------------------------------------
n <- sample(1:5, 10^6 , rep = T)
vector_list <- lapply(n, sample , x = 0:9)


## ----functionals-015-----------------------------------------------------
system.time(
  sapply(vector_list, length )
)

system.time(
  vapply(vector_list, length, FUN.VALUE = numeric(1))
)


## ----functionals-016-----------------------------------------------------
x = runif(3)
vapply(x, function(x) x*x, numeric(1))


## ----functionals-017-----------------------------------------------------
x = runif(3)
nx = length(x)
vapply(1:nx, function(i , x) x[i]*x[i], x=x , FUN.VALUE=numeric(1))


## ----functionals-018-----------------------------------------------------
n = ncol(trees)
out = numeric(n)
trim = c(0.1, 0.2, 0.3)
for ( i in 1:n){
  out[i] = mean(trees[,i], trim[i], na.rm = TRUE)
}
out


## ----functionals-019-----------------------------------------------------
lapply(1:ncol(trees), 
  function(i, x, trim, ...) mean(x[,i], trim[i], na.rm = TRUE), 
  x = trees, trim = c(0.1, 0.2, 0.3))


## ----functionals-020-----------------------------------------------------
ni = 4
nj = 2

nk = ni*nj
k = numeric(nk)
for (j in 1:nj){
  if ( j %% 2 == 0){  
    for ( i in 1:ni){
      if ( i %% 2 == 0 ) next_k = i+j 
      else next_k = i-j
      cat("first ij " , i , j , next_k, "\n")
      k[i+(j-1)*ni] = next_k
    }
  }  
  else {
    for (i in 1:ni){
    next_k <- 99
    cat("second ij " , i , j , next_k, "\n")
    k[i+(j-1)*ni] = next_k
    }
  }
}



## ----functionals-021-----------------------------------------------------
f = function(k , i , j) {
  i <- i[k]
  j <- j[k]
  result = 99
  if( j %% 2 == 0){
  result <- ifelse(i %% 2 == 0 , i+j , i-j)
  }
  result
}


## ----functionals-022-----------------------------------------------------
grid <- expand.grid(i = 1:ni, j = 1:nj )
with(grid , vapply(1:nrow(grid), f, i=i , j=j, FUN.VALUE = numeric(1)))


## ----functionals-023-----------------------------------------------------
f = function(i , j) {
  ifelse( j %% 2 == 0,
    ifelse(i %% 2 == 0 , i+j , i-j),
  99)        
}


## ----functionals-024-----------------------------------------------------
unlist(
lapply(1:2, function(j, i = ni)   vapply(1:4, FUN = f, j, FUN.VALUE = numeric(1)))
)



## ----functionals-025-----------------------------------------------------
mapply(mean ,trees, trim = c(0.1 , 0.2, 0.3), 
  MoreArgs = list(na.rm = TRUE),
  SIMPLIFY = FALSE) 


## ----functionals-026-----------------------------------------------------
Map(function(...) mean(..., na.rm = TRUE), 
  x = trees , trim = c(0.1 , 0.2, 0.3))


## ----functionals-027-----------------------------------------------------
f = function(i , j) {
  result = 99
  if( j %% 2 == 0){
  result <- ifelse(i %% 2 == 0 , i+j , i-j)
  }
  result
}

grid <- expand.grid(i = 1:4, j = 1:2)


## ----functionals-028-----------------------------------------------------
with(grid , mapply(f, i=i , j=j, SIMPLIFY = TRUE))


## ----functionals-029-----------------------------------------------------
unlist(with(grid , Map(f, i=i , j=j)))


## ----functionals-030-----------------------------------------------------
env <- new.env()
env$x = 3 ; env$y = -2
eapply(env, function(x) ifelse(x>0 , 1 , -1))


