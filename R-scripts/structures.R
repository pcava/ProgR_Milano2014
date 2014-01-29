
## ----structures-001------------------------------------------------------
languages <- c("C", "C++", "R", "Java", "Python")
for(lang in languages) {
  cat("I love ", lang, "\n")

}


## ----structures-002------------------------------------------------------
languages <- c("C", "C++", "R", "Java", "Python")
n <- length(languages)
for(i  in 1:n) {
  cat("I love ", languages[i], "\n")

}


## ----structures-003, include =FALSE--------------------------------------
rm(list = ls())
g = gc()


## ----structures-004------------------------------------------------------
k = 100000
n = 10
z = NULL 
system.time({
    for (i in 1:k) {
    x = rnorm(n , 0, 1)
    y = rnorm(n , 0, 1)
    z = c(z , cor(x, y))
  }
  cat ("95th quantile = " , quantile(z , .95), "\n")
})[3]


## ----structures-005------------------------------------------------------
k = 100000
n = 10
z = numeric(k)
system.time({
  for ( i in 1:k){
    x = rnorm(n , 0, 1)
    y = rnorm(n , 0, 1)
    z[i] = cor(x, y)
  }
  cat ("95th quantile = " , quantile(z , .95), "\n")
})[3]


## ----structures-006------------------------------------------------------
slow =  function(x, y) {  
  nx = length(x)
  ny = length(y)  
  xy = numeric(nx + ny - 1)  
  
  for(i in 1:nx) {  
         for(j in 1:ny) {  
              ij = i+j-1  
              xy[ij] = xy[ij] + x[i] * y[j]  
          }  
      }  
      xy  
}  


system.time(slow(runif(1000), runif(1000)))[3]


## ----structures-007------------------------------------------------------
fast =  function(x, y) {  
  nx = length(x)
  ny = length(y)  
  xy = numeric(nx + ny - 1)  
  
  for(i in 1:nx) {  
              j = 1:ny
              ij = i+j-1  
              xy[ij] = xy[ij] + x[i] * y  
      }  
      xy  
}
system.time(fast(runif(1000), runif(1000)))[3]


## ----structures-008------------------------------------------------------
if(x < 5){"LESS"} else {"GREATER"}


## ----structures-009------------------------------------------------------
x = 1:10
ifelse(x < 5, "LESS", "GREATER")


## ----structures-010, include =FALSE--------------------------------------
rm(list = ls())
g = gc()


## ----structures-011------------------------------------------------------
system.time({
  det = NULL
  for ( a in 1:20)
    for(b in 1:20)
      for(d in 1:20)
        for(e in 1:20)
          det = c(det, a*b-d*e)
  cat ("95th quantile = " , quantile(det , .95), "\n")
})[3]


## ----structures-012, include =FALSE--------------------------------------
rm(list = ls())
g = gc()


## ----structures-013------------------------------------------------------
system.time({
  half = outer(1:20, 1:20, "*")
  det = outer(half, half, "-")
  cat ("95th quantile = " , quantile(det , .95), "\n")
})[3]


## ----structures-014------------------------------------------------------
x = y = 1:3
outer(x, y , "+")


## ----structures-015------------------------------------------------------
f = function(x){
  n = length(x)
  y = numeric(n)
  for ( i in 2:n) {
    if(x[i-1]==0) { y[i] = 1 }
    else {y[i] = 0}
  }
  y
}

n = 10000
x = sample(0:1, n, rep = T)
system.time(f(x))


## ----structures-016------------------------------------------------------
library(compiler)
cf = cmpfun(f)
system.time(cf(x))


## ----structures-017------------------------------------------------------
system.time(c(0, ifelse(x[2:n] == 0 , 1, 0)))


## ----gain----------------------------------------------------------------
gain = function(bill , free) {
  who = bill$who
  paid = bill$paid
  k = length(free)
  n = length(who)
  
  gain = numeric(n)
  
  i = 2
  j = 1
  
  for (i in 1:n) {
    for ( j in 1:k){
      if(is.element (who[i], free[[j]][["who"]]))
      {
        if (free[[j]][["free"]] >= paid[i])
        {
          gain[i] = paid[i]
          free[[j]][["free"]] == free[[j]][["free"]] - paid[i]
        } 
        else {
          gain[i] = free[[j]][["free"]]
          free[[j]][["free"]] = 0 
        }
      break()
      }
    }
  }
  
  data.frame(who, paid, gain)
}

library(compiler)
cgain = cmpfun(gain)


## ----billfree------------------------------------------------------------
n = 10000
bill = data.frame(
      who = sample(1:10, n, rep = T),
      paid = round(rexp(n , 1/10),2))
free = list(slot1 = list (who = 1, free = 1000),  list (who = 1:5, free = 100))


## ----plotcc--------------------------------------------------------------
size = c(1000, 10000, 100000, 250000, 500000)
N = length(size)
ct = ft = numeric(length(size))

for ( p in 1:N){
  bill = data.frame(
      who = sample(1:10, size[p], rep = T),
      paid = round(rexp(size [p] , 1/10),2))
  ft[p] = system.time(gain (bill , free))[3]
  ct[p] = system.time(cgain (bill , free))[3]
}


plot(size, ft, type = "b", lty = 1, 
     ylab = "Elapsed Time (sec)", xlab = "Data Size (# records)")
points(size, ct, type = "b", lty = 2)
legend("topleft", c("Not Compiled", "Compiled") , 
       lty = 1:2, cex = 2, bty = "n")


## ----structures-021, eval = FALSE----------------------------------------
## myCon = file("sqltest", open = "w+")
## lambda = 1000; n = 10000000
## np = rpois(n, lambda)
## for (i in 1:n){
##    out = sum(rlnorm(np[i], meanlog = 9 , sdlog = 2))
##    writeLines(as.character(out), con = myCon)
## }
## out = scan(myCon)
## # Read 10000000 items
## cat ("95th quantile = " , quantile(out , .95), "\n")
## # 95th quantile =  81493686
## close(myCon)


