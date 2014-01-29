
## ----packages-001--------------------------------------------------------
rm(list = ls())
itself = function(x) print(x)
paste2 = function(x, y) print(paste(x, y)) 
set.seed(2012) 
df = data.frame(x = rnorm(100), y = rnorm(100))
ls()


## ----packages-002, eval=FALSE--------------------------------------------
## package.skeleton(name = "simpleExample")


## ----packages-003, eval=FALSE--------------------------------------------
## library(simpleExample)


## ----packages-004--------------------------------------------------------
itself(5)
paste2("Score", 23)


