
## ----functions-001-------------------------------------------------------
f <- function(x, y = 0) {
  z <- x + y
  z
}


## ----functions-002-------------------------------------------------------
formals(f)
body(f)
environment(f)


## ----functions-003-------------------------------------------------------
is.null(pairlist())
is.null(list())


## ----functions-004-------------------------------------------------------

mean(x = 1:5, trim = 0.1)
mean(1:5, trim = 0.1)
mean(x = 1:5, 0.1)
mean(1:5, 0.1)
mean(trim = 0.1, x = 1:5)


## ----functions-005-------------------------------------------------------
mean(1:5, tr = 0.1)
mean(tr = 0.1, x = 1:5)


## ----functions-006-------------------------------------------------------
mean(c(1, 2, NA))


## ----functions-007-------------------------------------------------------
mean(c(1, 2, NA), na.rm = TRUE)


## ----functions-008-------------------------------------------------------
formals(f)


## ----functions-009-------------------------------------------------------
args(f)


## ----functions-010-------------------------------------------------------
is.list(formals(mean))


## ----functions-011-------------------------------------------------------
exists("formals<-")


## ----functions-012-------------------------------------------------------
g <- function(x, y=0) x+y
g(1)
formals(g) <- alist(x=, y=1)
g(1)


## ----functions-013-------------------------------------------------------
formals(mean.default)[3] <- TRUE
mean(c(1,2,NA))


## ----functions-014-------------------------------------------------------
exists("mean.default", envir = globalenv())


## ----functions-015-------------------------------------------------------
h <- function (x, ...) {0}
formals(h)


## ----functions-016-------------------------------------------------------
count_rows <- function(...) {
  list <- list(...)
  lapply(list, nrow)
}

count_rows(airquality, cars)


## ----functions-017-------------------------------------------------------
time = 1:13
depth = c(0,9,18,21,21,21,21,18,9,3,3,3,0)

plot.depth = function ( time , depth , type = "l", ...){
  plot(time, -depth, type = type, 
       ylab = deparse(substitute(depth)), ...)
}
par(mfrow = c(1, 2))
plot.depth(time, depth, lty = 2)
plot.depth(time, depth, lwd = 4, col = "red")


## ----functions-018-------------------------------------------------------
right <- function(x){x+y}


## ----functions-019-------------------------------------------------------
right(x = 2)


## ----functions-020-------------------------------------------------------
f <- function(x) {x+1}
class(body(f))


## ----functions-021-------------------------------------------------------
as.list(body(f))


## ----functions-022-------------------------------------------------------
body(f)[[1]] <- `-`
f(1)


## ----functions-023-------------------------------------------------------
f <- function(x){x+1}
environment(f)


## ----functions-024-------------------------------------------------------
environment(mean)


## ----functions-025-------------------------------------------------------
env <- new.env()

with(env,{ 
     y <- 1
     g <- function(x){x+y}
     })


with(env, g(1))


## ----functions-026-------------------------------------------------------
y <- 1
g <- function(x){x+y}
g(2)


## ----functions-027-------------------------------------------------------
rm(y)


## ----functions-028-------------------------------------------------------
g(1)


## ----functions-029-------------------------------------------------------
f <-  function(x){
  env <-  environment()
  env
}

ef <- f(x = 0)
get("x", envir = ef)


## ----functions-030-------------------------------------------------------
env <- new.env(parent = baseenv()) 
with(env, f <- function(x) {is.function(x)})


## ----functions-031-------------------------------------------------------
with(env , f(x = c))


## ----functions-032-------------------------------------------------------
with(env, c <- 0) 


## ----functions-033-------------------------------------------------------
with(env, f(x = c))


## ----functions-034-------------------------------------------------------
remove(c, envir = env)


## ----functions-035-------------------------------------------------------
c <- 0


## ----functions-036-------------------------------------------------------
with(env, f(x = c))


## ----functions-037-------------------------------------------------------
f <- function(){
  evaluated_in <- environment()
  defined_in <- parent.env(evaluated_in)
  called_from <- parent.frame(n = 1)

  c(evaluated_in = evaluated_in, defined_in = defined_in, called_from = called_from)
}
f()


## ----functions-038-------------------------------------------------------
env <- new.env()
with(env, f <- function(){
  evaluated_in <- environment()
  defined_in <- parent.env(evaluated_in)
  called_from <- parent.frame(n = 1)
  c(evaluated_in = evaluated_in, defined_in = defined_in, called_from = called_from)
})
env$f()


## ----functions-039-------------------------------------------------------
env <- new.env(parent = baseenv()) 
with(env, fe <- function(x) {
  x <- eval(x, envir = parent.frame())
  is.function(x)
  })


## ----functions-040-------------------------------------------------------
attach(env)
fe(c)
c <- 1
fe(c)
rm(c)
detach(env)


## ----functions-041-------------------------------------------------------
env <- new.env(parent = baseenv()) 
with(env, f <- function() eval(pi, parent.env(environment())))


## ----functions-042-------------------------------------------------------
attach(env)
f()
pi <- 0
f()
detach(env)


## ----functions-043-------------------------------------------------------
g <-  function (n){
 out <- runif(n)
 cat(head(out))
 invisible(out)
}

x <-  g(10^5)
length(x)


## ----functions-044-------------------------------------------------------
msg <- function(x){
  cat(x, "\n")
  invisible(NULL)
}


## ----functions-045-------------------------------------------------------
msg("test message")


## ----functions-046-------------------------------------------------------
"%+%" = function(x,y){paste(x, y, sep = "")} 
"we " %+% "love " %+% "R !"


## ----functions-047-------------------------------------------------------
methods(`+`)


## ----functions-048-------------------------------------------------------
string <- function(x) {
  s <- as.character(x)
  class(s) <- "string"
  s
}


## ----functions-049-------------------------------------------------------
`+.string` <- function(s1, s2) paste(s1, s2, sep = "")


## ----functions-050-------------------------------------------------------
a <- string("Mickey")
b <- string("Mouse")
a+b


## ----functions-051-------------------------------------------------------
f = function(x, y){
  x+1
}


## ----functions-052-------------------------------------------------------
f(x = 0, y = z)


## ----functions-053, eval=TRUE--------------------------------------------
h <-  function(a , b){
cat ("a is:", a, "\n")
cat ("b is:", b, "\n")
invisible(NULL)
}


## ----functions-054-------------------------------------------------------
h(a = "we love R")


## ----functions-055-------------------------------------------------------
g <-function(x, y){
  call <- match.call()
  args <- match(c("x", "y"), names(call))
  if(any(is.na(args))) stop("All args must be provided!")
  pi
}

g(y = 1)



## ----functions-056-------------------------------------------------------
rescale = function(x, location = min(x), scale = max(y)){
  y = x-location
  y/scale
}
rescale(1:4)


## ----functions-057, eval=FALSE-------------------------------------------
## mean(x = 1:100, trim = 0.2)


## ----functions-058, eval=FALSE-------------------------------------------
## do.call("mean", list(x = 1:100, trim = 0.2))


## ----functions-059-------------------------------------------------------
mle = function(theta, x){
  ml = function(theta, x) {
    ml = dnorm(x = x, mean = theta[1], sd = theta[2])
    ml = -sum(log(ml))
    }
    optim(theta, ml, x = x)$par
}
mle(theta = c(0, 1), x = rnorm(100, 5, 2))


## ----functions-060-------------------------------------------------------
mle = function(theta, x){
  ml = function(theta, x) {
    ml = do.call(dnorm, list(x, theta[1], theta[2]))
    ml = -sum(log(ml))
  }
  optim(theta, ml, x = x)$par
}

mle(theta = c(0, 1), x = rnorm(100, 5, 2))


## ----functions-061-------------------------------------------------------
mle = function(theta, x, dist){
  dist = paste("d", dist , sep = "")
  ml = function(dist , theta, x) {
    ml = do.call(dist, list(x, theta[1], theta[2]))
    -sum(log(ml))
  }  
  optim(theta,  ml, dist = dist  , x = x)$par
}
mle(dist = "norm" , theta = c(0, 1), x = rnorm(10, 5, 2))


## ----functions-062-------------------------------------------------------
mle(dist = "lnorm" , theta = c(0,1), x = rlnorm(100, 3, 1))
mle(dist = "weibull" , theta = c(1,1), x = rweibull(100, 3, 1))


## ----functions-063-------------------------------------------------------
f = function(a, b){
  call = match.call()
  call}
  
my_call = f(2, 3)  
my_call
class(my_call)  


## ----functions-064-------------------------------------------------------
my_call_list <- as.list(my_call)
my_call_list


## ----functions-065-------------------------------------------------------
my_call$a <- 0
eval(my_call)


## ----functions-066-------------------------------------------------------
anyway = function(a , b){
  call = match.call()
  names(call) = ""
  if (is.numeric(a) & is.numeric(b)) {call[[1]] = as.name("sum")} 
    else {
      call[[1]] = as.name("paste" )
      call$sep = "+"
    }
eval(call)
}

anyway(3, 6)
anyway("c", 2)


## ----functions-067-------------------------------------------------------
write.csv <-  function(...) write.table(sep = ",", dec = ".", ...)
siris <- head(iris, 3)
write.csv(siris)


## ----functions-068-------------------------------------------------------
write.csv(siris, sep = ";")


## ----functions-069-------------------------------------------------------
write.csv = function(...){
  call = match.call()
  call[[1]] = as.name("write.table")
  call$sep = ","
  call$dec = "."
  eval(call)
}

write.csv(siris, sep = ";")


## ----functions-070-------------------------------------------------------
one_c <- function(x){
  while (x > 2){
    x <- x/2
}
x
}
one_c(10)


## ----functions-071-------------------------------------------------------
one_r <- function(x){
  if (x > 2 ){
   x <- x/2
   x <- Recall(x)
  }
x  
}
one_r(10)


## ----functions-072-------------------------------------------------------
quick_sort_r  <- function(x) {
  
  if(length(x) > 1) {
    base <- x[1]
    l <- Recall(x[x < base])
    m <- x[x == base]
    h <- Recall(x[x > base])
    
    c(l, m, h)
  }
  else x
}


## ----functions-073-------------------------------------------------------
quick_sort_r(sample(1:10))


## ----functions-074-------------------------------------------------------
quick_sort_c <- function(x , max_lev = 1000) {
  n <- length(x)
  i <- 1
  beg <- end <- max_lev
  beg[1] <- 1 
  end[1] <- n+1
  
  while (i>=1) {
    L <- beg[i]
    R <- end[i]-1
    if (L<R) {
      piv <- x[L] 
      if (i == max_lev) 
        stop("Error: max_lev reached");
      
      while (L<R) {
        while (x[R]>=piv && L<R){ 
          R <- R-1
        }
        if (L < R){
          x[L] <-  x[R]
          L <- L+1
        }
        while (x[L]<=piv && L<R){
          L <- L+1
        }
        if (L<R) {
          x[R] <- x[L]
          R <- R-1
        }
      }
      x[L] <- piv
      beg[i+1] <- L+1
      end[i+1] <- end[i]
      end[i] <- L
      i <- i+1
    }
    else {
      i <- i-1 
    }
  }
  return( x) 
}


## ----functions-075-------------------------------------------------------
quick_sort_c(sample(1:10))


## ----functions-076-------------------------------------------------------
x <- sample(1:10^5)
system.time(quick_sort_r(sample(x)))
system.time(quick_sort_c(sample(x)))


## ----functions-077-------------------------------------------------------
df <-  data.frame(x = 1:3, y = 3:1)


## ----functions-078-------------------------------------------------------
names(df) <-  c("xx", "yy")


## ----functions-079-------------------------------------------------------
get("names<-")


## ----functions-080-------------------------------------------------------
trim <-  function(x, p){
  x[x <= quantile(x, p)]
}

trim(1:10, p = .25)


## ----functions-081-------------------------------------------------------
"trim<-" <-  function (x, p, value){
  x[x <= quantile(x, p)] <-  value
  x
}


## ----functions-082-------------------------------------------------------
y <- 1:10
trim(x = y, p = .25) <-  0


## ----functions-083-------------------------------------------------------
df <- data.frame(x = 0, y = 1)
names(df) <- c("a", "b")


## ----functions-084-------------------------------------------------------
names(data.frame(x = 0, y = 1)) <- c("a", "b")


## ----functions-085-------------------------------------------------------
(function(x) x + 3)(10)


## ----functions-086-------------------------------------------------------
f <- function(x) x + 3
f(10)


## ----functions-087-------------------------------------------------------
cv <- function(x){sd(x)/mean(x)}


## ----functions-088, eval = FALSE-----------------------------------------
## lapply(cars, cv)


## ----functions-089, eval = FALSE-----------------------------------------
## lapply(cars, function(x) sd(x)/mean(x))


## ----functions-090-------------------------------------------------------
formals(function(x) x+1)
body(function(x) x+1)
environment(function(x) x+1)


