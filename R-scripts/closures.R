
## ----closures-001--------------------------------------------------------
f <- function(x) 0
environment(f)


## ----closures-002--------------------------------------------------------
environment(mean)


## ----closures-003--------------------------------------------------------
y <- 1 
f <- function(x){x+y}
f(1)


## ----closures-004--------------------------------------------------------
f <- function(x){x+pi}
f(1)


## ----closures-005--------------------------------------------------------
g <- function(x){
  y <- 1
  function(x) {x+y}
}
f1 <- g()
f1(3)


## ----closures-006--------------------------------------------------------
g <- function (y) {
    function(x) x+y
}
f1 <- g(1)
f1(3)


## ----closures-007--------------------------------------------------------
g <- function(y){
  print(environment())
  function(x) {x+y}
}


## ----closures-008--------------------------------------------------------
f1 <- g(1)


## ----closures-009--------------------------------------------------------
environment(f1)


## ----closures-010--------------------------------------------------------
get("y" , env = environment(f1))


## ----closures-011--------------------------------------------------------
f2 <- g(1)
environment(f1)
environment(f2)


## ----closures-012--------------------------------------------------------
add <- function(x, i){
  x+i
}


## ----closures-013--------------------------------------------------------
f <- function(i){
  function(x) {x+i}
}


## ----closures-014--------------------------------------------------------
f1 <-  f(1)
f1(3)
f2 <- f(2)
f2(4)


## ----closures-015--------------------------------------------------------
new_estimate <-  function(dist){
  estimate <-  function(x, theta){   
    neglik <-  function(theta = theta , x = x, log = T){
      args <-  c(list(x), as.list(theta), as.list(log))
      neglik <-  -sum(do.call(dist,  args))
      neglik
    }
    optim(par = theta, fn = neglik , x = x)$par
  }
estimate}


## ----closures-016--------------------------------------------------------
lnorm <- new_estimate("dnorm")
y <- rnorm(100, 7 , 2)
lnorm(y, theta = c(mean(y), sd(y)))


## ----closures-017--------------------------------------------------------
lweibull <- new_estimate("dweibull")
w <- rweibull(100, 2 , 1)
lweibull(w, theta = c(mean(w), sd(w)))


## ----closures-018--------------------------------------------------------
g <- function(i , x, n , f, ...) f(x[(i-n+1):i], ...)
g(i = 5 , x = 1:10,n = 3  , f= mean) 
g(i = 5 , x = 1:10,n = 3  , f= sd) 



## ----closures-019--------------------------------------------------------
moving <- function(f){
  g <- function(i , x, n , f, ...) f(x[(i-n+1):i], ...)
  h <- function(x, n, ...) {
    N <- length(x)
    vapply(n:N, g, x , n , f, FUN.VALUE = numeric(1), ...)
  }
return(h)  
}


## ----closures-020--------------------------------------------------------
moving_average <- moving(mean)  
moving_average(x = rpois(10, 10), n = 3)


## ----closures-021--------------------------------------------------------
moving_average(x = rpois(10, 10), n = 3, trim = .5)


## ----closures-022--------------------------------------------------------
moving(sd)(rpois(10, 10), n = 5)


## ----closures-023,  fig.width=7, fig.height=4, fig.cap="Plot of moving average and median"----

x <- 1:100
y <- seq(along = x, from = 0 , to = 1)+rnorm(length(x), 0 , .1)
plot(x, y)
lines(x[10:length(x)], moving(mean)(y, 10), col = "red", lwd = 2)
lines(x[10:length(x)], moving(median)(y, 10), col = "green", lwd = 2)


## ----closures-024--------------------------------------------------------
x <- c(7,10,13)
dlnorm(x , meanlog = 2, sdlog = 1)


## ----closures-025--------------------------------------------------------
require(truncdist)
dtrunc(x, spec = "lnorm", a = 5)


## ----closures-026--------------------------------------------------------
a_dlnorm <-  function (x, meanlog = 0, sdlog = 1,  L = 0,  H = Inf) 
 {
  
  density <-  
     stats::dlnorm(x, meanlog=meanlog, sdlog=sdlog)/
        (
        stats::plnorm(H, meanlog=meanlog, sdlog=sdlog)-  
        stats::plnorm(L, meanlog=meanlog, sdlog=sdlog)  
          )
              
   return(density)
 }


## ----closures-027--------------------------------------------------------
a_dlnorm(x, 1, 2, L= 5, H = 20)
dtrunc(x, spec = "lnorm", a = 5, b = 20, meanlog = 1, sdlog = 2)


## ----closures-028--------------------------------------------------------
dtruncate <- function (dist,
          ddist=paste("d", dist, sep = ""), 
          pdist=paste("p", dist, sep = ""),
          L=-Inf,
          H=Inf,
          envir = as.environment("package:stats")){ 

  #gets density function                    
  ddist <- get(ddist, mode = "function", envir = envir)
  #gets argument of density function
  dargs <- formals(ddist)
  
  #gets probability function                    
  pdist <- get(pdist, mode = "function", envir = envir)
  #gets argument of probability function
  pargs <- formals(pdist)
  
  #Output function starts here
  density <- function (...) 
  {
    #check L H 
    if (L > H) stop("H must be greater than or equal to L")
    
    #gets density arguments
    call <- as.list(match.call())[-1]
    
    #call[is.element(names(call), names(dargs))] -> 
    #arguments passed to density and belonging to dargs 
    #i.e. arguments belonging to density function ddist
    
    #c(dargs[!is.element(names(dargs), names(call))]
    #arguments belonging to dargs 
    #i.e. arguments belonging to density function ddist
    # and not being part of arguments passed to density 
    
    #as a result, the whole string gets all unique arguments 
    #belonging to density function and ddist 
    dargs <- c(dargs[!is.element(names(dargs), names(call))], 
              call[is.element(names(call),names(dargs))])
    
    #all unique arguments belonging to probability and pdist 
    pargs <- c(pargs[!is.element(names(pargs), names(call))], 
              call[is.element(names(call), names(pargs))])
    
    #select x only where defined by L and H
    dargs$x <- x[x > L & x <= H]
    
    #define arguments for pdist in L and H
    pHargs <- pLargs <-pargs 
    pHargs$q <- H
    pLargs$q <- L
    
    #initialize output
    density <- numeric(length(x))
    
    #this is standard method for computing density values for truncated distributions
    density[x > L & x <= H] <-  do.call("ddist", as.list(dargs)) / (do.call("pdist", as.list(pHargs)) - do.call("pdist", as.list(pLargs)))
    
    #returns density values for truncated distributions
    return(density)
    
  }
  
  #add to density function formals L and H with values as passed with dtruncate
  formals(density) <- c(formals(ddist), eval(substitute(alist(L=L, H=H))))
  #return density function
  density
}


## ----closures-029--------------------------------------------------------
dlnorm <- dtruncate(dist = "lnorm", L=-Inf, H=Inf, envir = as.environment("package:stats"))


## ----closures-030--------------------------------------------------------
dlnorm(x, 1, 2, L= 5, H = 20)


## ----closures-031--------------------------------------------------------
dtrunc(x, spec = "lnorm", a = 5, b = 20, meanlog = 1, sdlog = 2)


## ----closures-032--------------------------------------------------------
dweibull <-  dtruncate(dist = "weibull", L=0, H=Inf, 
                       envir = as.environment("package:stats"))
dgpd <- dtruncate(dist = "gpd", L=0, H=Inf, 
                  envir = as.environment("package:evd")) 


## ----closures-033--------------------------------------------------------
g <- function(){
 i <- 0
 f <- function(){
    i <<- i+1
    cat("this function has been called ", i, " times", "\n")
    date()  
}}

f <- g()
#first call
f()
#second call
f() 
#third call
f()


## ----closures-034--------------------------------------------------------
library(pracma)
primes(n = 9)


## ----closures-035--------------------------------------------------------
makefprime = function () {
  .env = new.env()
  f = function(n) {
    symbol = paste("p", n, sep = ".")
    if (exists(symbol, envir = .env)){
      prime = get(symbol, envir = .env)
    } 
    else {prime = primes(n = n)
      assign(symbol , prime, envir = .env)
    }
    prime
   }  
f
}


## ----closures-036--------------------------------------------------------
fprimes = makefprime()
fprimes(10)


## ----closures-037--------------------------------------------------------
system.time({p1 = fprimes(n = 10^7)})


## ----closures-038--------------------------------------------------------
system.time({p2 = fprimes(n = 10^7)})


## ----closures-039--------------------------------------------------------
make_flip <- function(){
now <- as.numeric(format(Sys.time(), "%S"))
  flip <- function() { 
    coin <- ifelse(now %% 2 == 0 , "head", "tail")
    cat("This outcome is ", sQuote(coin), "\n")
    cat("Previous time call was ", now,"\n")
    now <<- as.numeric(format(Sys.time(), "%S"))
    cat("This time call is ", now, "\n")
    cat("-------------------------","\n")
    invisible(coin)
  }
flip
}


## ----closures-040--------------------------------------------------------
flip_the_coin  <- make_flip()


## ----closures-041--------------------------------------------------------
flip_the_coin()
Sys.sleep(sample(1:5, 1))
flip_the_coin()
Sys.sleep(sample(1:5, 1))
flip_the_coin()


## ----closures-042--------------------------------------------------------
f <- g()
system.time(f(1))
system.time(f(2))


## ----closures-043--------------------------------------------------------
new_plot = function(){
  xx = NULL
  yy = NULL
  function(x, y, ...) {
  xx <<- c(xx, x)
  yy <<- c(yy, y)
  plot(xx, yy, ...)
}}

this_plot <- new_plot()


## ----closures-044, fig.width=7, fig.height=4, fig.cap="first call"-------
this_plot (1:4, c(2, 3, 1, 5), type = "b")


## ----closures-045, fig.width=7, fig.height=4, fig.cap="second call"------
this_plot(5, 3, type = "b")


## ----closures-046, fig.width=7, fig.height=4, fig.cap="third call"-------
this_plot(6, 3, type = "b", col = "red")


