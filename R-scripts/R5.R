
## ----RC_01, tidy = FALSE-------------------------------------------------
zero_one  <- setRefClass("zero_one",
                      fields = list( x = "numeric"),
                      methods = list(
                        set_to_zero = function(x){
                          x <<- 0
                        },
                        set_to_one = function(x){
                          x <<- 1
                        }
                      )  
)


## ----RC_02---------------------------------------------------------------
zero_one_test <- zero_one$new(x = 33)
zero_one_test


## ----RC_03---------------------------------------------------------------
zero_one_test$set_to_zero()


## ----RC_04---------------------------------------------------------------
zero_one_test


## ----RC_05, tidy=FALSE---------------------------------------------------
stack  <- setRefClass("stack",
                      fields = list( stack = "numeric"),
                      methods = list(
                        put_in = function(x){
                          stack <<- c(stack, x)
                        },
                        get_out = function(n = 1 , method = "fifo"){
                          stopifnot(method %in% c("fifo", "lilo"))
                          if(method == "fifo"){
                            first <- 1:n
                            stack <<- stack[-first]
                          }
                          if(method == "lilo"){
                            N <- length(stack)
                            last <- c((N-n+1):N)
                            #                         cat(N, " - ", last, "\n")
                            stack <<- stack[-last]
                            
                          }
                        }  
                      )
)


## ----RC_06---------------------------------------------------------------
stack_test <-stack$new(stack = 0)
stack_test$put_in(1:10) 
stack_test$get_out(method = "fifo", n = 2) 
stack_test
stack_test$get_out(method = "lilo", n = 2) 
stack_test


