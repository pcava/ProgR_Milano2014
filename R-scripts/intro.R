
## ----intro-001-----------------------------------------------------------
makeActiveBinding("whoami", 
    function() sample(c("Mr.Hyde", "Dr.Jekill"), 1), globalenv())
for ( i in 1:5) cat(whoami, " - ")


