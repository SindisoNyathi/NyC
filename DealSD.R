#Function to combine SD 
dealsd <- function(x) {
  
  #No. of different groups of three
  no_sets <- ncol(x)/3
  
  conI <- vector("numeric", length = ncol(x))
  
  for (i in 1:no_sets) {
    
    this.mean <- 3*(i-1) + 1
    this.upper <- 3*(i-1) + 2
    this.lower <- 3*(i-1) + 3
    
    conI[this.mean] <- mean(x[,this.mean], na.rm = TRUE)
    new.variance <- mean(((x[,this.upper] - x[,this.lower])*3.007608)^2, na.rm = TRUE)
    new.upper <- conI[this.mean] + 1.96*(sqrt(new.variance)/10)
    new.lower <- conI[this.mean] - 1.96*(sqrt(new.variance)/10)
    
    conI[this.upper] <- new.upper
    conI[this.lower] <- new.lower
  }
  return(conI)
}