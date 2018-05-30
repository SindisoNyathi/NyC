#Function for dealing with BMIs

split_bmi <- function(thing) {
  
  #Make them characters
  thing <- str_replace(thing, "\\[", "") %>%
    str_replace("\\]", "") %>%
    str_split(pattern = ",") %>%
    unlist()%>%
    as.numeric()
  
  #Correct the lenght of thing to 44.
  thing.size <- length(thing)
  
   if (thing.size < 44) {
     new.thing <- vector("numeric", 44)
     new.thing[c(1:thing.size)] <- thing[c(1:thing.size)]
     new.thing[c((thing.size+1):44)] <- NA
     thing <- new.thing
   }
  
  #return the thing.
  return(thing)
}