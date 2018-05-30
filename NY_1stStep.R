ny_master <- function(home, school, fold) {
  
  #Set time function.
  tic();
  
  #set wd to the given file.
  setwd(paste(home, "/", school, "/", fold, sep = ""))
  
  #List the files in the directory.
  all_files <- list.files(pattern = "_")
  
  #Set the number of runs
  no_r <- length(all_files)
  
  #Create and empty data frame
  final_ny <- data.frame(matrix(NA, nrow = 0, ncol = 139))
  final_ny_U <- data.frame(matrix(NA, nrow = 0, ncol = 139))
  final_ny_L <- data.frame(matrix(NA,  nrow = 0, ncol = 139))
  
  for (i in 1:no_r){
    
    #Read in the files.
    curr <- as.data.frame(read.csv(all_files[i]))
    
    #For the ones who were in the model for 3 years from the beggining.
    curr["InSim"] <- (as.numeric(as.character(curr$death_date)) - as.numeric(as.character(curr$birth_date))) + 1
    
    #In Sim for 3 years.
    curr <- curr[curr$InSim == 3,]
    #curr <- curr[curr$InSim == 3 & curr$initial_bmip < 0.85001,]
    
    #In Sim for 2 years
    #curr <- curr[curr$InSim == 2,]
    
    #In Sim for 1 year
    #curr <- curr[curr$InSim == 1,]
    
    #Deal with the fact that the BMIs and BMI percentiles are all in one column.
    #BMI should be in column 15
    curr_bmi <- curr$bmi
    
    curr_bmi <- lapply(curr_bmi, function(x) split_bmi(x)) %>%
      Reduce(f = rbind) %>%
      data.frame(row.names = NULL)
      #data.frame()
    
    #Do the same with percentile, in column 15
    curr_per <- curr$bmip
    
    #The appending
    curr_per_app <- as.numeric(as.character(curr$initial_bmip))
    
    curr_per <- lapply(curr_per, function(x) split_bmi(x)) %>% 
      Reduce(f = rbind) %>% 
      data.frame(row.names = NULL)
    curr_per <- cbind(curr_per_app, curr_per)
    
    curr_sn <- curr$snacking_calories

    curr_sn <- lapply(curr_sn, function(x) split_bmi(x)) %>% 
      Reduce(f = rbind) %>% 
      data.frame(row.names = NULL)
    
    curr_sne <- curr$snacking_events
    
    curr_sne <- lapply(curr_sne, function(x) split_bmi(x)) %>% 
      Reduce(f = rbind) %>% 
      data.frame(row.names = NULL)
    
    curr_sna <- curr_sn/curr_sne
    
    #for (i in 1:ncol(curr_sn)) {curr_sna <- cbind(curr_sna, curr_sn[,i]/curr_sne[,i])}
         
    #curr_sna <- as.data.frame(map2(curr_sn, curr_sne, map2("/")))
    
    #Now remove them from the file.  The file should be left iwth 15 columns
      curr <- data.frame(curr$X...0.1.kilometers, curr$X...0.274.kilometers, curr$X...0.804672.kilometers, 
                            curr$Bodegas, curr$Drug.Stores...Pharmacies, curr$Fast.food...Quick.service.restaurants, 
                            curr$Healthy, curr$In.School.Vending.Machines, curr$Supermarkets, 
                            curr$Unhealthy, row.names = NULL)
    
    #Make them all numerical
    curr <- lapply(curr, corr_type)
    curr <- as.data.frame(curr)
    
    #Appenf curr_bmi and ucrr_per to the end. 
    curr <- cbind(curr, curr_bmi)
    curr <- cbind(curr, curr_per)
    curr <- cbind(curr, curr_sna)
      
    #Now for all the other peeps in the file, save the means and CI in seperate row vectors.
    curr_means <- apply(curr, 2, mean, na.rm = TRUE)
    curr_upper <- (apply(curr, 2, ci, na.rm = TRUE))[3,]
    curr_lower <- (apply(curr, 2, ci, na.rm = TRUE))[2,]
  
    #curr[nrow(curr) + 1, c(2:42)] <- curr_means
    final_ny <- rbind(final_ny, curr_means)
    final_ny_U <- rbind(final_ny_U, curr_upper)
    final_ny_L <- rbind(final_ny_L, curr_lower)
    
    names(final_ny) <- names(curr)
    names(final_ny_U) <- names(curr)
    names(final_ny_L) <- names(curr)
    
    print(paste("Run ", i, " Complete . . . ", sep = ""))
    
  } 
  
  print("All runs complete, seperating and saving variables . . .")
  
  final_ny[no_r+1,] <- apply(final_ny, 2, mean)
  final_ny[no_r+2,] <- apply(final_ny, 2, ci, na.rm = TRUE)[3,]
  final_ny[no_r+3,] <- apply(final_ny, 2, ci, na.rm = TRUE)[2,]
  
  final_ny_U[no_r+1,] <- apply(final_ny_U, 2, mean)
  final_ny_U[no_r+2,] <- apply(final_ny_U, 2, ci, na.rm = TRUE)[3,]
  final_ny_U[no_r+3,] <- apply(final_ny_U, 2, ci, na.rm = TRUE)[2,]
  
  final_ny_L[no_r+1,] <- apply(final_ny_L, 2, mean)
  final_ny_L[no_r+2,] <- apply(final_ny_L, 2, ci, na.rm = TRUE)[3,]
  final_ny_L[no_r+3,] <- apply(final_ny_L, 2, ci, na.rm = TRUE)[2,]
  
  #Save the files as individual variable based files.
  #Distance.
  dist_file <- data.frame(final_ny[,1], final_ny_U[,1], final_ny_L[,1],
                            final_ny[,2], final_ny_U[,2], final_ny_L[,2],
                            final_ny[,3], final_ny_U[,3], final_ny_L[,3])
  names(dist_file) <- c("0.1km", "Upper", "Lower",
                        "0.274km", "Upper", "Lower",
                        "0.804km", "Upper", "Lower")
  
  write.csv(dist_file, "Distances.csv")
  
  #Stores
  stores_file <- data.frame(final_ny[,4], final_ny_U[,4], final_ny_L[,4],
                            final_ny[,5], final_ny_U[,5], final_ny_L[,5],
                            final_ny[,6], final_ny_U[,6], final_ny_L[,6],
                            final_ny[,8], final_ny_U[,8], final_ny_L[,8],
                            final_ny[,9], final_ny_U[,9], final_ny_L[,9])
  
  names(stores_file) <- c("Bodegas", "Upper", "Lower",
                         "Drug Stores", "Upper", "Lower",
                        "Fast Food", "Upper", "Lower",
                        "Vending Machines", "Upper", "Lower",
                        "Supermarkets", "Upper", "Lower")
  
  write.csv(stores_file, "Stores.csv")
  
  #Healthy
  health_file <- data.frame(final_ny[,7], final_ny_U[,7], final_ny_L[,7],
                              final_ny[,10], final_ny_U[,10], final_ny_L[,10])
  
  names(health_file) <- c("Healthy", "Upper", "Lower",
                          "Healthy", "Upper", "Lower")
  
  write.csv(health_file, "Healthy.csv")
  
  #BMIs
  bmis_file <- as.data.frame(matrix(nrow = no_r+3, ncol = 0))
  
  for (i in 11:54) { 
   
     bmis_file <- cbind(bmis_file, data.frame(final_ny[,i], final_ny_U[,i], final_ny_L[,i]))

  }  
  
  write.csv(bmis_file, "BMI.csv")
  
  #Percentiles
  pers_file <- as.data.frame(matrix(nrow = no_r+3, ncol = 0)) 
  for (i in 55:99) {
    pers_file <- cbind(pers_file, data.frame(final_ny[,i], final_ny_U[,i], final_ny_L[,i]))
  }
  
  write.csv(pers_file, "Percentile.csv")
  
  #Snacking
  sna_file <- as.data.frame(matrix(nrow = no_r+3, ncol = 0)) 
  for (i in 100:143) {
    sna_file <- cbind(sna_file, data.frame(final_ny[,i], final_ny_U[,i], final_ny_L[,i]))
  }
  
  write.csv(sna_file, "Snacking.csv")
  
  print("Done")
  setwd(paste(home, "/", school, sep = ""))
  toc()

}

