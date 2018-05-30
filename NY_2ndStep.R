#****************************************************************************************************************************#
#SN
#Latest Update: 11/4/2018
#Goal: Step 2 of NY results process 
#Input: 
#Method:
#***************************************************************************************************************************#
ny_format <- function(home, school, no_i) {
  
  setwd(paste(home, "/", school, sep = ""))
   
  #create each of the files anew, file size = 60 by 6.
  a_Dist <- data.frame("Intervention" = numeric(3*no_i), "Scenario" = numeric(3*no_i), 
                        "Distance" = numeric(3*no_i), "Value" = numeric(3*no_i), 
                        "Upper" = numeric(3*no_i), "Lower" = numeric(3*no_i))
   
   a_Stores <- data.frame("Intervention" = numeric(5*no_i), "Scenario" = numeric(5*no_i), 
                        "Store" = numeric(5*no_i), "Value" = numeric(5*no_i), 
                        "Upper" = numeric(5*no_i), "Lower" = numeric(5*no_i))
   
   a_Health <- data.frame("Intervention" = numeric(2*no_i), "Scenario" = numeric(2*no_i), 
                          "Store" = numeric(2*no_i), "Value" = numeric(2*no_i), 
                          "Upper" = numeric(2*no_i), "Lower" = numeric(2*no_i))
  
  #Add lines for BMI and BMI Percentile.
  a_bmi <- data.frame("Intervention" = numeric(44*no_i), "Scenario" = numeric(44*no_i), 
                      "Timepoint" = numeric(44*no_i), "Value" = numeric(44*no_i), 
                      "Upper" = numeric(44*no_i), "Lower" = numeric(44*no_i))
  
  a_per <- data.frame("Intervention" = numeric(44*no_i), "Scenario" = numeric(44*no_i), 
                      "Timepoint" = numeric(44*no_i), "Value" = numeric(44*no_i), 
                      "Upper" = numeric(44*no_i), "Lower" = numeric(44*no_i))
  
  a_sna <- data.frame("Intervention" = numeric(44*no_i), "Scenario" = numeric(44*no_i), 
                      "Timepoint" = numeric(44*no_i), "Value" = numeric(44*no_i), 
                      "Upper" = numeric(44*no_i), "Lower" = numeric(44*no_i))
  
  
  #Name the interventions
  scenarios <- read.csv(paste(home, "/Scenarios.csv", sep = ""))
  
  these <- c(1, 3, 5, 10)
  
  for (i in 1:no_i) {
    
    no = these[i]
    
    ny_folder = as.character(paste("Intervention", no, sep = ""))
    
    setwd(paste(home, "/", school, "/", ny_folder, sep = ""))
    
    #Read each of files and format them the way they should be formatted.
    dist_file <- read.csv("Distances.csv")
    dist_file <- dist_file[,-1]

    #Assuming that each of the files have 103 lines and that the line we want is line
    #101.
    conI <- dealsd(dist_file[c(-101, -102, -103),])
    
    #
    dist_one <- conI[c(1:3)]
    dist_two <- conI[c(4:6)]
    dist_tre <- conI[c(7:9)]

    #Set columns numbers.
    row1 <- ((i-1)*3) + 1
    row2 <- ((i-1)*3) + 2
    row3 <- ((i-1)*3) + 3

    #Fill in the dist file.
    a_Dist[row1,c(4, 5, 6)] <- dist_one
    a_Dist[row2,c(4, 5, 6)] <- dist_two
    a_Dist[row3,c(4, 5, 6)] <- dist_tre

    #Other Columns
    a_Dist[c(row1, row2, row3), 1] <- no
    a_Dist[c(row1, row2, row3), 2] <- as.character(scenarios[no,1])
    a_Dist[row1, 3] <- "0.1km"
    a_Dist[row2, 3] <- "0.274km"
    a_Dist[row3, 3] <- "0.804km"

    ##*******************************************************************************************##
    #Repeat the above for stores abnnd healthy.

    #Stores.
    #Read each of files and format them the way they should be formatted.
    stor_file <- read.csv("Stores.csv")
    stor_file <- stor_file[,-1]

    #Assuming that each of the files have 103 lines and that the line we want is line
    #101.
    conI <- dealsd(stor_file[c(-101,-102,-103),])


    #
    stor_one <- conI[c(1:3)]
    stor_two <- conI[c(4:6)]
    stor_tre <- conI[c(7:9)]
    stor_fou <- conI[c(10:12)]
    stor_fiv <- conI[c(13:15)]

    #Set columns numbers.
    b_row1 <- ((i-1)*5) + 1
    b_row2 <- ((i-1)*5) + 2
    b_row3 <- ((i-1)*5) + 3
    b_row4 <- ((i-1)*5) + 4
    b_row5 <- ((i-1)*5) + 5

    #Fill in the dist file.
    a_Stores[b_row1,c(4, 5, 6)] <- stor_one
    a_Stores[b_row2,c(4, 5, 6)] <- stor_two
    a_Stores[b_row3,c(4, 5, 6)] <- stor_tre
    a_Stores[b_row4,c(4, 5, 6)] <- stor_fou
    a_Stores[b_row5,c(4, 5, 6)] <- stor_fiv

    #Other Columns
    a_Stores[c(b_row1, b_row2, b_row3, b_row4, b_row5), 1] <- no
    a_Stores[c(b_row1, b_row2, b_row3, b_row4, b_row5), 2] <- as.character(scenarios[no,1])
    a_Stores[b_row1, 3] <- "Bodegas"
    a_Stores[b_row2, 3] <- "Drug Stores"
    a_Stores[b_row3, 3] <- "Fast Food"
    a_Stores[b_row4, 3] <- "Vending Machines"
    a_Stores[b_row5, 3] <- "Supermarkets"


    ##*******************************************************************************************##

    #Healthy
    #Read each of files and format them the way they should be formatted.
    heal_file <- read.csv("Healthy.csv")
    heal_file <- heal_file[,-1]

    #Assuming that each of the files have 103 lines and that the line we want is line
    #101.
    conI <- dealsd(heal_file[c(-101, -102, -103),])
    
    #
    heal_one <- conI[c(1:3)]
    heal_two <- conI[c(4:6)]

    #Set columns numbers.
    c_row1 <- ((i-1)*2) + 1
    c_row2 <- ((i-1)*2) + 2


    #Fill in the dist file.
    a_Health[c_row1,c(4, 5, 6)] <- heal_one
    a_Health[c_row2,c(4, 5, 6)] <- heal_two

    #Other Columns
    a_Health[c(c_row1, c_row2), 1] <- no
    a_Health[c(c_row1, c_row2), 2] <- as.character(scenarios[no,1])
    a_Health[c_row1, 3] <- "Healthy"
    a_Health[c_row2, 3] <- "Unhealthy"
    
    
    #Read each of files and format them the way they should be formatted.
    bmi_file <- read.csv("BMI.csv")
    bmi_file <- bmi_file[,-1]
    
    #Assuming that each of the files have 103 lines and that the line we want is line
    #101.
    conI <- dealsd(bmi_file[c(-101, -102, -103),])
    #bmi_line <- bmi_file[101,]
    
    
    for (j in 1:44) {
      
      d_row <- ((i - 1)*44) + j
      a_bmi[d_row, c(4:6)] <- conI[c(3*(j-1)+1, 3*(j-1)+2, 3*(j-1)+3)] 
      a_bmi[d_row, 1] <- no
      a_bmi[d_row, 2] <- as.character(scenarios[(no), 1])
      a_bmi[d_row, 3] <- as.character(j)
    }
    
    ##*******************************************************************************************##
    
    #BMI PErcentile
    #BMI
    #Read each of files and format them the way they should be formatted.
    per_file <- read.csv("Percentile.csv")
    per_file <- per_file[,-1]
    
    #Assuming that each of the files have 103 lines and that the line we want is line
    #101.
    conI <- dealsd(per_file[c(-101, -102, -103),])
    #per_line <- per_file[101,]
    
    
    for (j in 1:44) {
      
      e_row <- ((i - 1)*44) + j
      a_per[e_row, c(4:6)] <- conI[c(3*(j-1)+1, 3*(j-1)+2, 3*(j-1)+3)] 
      a_per[e_row, 1] <- no
      a_per[e_row, 2] <- as.character(scenarios[(no), 1])
      a_per[e_row, 3] <- as.character(j)
    }
    
    #Repeat the above for stores abnnd healthy.
    sna_file <- read.csv("Snacking.csv")
    sna_file <- sna_file[,-1]
    
    #Assuming that each of the files have 103 lines and that the line we want is line
    #101.
    conI <- dealsd(sna_file[c(-101, -102, -103),])
    #per_line <- per_file[101,]
    
    
    for (j in 1:44) {
      
      f_row <- ((i - 1)*44) + j
      a_sna[f_row, c(4:6)] <- conI[c(3*(j-1)+1, 3*(j-1)+2, 3*(j-1)+3)] 
      a_sna[f_row, 1] <- no
      a_sna[f_row, 2] <- as.character(scenarios[(no), 1])
      a_sna[f_row, 3] <- as.character(j)
    }
  } 
  
  setwd(paste(home, "/", school, sep = ""))
  
  #Write all the files.
  write.csv(a_Dist, "All Distances.csv")
  write.csv(a_Stores, "All Stores.csv")
  write.csv(a_Health, "All Healthy.csv")
  
  write.csv(a_bmi, "All BMI.csv")
  write.csv(a_per, "All Percentile.csv")
  
  write.csv(a_sna, "All Snacking.csv")
  
  #Call the graphing files.
  plot_dist(home, school)
  plot_heal(home, school)
  plot_stores(home, school)
  plot_bmi2(home, school)
  plot_perc2(home, school)
  plot_sna(home, school)
  
  #Fin.
  
}