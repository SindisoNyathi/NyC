#Run all the NY files. 
ny <- function(school, no_i) {
  
  #For each file read in the file.
  require(Rmisc)
  require(data.table)
  require(tictoc)
  require(stringr)
  require(magrittr)
  require(purrr)
  library(reshape2)
  library(ggplot2)
  library(magrittr)
  library(ggthemes)
  require(gmodels)
  library(purrr)
  require(RColorBrewer)
  
  home <- "C:/Users/Sindiso Nyathi/Desktop/Systems Modeling Data Files/New York/Interventions 1_5"
  setwd(paste(home, "/", school, sep = ""))
  
  #Source
  source(paste(home, "/NY_1stStep.R", sep = ""))
  source(paste(home, "/NY_2ndStep.R", sep = ""))
  source(paste(home, "/Split_BMI.R", sep = ""))
  source(paste(home, "/Corr_Type.R", sep = ""))
  source(paste(home, "/Dist plot.R", sep = ""))
  source(paste(home, "/Heal plot.R", sep = ""))
  source(paste(home, "/StoreTypes.R", sep = ""))
  source(paste(home, "/Perc Plot.R", sep = ""))
  source(paste(home, "/BMI plot.R", sep = ""))
  source(paste(home, "/Perc Plot2.R", sep = ""))
  source(paste(home, "/BMI plot2.R", sep = ""))
  source(paste(home, "/DealSD.R", sep = ""))
  source(paste(home, "/Sna Plot.R", sep = ""))
  
  these <- c(1, 3, 5, 10)
  
  #Use a for loop to just write everything.
  # for (i in 1:no_i) {
  # 
  #   no = these[i]
  #   ny_folder = as.character(paste("Intervention", no, sep = ""))
  # 
  #   #Run the NY file in question.
  #   ny_master(home, school, ny_folder)
  # 
  #   #Print the file is done.
  #   print(paste("NY_folder ", ny_folder, " is completed", sep = ""))
  #   }
  
  ny_format(home, school, no_i)
  
  #Print that all the runs are done.
  print("All the folders are done")

}