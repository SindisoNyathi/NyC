plot_bmi2 <- function(home, school) {
  
  setwd(paste(home, "/", school, sep = ""))
  bmi_trend <- read.csv("All BMI.csv")
  bmi_trend <- bmi_trend[,-1]
  bmi_trend[,1] <- as.character(bmi_trend[,1])
  
  #reshape into wide format
  bmi_trend <- bmi_trend[,c(1,2,3,4)]
  bmi_trend <- reshape(bmi_trend, idvar = c("Intervention", "Scenario"), timevar = "Timepoint", direction = "wide")
  bmi_trend <- bmi_trend[,c(1:44)]
  colnames(bmi_trend)[c(3:44)] <- c(1:42)
  bmi_trend[2, c(3:44)] <-  bmi_trend[2, c(3:44)] - bmi_trend[1, c(3:44)]
  bmi_trend[3, c(3:44)] <-  bmi_trend[3, c(3:44)] - bmi_trend[1, c(3:44)]
  bmi_trend[4, c(3:44)] <-  bmi_trend[4, c(3:44)] - bmi_trend[1, c(3:44)]
  bmi_trend[1, c(3:44)] <-  bmi_trend[1, c(3:44)] - bmi_trend[1, c(3:44)]
  bmi_trend <- melt(bmi_trend, idvar = c("Intervention", "Scenario"))
  
  names(bmi_trend) <- c("Intervention", "Scenario", "Time", "BMI")
  bmi_trend$Time <- as.numeric(bmi_trend$Time)
  block <- bmi_trend
  
  #function to plot BMI percentiles.
    plot_block <-  ggplot(block, aes(x = ((Time*25)/30), y = BMI, color = Scenario)) + 
      geom_line(size = 1) +
      theme_solarized() + 
      theme(title = element_text(color = "gray0"), panel.background = element_rect(fill = 'snow'), plot.background = element_rect("white"), 
            legend.background = element_rect("white"), legend.text = element_text(color = "gray0"), 
            legend.title = element_text(color = "gray0")) +
      #scale_size_manual(values = c(0.1, 0.1, 0.1, 1)) + 
      #geom_point() +
      ggtitle("New York City School.\nBMI Difference") + 
      ylim(-2.5, 0) + #theme_gdocs() + 
      theme(plot.title = element_text(hjust = 0.5), legend.position = "right") +
      scale_color_manual(values = c("chartreuse", "blue", "gray0", "red")) +
      scale_x_continuous(breaks = seq(0, 36, 6)) +
      xlab("Time (months)") + ylab("BMI Change (kg/m^2)") # labs(color = "Scenario")# +
      #geom_errorbar(lims, width = 1, position = "dodge")
    
    jpeg("BMI Trends.jpg", width = 550)
    plot(plot_block)
    dev.off()
    
}

