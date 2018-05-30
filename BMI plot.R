plot_bmi <- function(home, school) {
  
  setwd(paste(home, "/", school, sep = ""))
  bmi_trend <- read.csv("All BMI.csv")
  bmi_trend <- bmi_trend[,-1]
  bmi_trend[,1] <- as.character(bmi_trend[,1])
  
  names(bmi_trend) <- c("Intervention", "Scenario", "Time", "BMI", "Upper", "Lower")
  block <- bmi_trend
  
  #function to plot BMI percentiles.
    lims <- aes(ymax = block$Upper, ymin =  block$Lower)
    plot_block <-  ggplot(block, aes(x = ((Time*25)/30), y = BMI, color = Scenario)) + 
      geom_line(size = 1) +
      theme_solarized() + 
      theme(title = element_text(color = "gray0"), panel.background = element_rect(fill = 'snow'), plot.background = element_rect("white"), 
            legend.background = element_rect("white"), legend.text = element_text(color = "gray0"), 
            legend.title = element_text(color = "gray0")) +
      #scale_size_manual(values = c(0.1, 0.1, 0.1, 1)) + 
      #geom_point() +
      ggtitle("New York City School.\nBMI Trajectory") + 
      ylim(16.5, 20.5) + #theme_gdocs() + 
      theme(plot.title = element_text(hjust = 0.5), legend.position = "right") +
      scale_color_manual(values = c("chartreuse", "blue", "gray0", "red")) +
      scale_x_continuous(breaks = seq(0, 36, 6)) +
      xlab("Time (months)") + ylab("BMI (kg/m^2)") # labs(color = "Scenario")# +
      #geom_errorbar(lims, width = 1, position = "dodge")
    
    jpeg("BMI Trends.jpg", width = 550)
    plot(plot_block)
    dev.off()
    
}

