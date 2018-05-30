plot_perc <- function(home, school) {
  
  setwd(paste(home, "/", school, sep = ""))
  
  bmi_trend <- read.csv("All Percentile.csv")
  bmi_trend <- bmi_trend[,-1]
  bmi_trend[,1] <- as.character(bmi_trend[,1])
  
  names(bmi_trend) <- c("Intervention", "Scenario", "Time", "BMI", "Upper", "Lower")
  block <- bmi_trend
  
  #function to plot BMI percentiles.
  lims <- aes(ymax = block$Upper, ymin =  block$Lower)
  plot_block <-  ggplot(block, aes(x = ((Time*25)/30), y = BMI, color = Scenario)) + 
    geom_line(size = 1) + 
    #scale_size_manual(values = c(0.1, 0.1, 0.1, 1)) + 
    #geom_point() +
    ggtitle("New York City School.\nBMI Percentile Trajectory") + 
    #ylim(0.25, 1) + theme_gdocs() + 
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    scale_color_manual(values = c("chartreuse", "blue", "gray0", "red", "purple", "darkgreen")) +
    theme_solarized() +
    theme(title = element_text(color = "gray0"), panel.background = element_rect(fill = 'snow'), plot.background = element_rect("white"), 
          legend.background = element_rect("white"), legend.text = element_text(color = "gray0"), 
          legend.title = element_text(color = "gray0")) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "right") +
    xlab("Time (months)") + ylab("BMI (%ile)")# + #labs(color = "Scenario")# +
    #geom_errorbar(lims, width = 01, position = "dodge")
  
  jpeg("Percentile.jpg", width = 650)
  plot(plot_block)
  dev.off()
  
}
