plot_sna <- function(home, school) {
  
  setwd(paste(home, "/", school, sep = ""))
  
  bmi_trend <- read.csv("All Snacking.csv")
  bmi_trend <- bmi_trend[,-1]
  bmi_trend[,1] <- as.character(bmi_trend[,1])
  
  names(bmi_trend) <- c("Intervention", "Scenario", "Time", "Calories", "Upper", "Lower")
  block <- bmi_trend
  
  #function to plot BMI percentiles.
  lims <- aes(ymax = block$Upper, ymin =  block$Lower)
  plot_block <-  ggplot(block, aes(x = Time, y = Calories, color = Scenario)) + 
    geom_line(size = 1) + 
    #scale_size_manual(values = c(0.1, 0.1, 0.1, 1)) + 
    #geom_point() +
    scale_color_manual(values = c("chartreuse", "blue", "gray0", "red", "purple", "darkgreen")) +
    ggtitle("New York City School.\nSnacking Calories") + 
    #ylim(0.25, 1) + theme_gdocs() + 
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Time (months)") + ylab("Snack Size (calories)") + #labs(color = "Scenario")# +
    theme_solarized() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "right") +
    theme(title = element_text(color = "gray0"), panel.background = element_rect(fill = 'snow'), plot.background = element_rect("white"), 
          legend.background = element_rect("white"), legend.text = element_text(color = "gray0"), 
          legend.title = element_text(color = "gray0")) 
    #geom_errorbar(lims, width = 01, position = "dodge")
  
  jpeg("Snacking.jpg", width = 650)
  plot(plot_block)
  dev.off()
  
}