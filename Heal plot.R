plot_heal <- function(home, school) {
  
  setwd(paste(home, "/", school, sep = ""))
  
  stores <- read.csv("All Healthy.csv")
  stores <- stores[,-1]
  
  names(stores) <- c("Intervention", "Scenario", "Health", "Proportion", "Upper", "Lower")
  stores$Intervention <- as.character(stores$Intervention)
  block <- stores
  
  # block_1 <- subset(stores, Intervention == "0"| Intervention == "1"| Intervention == "2"|Intervention == "3"|Intervention == "4")
  # block_2 <-  subset(stores, Intervention == "0"| Intervention == "5"| Intervention == "6"|Intervention == "7"|Intervention == "8")
  # block_3 <-  subset(stores, Intervention == "0"| Intervention == "9"| Intervention == "10"|Intervention == "11"|Intervention == "12")
  # block_4 <-  subset(stores, Intervention == "21"| Intervention == "13"| Intervention == "14"|Intervention == "15"|Intervention == "16")
  # block_5 <-  subset(stores, Intervention == "21"| Intervention == "17"| Intervention == "18"|Intervention == "19"|Intervention == "20")
  # 
  # block_6 <-  subset(stores, Intervention == "0"| Intervention == "4"| Intervention == "10")
  # block_7 <-  subset(stores, Intervention == "21"| Intervention == "14"| Intervention == "20")
  # 
  # blocks <- list(block_1, block_2, block_3, block_4, block_5, block_6, block_7)
  
  #for (i in 1:7) {
    
    #block <- blocks[[i]]
    
    cols <- c("limegreen", "brown1")
    lims <- aes(ymax = block$Upper, ymin =  block$Lower)
    plot_block <-  ggplot(block, aes(x = Scenario, y = Proportion, fill = Health)) + 
      geom_bar(stat = "identity", position = "stack") + 
      ggtitle("New York City Health/Unhealthy Consumption") +
      ylim(0, 1.1) +
      theme_solarized() + 
      theme(title = element_text(color = "gray0"), panel.background = element_rect(fill = 'snow'), plot.background = element_rect("white"), 
            legend.background = element_rect("white"), legend.text = element_text(color = "gray0"), 
            legend.title = element_text(color = "gray0")) +
      theme(panel.background = element_rect(fill = 'snow'), plot.background = element_rect("white")) +
      theme(plot.title = element_text(hjust = 0.5)) + coord_flip() +
      xlab("Intervention") + ylab("Proportion Healthy") +
      scale_fill_manual(name = "Health",
                        labels = c("Healthy", "Unhealthy"), 
                        values = cols)
    
    jpeg("Healthy.jpg", width = 850, height = 400)
    plot(plot_block)
    dev.off()
  #}
  
}
  