library(ggpubr)
library(reshape)
library(ggthemes)
library(ggplot2)

#Single Intervention.
setwd("C:/Users/Sindiso Nyathi/Desktop/Systems Modeling Data Files/New York/Interventions 17_4")

library(reshape)
run0 <- read.csv("ESMS/Intervention1/Baseline_23.csv")
runI <- read.csv("ESMS/Intervention10/Int1_23.csv")

#For the ones who were in the model for 3 years from the beggining.
run0["InSim"] <- (as.numeric(as.character(run0$death_date)) - as.numeric(as.character(run0$birth_date))) + 1
runI["InSim"] <- (as.numeric(as.character(runI$death_date)) - as.numeric(as.character(runI$birth_date))) + 1


#In Sim for 3 years.
run0 <- run0[run0$InSim == 3,]
runI <- runI[runI$InSim == 3,]

colnames(runI)

dist0 <- run0[,c(11, 2, 3, 4)]
distI <- runI[,c(11, 2, 3, 4)]

dist0["Scenario"] <- "Baseline"
distI["Scenario"] <- "Intervention"

names(distI) <- c("ID", "0.1km", "0.4km", "0.8km", "Scenario")
names(dist0) <- c("ID", "0.1km", "0.4km", "0.8km", "Scenario")

dist0 <- melt(dist0, id.vars = c("ID", "Scenario"))
distI <- melt(distI, id.vars = c("ID", "Scenario"))

#dist <- rbind(dist0, distI)

names(dist0) <- c("ID", "Scenario", "Distance", "Proportion")
names(distI) <- c("ID", "Scenario", "Distance", "Proportion")

#names(dist) <- c("ID", "Scenario", "Distance", "Proportion")

distI$Proportion <- as.numeric(distI$Proportion)
dist0$Proportion <- as.numeric(dist0$Proportion)


dist$Proportion <- as.numeric(dist$Proportion)

dist_plotI <- ggplot(distI, aes(x = Distance, y = Proportion)) + geom_violin(scale = "area", trim = FALSE, fill = "palegreen4") +
  geom_boxplot(width = 0.4, fill = "palegreen4") + geom_jitter(size = 0.1) + stat_boxplot(geom = "errorbar", width = 0.1) + 
  theme_solarized() + theme(panel.background = element_rect(fill = 'snow'), plot.background = element_rect("white")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1) +
  xlab("Distance") + ylab("Proportion Chosen") +
  ggtitle("All/0.5/50%. \n New York City, Single Run. Distances Chosen.")

dist_plot0 <- ggplot(dist0, aes(x = Distance, y = Proportion)) + geom_violin(scale = "area", trim = FALSE, fill = "palegreen4") +
  geom_boxplot(width = 0.4, fill = "palegreen4") + geom_jitter(size = 0.1) + stat_boxplot(geom = "errorbar", width = 0.1) + 
  theme_solarized() + theme(panel.background = element_rect(fill = 'snow'), plot.background = element_rect("white")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1) +
  xlab("Distance") + ylab("Proportion Chosen") +
  ggtitle("Baseline \n New York City, Single Run. Distances Chosen.")

ggarrange(dist_plot0, dist_plotI, nrow = 1, ncol = 2)
plot(dist_plotI)
#Stores.
colnames(runI)

store0 <- run0[,c(1, 6, 7, 8, 10, 12)]
storeI <- runI[,c(1, 6, 7, 8, 10, 12)]

dist0["Scenario"] <- "Old Distances"
distI["Scenario"] <- "New Distances"

names(store0) <- c("ID", "Bodegas", "Drug Stores", "Fast Food", "VM", "Supermarkets")
names(storeI) <- c("ID", "Bodegas", "Drug Stores", "Fast Food", "VM", "Supermarkets")

store0 <- melt(store0, id.vars = "ID")
storeI <- melt(storeI, id.vars = "ID")

names(store0) <- c("ID", "Store", "Proportion")
names(storeI) <- c("ID", "Store", "Proportion")

storeI$Proportion <- as.numeric(storeI$Proportion)
store0$Proportion <- as.numeric(store0$Proportion)

store_plot0 <- ggplot(store0, aes(x = Store, y = Proportion)) + geom_violin(scale = "area", trim = FALSE, fill = "palegreen") +
  geom_boxplot(width = 0.4, fill = "palegreen4") + 
  #geom_jitter(size = 0.1) +
  stat_boxplot(geom = "errorbar", width = 0.1) + 
  theme_solarized() + theme(panel.background = element_rect(fill = 'snow'), plot.background = element_rect("white")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1) +
  xlab("Store") + ylab("Proportion Chosen") +
  ggtitle("Baseline \n New York City, Single Run. Stores Chosen.")

store_plotI <- ggplot(storeI, aes(x = Store, y = Proportion)) + geom_violin(scale = "area", trim = FALSE, fill = "palegreen") +
  geom_boxplot(width = 0.4, fill = "palegreen4") + 
  #geom_jitter(size = 0.1) + 
  stat_boxplot(geom = "errorbar", width = 0.1) + 
  theme_solarized() + theme(panel.background = element_rect(fill = 'snow'), plot.background = element_rect("white")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1) +
  xlab("Store") + ylab("Proportion Chosen") +
  ggtitle("Intervention. \n New York City, Single Run. Stores Chosen.")

ggarrange(store_plot0, store_plotI, nrow = 1, ncol = 2)
plot(store_plotI)

#Healthy and Unhealth
#Stores.
colnames(run0)

heal0 <- run0[,c(1, 9, 13)]
healI <- runI[,c(1, 9, 13)]

#dist0["Scenario"] <- "Baseline"
#distI["Scenario"] <- "Intervention"

names(heal0) <- c("ID", "Healthy", "Unhealthy")
names(healI) <- c("ID", "Healthy", "Unhealthy")

heal0 <- melt(heal0, id.vars = "ID")
healI <- melt(healI, id.vars = "ID")

names(heal0) <- c("ID", "Store", "Proportion")
names(healI) <- c("ID", "Store", "Proportion")

heal_plot0 <- ggplot(heal0, aes(x = Store, y = Proportion)) + geom_violin(scale = "area", trim = FALSE, fill = "palegreen") +
  geom_boxplot(width = 0.4, fill = "palegreen4") + 
 # geom_jitter(size = 0.1) +
  stat_boxplot(geom = "errorbar", width = 0.1) + 
  theme_solarized() + theme(panel.background = element_rect(fill = 'snow'), plot.background = element_rect("white")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1) +
  xlab("Healthy/Unhealthy") + ylab("Proportion Chosen") +
  ggtitle("Baseline \n New York City, Single Run. Healthy Chosen.")

heal_plotI <- ggplot(healI, aes(x = Store, y = Proportion)) + geom_violin(scale = "area", trim = FALSE, fill = "palegreen") +
  geom_boxplot(width = 0.4, fill = "palegreen4") + 
  #geom_jitter(size = 0.1) + 
  stat_boxplot(geom = "errorbar", width = 0.1) + 
  theme_solarized() + theme(panel.background = element_rect(fill = 'snow'), plot.background = element_rect("white")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1) +
  xlab("Healthy/Unhealthy") + ylab("Proportion Chosen") +
  ggtitle("Intervention. \n New York City, Single Run. Healthy Chosen.")

ggarrange(heal_plot0, heal_plotI, nrow = 1, ncol = 2)
plot(heal_plotI)

mean(runI$Healthy)
mean(runI$Unhealthy)

