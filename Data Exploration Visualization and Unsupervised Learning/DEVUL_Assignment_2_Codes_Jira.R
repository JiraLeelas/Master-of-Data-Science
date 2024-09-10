## DEVUL - Assignment 2
## library Imports
library(ggplot2)
library(ggpubr)
library(visdat)
library(scales)
library(psych)


## 0. Data Loading and Pre-processing
MaunaLoa <- read.table("C://Users//Jira//OneDrive - Durham University//MDS - Master of Data Science//2023 DEVAL//DEVUL - Quiz and Assignments//DEVUL - Assignment 2//MaunaLoa.csv", 
                       sep = ",", header = 1)

MaunaLoa$Date <- as.Date(MaunaLoa$Date, format = "%Y-%m-%d") # Format as a data format

## Fill in the skipped months
every_month_first_day <- data.frame(Date = seq(as.Date("2000-01-01"), # Min
                                               as.Date("2019-12-01"), # Max
                                               by = "month")) # Create
MaunaLoa_NA_Gaps <- merge(every_month_first_day, 
                            MaunaLoa, 
                            by = "Date", 
                            all.x = TRUE)
MaunaLoa_NA_Gaps$Date <- as.Date(MaunaLoa_NA_Gaps$Date, format = "%Y-%m-%d")

## Add 132:135 from original dataset
strange_records <- MaunaLoa[132:135,] # In the original dataset
strange_records$Date <- as.Date(strange_records$Date, format = "%Y-%m-%d")
strange_records # Strange records
MaunaLoa_NA_Gaps <- rbind(MaunaLoa_NA_Gaps, strange_records) # bind them
MaunaLoa_NA_Gaps <- MaunaLoa_NA_Gaps[order(MaunaLoa_NA_Gaps$Date), ] # Sort by values
rownames(MaunaLoa_NA_Gaps) <- NULL # Reset index start from 0

## 1. Exploratory Data Analysis (15 %)

## Visualize the Missing Values
missing_val_plot <- vis_miss(MaunaLoa_NA_Gaps)
missing_val_plot


## Standardize each Gases Showing relative Change
MaunaLoa_NA_Gaps$CO_rel <- 100 * (MaunaLoa_NA_Gaps$CO/MaunaLoa_NA_Gaps$CO[1]-1)
MaunaLoa_NA_Gaps$CO2_rel <- 100 * (MaunaLoa_NA_Gaps$CO2/MaunaLoa_NA_Gaps$CO2[1]-1)
MaunaLoa_NA_Gaps$Methane_rel <- 100 * (MaunaLoa_NA_Gaps$Methane/MaunaLoa_NA_Gaps$Methane[1]-1)
MaunaLoa_NA_Gaps$NitrousOx_rel <- 100 * (MaunaLoa_NA_Gaps$NitrousOx/MaunaLoa_NA_Gaps$NitrousOx[1]-1)
MaunaLoa_NA_Gaps$CFC11_rel <- 100 * (MaunaLoa_NA_Gaps$CFC11/MaunaLoa_NA_Gaps$CFC11[1]-1)

## Plot Standardize Series - Showing Relative Change
ggplot(data = MaunaLoa_NA_Gaps, aes(x = Date)) + 
  geom_line(aes(y = CO_rel, color = "CO"), linewidth = 1) +
  geom_line(aes(y = CO2_rel, color = "CO2"), linewidth= 1) +
  geom_line(aes(y = Methane_rel, color = "Methane"), linewidth = 1) +
  geom_line(aes(y = NitrousOx_rel, color = "NitrousOx"), linewidth = 1) +
  geom_line(aes(y = CFC11_rel, color = "CFC11"), linewidth = 1) +
  scale_color_manual(values = c("CO" = "#8c8c8c", "CO2" = "#00aedb", "Methane" = "#00b159", 
                                "NitrousOx" = "#d11141", "CFC11" = "#f37735")) +
  labs(title = "Relative Change in Atmospheric Gas Concentration at Mauna Loa Observatory",
       subtitle = "Selected Monthly Average Between 2000 and 2019",
       x = "Years",
       y = "Relative Change in Gas Concentration") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 year") 

## Visualize the time series as a year - After Removal of Strange Values
MaunaLoa_Remove_Strange <- MaunaLoa_NA_Gaps[-c(157:161),]
ggplot(data = MaunaLoa_Remove_Strange, aes(x = Date)) + 
  geom_line(aes(y = CO_rel, color = "CO"), linewidth = 1) +
  geom_line(aes(y = CO2_rel, color = "CO2"), linewidth= 1) +
  geom_line(aes(y = Methane_rel, color = "Methane"), linewidth = 1) +
  geom_line(aes(y = NitrousOx_rel, color = "NitrousOx"), linewidth = 1) +
  geom_line(aes(y = CFC11_rel, color = "CFC11"), linewidth = 1) +
  scale_color_manual(values = c("CO" = "#8c8c8c", "CO2" = "#00aedb", "Methane" = "#00b159", 
                                "NitrousOx" = "#d11141", "CFC11" = "#f37735")) +
  labs(title = "Relative Change in Atmospheric Gas Concentration at Mauna Loa Observatory",
       subtitle = "Selected Monthly Average Between 2000 and 2019",
       x = "Years",
       y = "Relative Change in Gas Concentration") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 year") 


## Time Series One by One - Adding NA Gaps between skipped months
## CO
mis_CO <- ggplot(data = MaunaLoa_NA_Gaps, aes(x = Date, y = CO)) + 
  geom_line(color = "#8c8c8c")+
  labs(title = "Carbon Monoxide Gas Concentration",
       x = "Years",
       y = "Gas Concentration (ppb)") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 year")

## CO2
mis_CO2 <- ggplot(data = MaunaLoa_NA_Gaps, aes(x = Date, y = CO2)) + 
  geom_line(color = "#00aedb") + 
  labs(title = "Carbon Dioxide Gas Concentration",
       x = "Years",
       y = "Gas Concentration (ppm)") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 year")

## Methane
mis_Met <- ggplot(data = MaunaLoa_NA_Gaps, aes(x = Date, y = Methane)) + 
  geom_line(color = "#00b159") + 
  labs(title = "Methane Gas Concentration",
       x = "Years",
       y = "Gas Concentration (ppb)") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 year")

## NitrousOx
mis_NOx <- ggplot(data = MaunaLoa_NA_Gaps, aes(x = Date, y = NitrousOx)) + 
  geom_line(color = "#d11141") + 
  labs(title = "Nitrous Oxide Gas Concentration",
       x = "Years",
       y = "Gas Concentration (ppb)") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 year")

## CFC11
mis_cfc <- ggplot(data = MaunaLoa_NA_Gaps, aes(x = Date, y = CFC11)) + 
  geom_line(color = "#f37735") + 
  labs(title = "CFC-11 Gas Concentration",
       x = "Years",
       y = "Gas Concentration (ppt)") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 year")



## Plot them all
missing_gases <- ggarrange(mis_CO2, mis_CO, mis_Met, 
                           mis_NOx, mis_cfc,
                             labels = c("A", "B", "C", "D", "E"),
                             ncol = 3, nrow = 2)
missing_gases

## Plot Boxplot
ggplot(data = MaunaLoa) + 
  geom_boxplot(mapping = aes(x = "CO", y = CO, fill = "CO")) +
  geom_boxplot(mapping = aes(x = "CO2", y = CO2, fill = "CO2")) +
  geom_boxplot(mapping = aes(x = "Methane", y = Methane, fill = "Methane")) +
  geom_boxplot(mapping = aes(x = "NitrousOx", y = NitrousOx, fill = "NitrousOx")) +
  geom_boxplot(mapping = aes(x = "CFC11", y = CFC11, fill = "CFC11"))+
  scale_fill_manual(values = c("CO" = "#8c8c8c", "CO2" = "#00aedb", "Methane" = "#00b159", 
                                "NitrousOx" = "#d11141", "CFC11" = "#f37735"))+
  labs(title = "Atmospheric Gas Concentration at Mauna Loa Observatory Boxplot",
       x = "Gases",
       y = "Gas Concentration") + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

## Get the outlines function
get_q_outliers <- function(data, variable){
  ## Outliers = Q1 - 1.5*IQR or Q3 + 1.5*IQR
  outlier_quantile <- boxplot.stats(data[[variable]])$out
  outlier_idx <- which(data[[variable]] %in% outlier_quantile) # Get indexes
  return(outlier_idx)
}

gases_names <- names(MaunaLoa)[-1] # get the gases names
print("Potential Outliers")
for (gas in gases_names){ # loop and get outliers
  gas_outliers <- c(get_q_outliers(MaunaLoa, gas))
  cat(gas, gas_outliers, "\n") # Print out outliers
}

## Create new dataset by removing these outliers
MaunaLoa_remove_outliers <- MaunaLoa[-c(131:135),]

## Plotting Pairs after the removal of outliers
pairs.panels(MaunaLoa_remove_outliers[,-1],
             method = "pearson", 
             hist.col = "#00AFBB",
             density = TRUE,  
             ellipses = TRUE,
             main = "Pairs Plot of MaunaLoa Gases"
)


## 2. Dimension Reduction (30 %) - Factor Analysis
par(mfrow=c(1,1))

## 2.1 Using the data without the "Date" columns and Scale the Data
gases_no_outliners <- MaunaLoa_remove_outliers[,-1] # Only select the gases columns
gases.scaled <- scale(gases_no_outliners) # Scale the gases

## 2.2 Using PCA to determine the numbers of Factors
gases.pr <- prcomp(gases.scaled) # data already scaled

summary(gases.pr) # we select 2 Factors
gases.pr

fviz_screeplot(gases.pr, 
               addlabels = TRUE)

plot(summary(gases.pr)$importance[2,], 
     type = "b", 
     xlab ="PCs", 
     ylab = "Variability explained",
     main = "Gases Scree Plot") # Visualization

## We select 2 factors

## 2.3 Select Rotation Type
gases.fa.none <- factanal(gases.scaled, 
                          factors = 2, 
                          rotation = "none",
                          scores="regression")
gases.fa.varimax <- factanal(gases.scaled, 
                            factors = 2, 
                            rotation = "varimax",
                            scores="regression")
gases.fa.promax <- factanal(gases.scaled, 
                           factors = 2, 
                           rotation = "promax",
                           scores="regression")

## Visualize different Rotation Types
par(mfrow = c(1, 3))
plot(gases.fa.none$loadings[, 1],
     gases.fa.none$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1, 1),
     xlim = c(-1, 1),
     col= "blue",
     main = "No rotation")
abline(h = 0, v = 0)

plot(gases.fa.varimax$loadings[, 1],
     gases.fa.varimax$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1, 1),
     xlim = c(-1, 1),
     col= "blue",
     main = "Varimax rotation")
abline(h = 0, v = 0)

plot(gases.fa.promax$loadings[, 1],
     gases.fa.promax$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1, 1),
     xlim = c(-1, 1),
     col= "blue",
     main = "Promax rotation")
abline(h = 0, v = 0)
par(mfrow = c(1, 1))

## We select "Promax" rotation
gases.fa.promax 

## 2.4 Evaluation
gases.fa.promax$uniquenesses # Uniqueness are unexplained variability proportion by the factors.
# High uniqueness for Methane -> the FA model will be less accounted on this variable

gases.fa.promax$loadings # Showing contributions of variables into these two factors

## Variance Explained
L <- gases.fa.promax$loadings
SS <- apply(L^2, 2, sum)
barplot(SS, col='steelblue' , main="Importance of Factors")
lines(x = c(.75,2), SS, type="b", pch=19, col = "red")

## Visualize the loadings 
autoplot(gases.fa.promax, 
         data = MaunaLoa_remove_outliers,
         loadings = TRUE, 
         loadings.label = TRUE, 
         loadings.label.size  = 3.5,
         color = "CO",
         main = "Projection of Variables on Factor 1 and 2")


## Pairs of the two factors
gases.fa <- as.data.frame(gases.fa.promax$scores)
pairs.panels(gases.fa,
             method = "pearson", 
             hist.col = "#00AFBB",
             density = TRUE,
             main = "Pairs Plot of MaunaLoa Factors"
)



## 3. Cluster Analysis (30 %) - kmeans and cluster

## Load Essential Libraries
library(factoextra)
library(cluster)
library(tidyverse)

## 3.1 Scaled the data
MaunaLoa_Scaled <- scale(MaunaLoa_remove_outliers[-1]) # Select only variables

## 3.2 k-Mean Clustering
## 3.2.1 Select the best K
fviz_nbclust(MaunaLoa_Scaled, 
             kmeans, 
             method = "wss") # Elbow pattern at 3 clusters
fviz_nbclust(MaunaLoa_Scaled, 
             kmeans, 
             method = "silhouette")+ # Avg Silhouette width recommend at 2 clusters
  labs(title = "K-means")

## 3.2.2 Best k k-means
set.seed(123) # reproducibility
kmean_output <- kmeans(MaunaLoa_Scaled, 
                       centers=3, # we select 3 clusters as Avg Sihouette width is similar for 2 and 3 clusters
                       nstart=25)

fviz_cluster(kmean_output, MaunaLoa_Scaled, ellipse.type = "norm")+
  labs(title = "K-means Cluster Plot") # Plot the clusters

## 3.2.3 Best k-Mean Silhouette
kmean_sil <- silhouette(kmean_output$cluster, dist(MaunaLoa_Scaled))

fviz_silhouette(kmean_sil)

## 3.3 Hierarchical Clustering

## 3.3.1 Determine the linkage type
link_methods <- c("average","single","complete")
names(link_methods) <- c("average","single","complete")

ac <- function(x){
  agnes(MaunaLoa_Scaled, method = x)$ac
}

map_dbl(link_methods,ac) # call the function

## Complete linkage is the best

## 3.3.2 Determine the number of clusters
fviz_nbclust(MaunaLoa_Scaled, 
             hcut, 
             method = "wss")

fviz_nbclust(MaunaLoa_Scaled, 
             hcut, 
             method = "silhouette")

## 3.3.3 Create hcut
hcut_output <- hcut(MaunaLoa_Scaled, k = 2, hc_method = "complete")

fviz_cluster(hcut_output, ellipse.type = "norm")

fviz_dend(hcut_output, 
          cex = 0.5, 
          color_labels_by_k = TRUE,
          ggtheme = theme_gray(),
          main = "Hierarchical Cluster Dendrogram"
)

## 3.3.4 Best k-cluster Silhouette
fviz_silhouette(hcut_output)

## Discussion & Conclusion (15 %)

