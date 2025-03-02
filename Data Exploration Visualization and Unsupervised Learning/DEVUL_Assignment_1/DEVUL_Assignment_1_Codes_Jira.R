## Assignment 1
head(plastic) # There are NAs

## Q1: The distributions of the two types of plastic waste.
# Histogram Plot - Make it same scale
par(mfrow=c(2,1))
pwaste_mean = mean(plastic$PWaste, na.rm = TRUE)
pwaste_med = median(plastic$PWaste, na.rm = TRUE)
hist(plastic$PWaste,
     breaks = seq(0,350,by=10),
     col = 2,
     main='Histogram of Plastic Waste Per Capita',
     xlab='Plastic Waste Per Capita (kg/day)')
abline(v=pwaste_mean, col='blue',lty='dashed',lwd=2) # mean line
text(x=pwaste_mean * 2.3, y = pwaste_mean*0.6,
     paste("Mean =", round(pwaste_mean,2), "kg/day"),
     col = 'blue',
     cex = 0.9)
abline(v=pwaste_med, col='brown',lty='dashed',lwd=2) # median line
text(x=pwaste_med * 3.1, y = pwaste_med*1.3,
     paste("Median =", round(pwaste_med,2), "kg/day"),
     col = 'brown',
     cex = 0.9)
rug(plastic$PWaste)


mpwaste_mean = mean(plastic$MPWaste, na.rm = TRUE)
mpwaste_med = median(plastic$MPWaste, na.rm = TRUE)
hist(plastic$MPWaste,
     breaks = seq(0,350,by=10),
     col = 3,
     main='Histogram of Mismanaged Plastic Waste Per Capita',
     xlab='Mismanaged Plastic Waste Per Capita (kg/day)')
abline(v=mpwaste_mean, col='blue',lty='dashed',lwd=2) # mean line
text(x=mpwaste_mean * 5.2, y = mpwaste_mean*1.7,
     paste("Mean =", round(mpwaste_mean,2), "kg/day"),
     col = 'blue',
     cex = 0.9)
abline(v=mpwaste_med, col='brown',lty='dashed',lwd=2) # median line
text(x=mpwaste_med * 7.7, y = mpwaste_med*5.5,
     paste("Median =", round(mpwaste_med,2), "kg/day"),
     col = 'brown',
     cex = 0.9)
rug(plastic$MPWaste)
par(mfrow=c(1,1))

# Boxplot - Only Select two data to compare
plastic_two <- subset(plastic, select = c('PWaste','MPWaste'))
boxplot(plastic_two,
        names = c('Plastic Waste', 'Mismanaged Plastic Waste'),
        col = c(2,3),
        main = 'Boxplot of Plastic Waste and Mismanaged Plastic Waste',
        xlab = 'Plastic Wastes',
        ylab = 'Plastic Waste Per Capita (kg/day)')

layout(matrix(c(1, 2, 3, 3), ncol = 2)) # Histogram and Boxplot

# layout()

# Outliers
# Look in to the points more than 200 kg/day
potenial_outliners <- subset(plastic, plastic$PWaste > 200)
# There are three points > 200 that could be potential outliners.

# unusual values ??? Not sure
# GDP is zero
summary(plastic)
# MPWaste should be lower than PWaste
# Function to look into it
unusual_values <- subset(plastic, plastic$MPWaste > plastic$PWaste)

library(ggplot2)
## Highlight the points with 1 missing values
one_missing <- plastic[!complete.cases(plastic$MPWaste),]
## Highlight the points with MPWaste > PWaste
not_match <- subset(plastic, MPWaste > PWaste)

library(ggplot2)
library(naniar)

# Creating a new dataset with missing values retained
missing_values <- subset(plastic, is.na(MPWaste))

## Plot missing values
ggplot(plastic, aes(x = PWaste, y = MPWaste)) + 
  geom_point() + 
  geom_miss_point() +
  geom_point(data = not_match, aes(color = 'Not Matched'), size = 2) +
  scale_color_manual(values = c('Not Missing' = "#4DB6D0", 'Missing' = "#D9717D", 'Not Matched' = 'black')) +
  labs(x='Plastic Waste Per Capita (kg/day)', y='Mismanaged Plastic Waste Per Capita (kg/day)') + 
  ggtitle('Data Quality Mismanaged Plastic Waste vs Plastic Waste') + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust=0.5))

# missing values
library(visdat)
vis_miss(plastic) # look into location of missing values
# Looking into the potential outliners.

library(mice)
md.pattern(plastic) # look into pattern

# install.packages('ggmice')
library(ggmice)
plot_pattern(plastic)

library(dplyr) # Look into the data
plastic %>% filter(!complete.cases(.))
plastic[!complete.cases(plastic$PWaste),] # Missing both PWaste and MPWaste

## Comments
# 1. Histograms and boxplots revealed that the distribution is right skew (positive skewness). 
# 2. From the rug plot there are around three data points,
#    which are far from the distribution. These points are more than are more than 200 kg/day.
# 3. From the summary, it can be identified that some of the 
# 4. Looking into the missing values there are MPWaste (19%) and PWaste (1%) columns, which contribute to 2% of the total data entries.
#    Another plot with 'mice' library provides insights into the missing values. We found out that there are 39 cases missing in the MPWaste and 2 cases missing both.


## Q2: The potential effects of region and income status on the distributions of plastic waste and mismanaged plastic.
## Region and income status 
library(dplyr)
library(ggplot2)
plastic$Region <- as.factor(plastic$Region)
plastic$IncomeStatus <- as.factor(plastic$IncomeStatus)


## Look into effect of Regions
library(ggpubr)
library(cowplot)
## Maybe add ordering from high to low
region_pw<-ggplot(plastic, aes(x = Region, y = PWaste, fill=Region)) +
  geom_boxplot() + 
  labs(title = 'Regions vs Plastic Waste',
       x = 'Regions',
       y = 'Plastic Waste per capita (kg/day)') + 
  theme(plot.title = element_text(hjust=0.5, size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1))

region_mpw<-ggplot(plastic, aes(x = Region, y = MPWaste, fill=Region)) +
  geom_boxplot() + 
  labs(title = 'Regions vs Mismanaged Plastic Waste',
       x = 'Regions',
       y = 'Mismanaged Plastic Waste per capita (kg/day)')+
  theme(plot.title = element_text(hjust=0.5, size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1))

region_plots<-ggarrange(region_pw, region_mpw,
          nrow = 1, ncol = 2,
          common.legend = TRUE, legend = 'right')

combined_plots <- ggdraw() +
  draw_plot(region_plots, width = 1, height = 1, x = 0, y = -0.05) +
  draw_label("Region Effect on Plastic Waste and Mismanaged Plastic Waste Distribution", 
             size = 14, x = 0.5, y = 0.96)

combined_plots

#######################
library(ggplot2)
library(ggpubr)
library(dplyr)

median_pwaste <- plastic %>% 
  group_by(Region) %>% 
  summarize(median_PWaste = median(PWaste, na.rm = TRUE))

plastic$Region <- factor(plastic$Region, levels = median_pwaste$Region[order(median_pwaste$median_PWaste)])

region_pw <- ggplot(plastic, aes(x = Region, y = PWaste, fill = Region)) +
  geom_boxplot() + 
  labs(title = 'Regions vs Plastic Waste',
       x = 'Regions',
       y = 'Plastic Waste Per Capita (kg/day)') + 
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        axis.text.x = element_text(angle = 25, hjust = 1))

region_mpw <- ggplot(plastic, aes(x = Region, y = MPWaste, fill = Region)) +
  geom_boxplot() + 
  scale_x_discrete(limits = levels(plastic$Region)) +  # Maintain the same order as plastic waste
  labs(title = 'Regions vs Mismanaged Plastic Waste',
       x = 'Regions',
       y = 'Mismanaged Plastic Waste Per Capita (kg/day)') +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        axis.text.x = element_text(angle = 25, hjust = 1))

# Arrange plots side by side
region_plots <- ggarrange(region_pw, region_mpw,
                          nrow = 1, ncol = 2,
                          common.legend = TRUE, legend = 'right')

combined_plots <- ggdraw() +
  draw_plot(region_plots, width = 1, height = 1, x = 0, y = -0.05) +
  draw_label("Region Effect on Plastic Waste and Mismanaged Plastic Waste Boxplots", 
             size = 14, x = 0.5, y = 0.96)

combined_plots
######################


## Look into the effect of Income Status
income_pw<-ggplot(plastic, aes(IncomeStatus, PWaste, fill=IncomeStatus)) +
  geom_boxplot() + 
  labs(title = 'IncomeStatus vs Plastic Waste',
       x = 'IncomeStatus',
       y = 'Plastic Waste Per Capita (kg/day)')+
  theme(plot.title = element_text(hjust=0.5, size = 11))

income_mpw<-ggplot(plastic, aes(IncomeStatus, MPWaste, fill=IncomeStatus)) +
  geom_boxplot() + 
  labs(title = 'IncomeStatus vs Mismanaged Plastic Waste',
       x = 'IncomeStatus',
       y = 'Mismanaged Plastic Waste Per Capita (kg/day)')+
  theme(plot.title = element_text(hjust=0.5, size = 11))

income_plots<-ggarrange(income_pw, income_mpw,
                        nrow = 1, ncol = 2,
                        common.legend = TRUE, legend = 'right')

combined_income_plots <- ggdraw() +
  draw_plot(income_plots, width = 1, height = 1, x = 0, y = -0.045) +
  draw_label("Income Status Effect on Plastic Waste and Mismanaged Plastic Waste Boxplots", 
             size = 14, x = 0.5, y = 0.96)
combined_income_plots


## Q3: The relationship between plastic waste and mismanaged plastic waste, and any potential impact of region and income status.
library(lattice)
xyplot(MPWaste~PWaste|IncomeStatus, data=plastic, 
       main = 'Different Income Status MPWaste vs PWaste', pch=16)

xyplot(MPWaste~PWaste|Region, data=plastic, 
       main = 'Different Region Status MPWaste vs PWaste', pch=16)

## GGplot
ggplot(plastic, aes(x=PWaste, y=MPWaste, col=Region))+
  geom_point() +
  facet_wrap(~ IncomeStatus, nrow = 2) +
  labs(x='Plastic Waste Per Capita (kg/day)', y='Mismanaged Plastic Waste Per Capita (kg/day)') + 
  ggtitle('Different Income Status Mismanaged Plastic Waste vs Plastic Waste') +
  theme(plot.title = element_text(hjust=0.5))

ggplot(plastic, aes(x=PWaste, y=MPWaste, col=IncomeStatus))+
  geom_point() +
  facet_wrap(~ Region, nrow = 2) +
  labs(x='Plastic Waste Per Capita (kg/day)', y='Mismanaged Plastic Waste Per Capita (kg/day)') + 
  ggtitle('Different Region Status Mismanaged Plastic Waste vs Plastic Waste') +
  theme(plot.title = element_text(hjust=0.5))


## Region and Income status
mosaicplot(~Region + IncomeStatus, data=plastic, col = 2:8)
income_region <- as.data.frame(xtabs(~IncomeStatus+Region, data=plastic))
ggplot(income_region, aes(x=Region, y=Freq, fill = IncomeStatus)) +
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5)+
  labs(x = 'Region', y = 'Frequency') +
  ggtitle('Frequency of Income Status by Region') + 
  theme(
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
# axis.text.x = element_text(angle=45, hjust=1),

ggplot(income_region, aes(x = Region, y = Freq, fill = IncomeStatus)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = 'Regions', y = 'Frequency') +
  ggtitle('Frequency of Income Status by Region') + 
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 25, hjust = 1)  # Adjust angle and hjust as needed
  )



# library(vcd)
# doubledecker(IncomeStatus~Region, data=plastic)

## Q4: The relationship between both types of plastic waste and the other quantitative variables.
# GDP, Population, Coastal Pop, Urban Pop
other_quantitative <- subset(plastic, select = c('GDP', 'Population', 'CoastalPopPC', 'UrbanPopPC', 'PWaste', 'MPWaste'))

pairs(other_quantitative)
library(ggplot2)
library(GGally)
ggpairs(other_quantitative, 
        aes(color=plastic$IncomeStatus, alpha = 0.7),
        title = 'Scatterplot Matrix for Other Quantitative Variables')+
  theme(plot.title = element_text(hjust=0.5))

ggpairs(other_quantitative)

library(lattice)
library(viridis)

parallelplot(other_quantitative, 
             col=c(1:4)[plastic$IncomeStatus], 
             horizontal=FALSE)

other_q_with_income <- subset(plastic, 
                              select = c('IncomeStatus', 'GDP', 'Population', 'CoastalPopPC', 'UrbanPopPC', 'PWaste', 'MPWaste'))
ggparcoord(other_q_with_income,
           columns = 2:7,
           groupColumn = 1,
           scale="uniminmax",
           showPoints = TRUE,
           title = 'Standardize Parallel Coordinate Plot for Other Quantitative Variables',
           alphaLines = 0.5,
           aes(color = IncomeStatus)) +
  scale_color_manual(values = c("HIC" = "purple", "LMC" = "grey", "UMC" = "grey", "LIC" = "red")) +
  theme(plot.title = element_text(hjust=0.5))

ggparcoord(other_q_with_income,
           columns = 2:7,
           groupColumn = 1,
           scale="uniminmax",
           showPoints = TRUE,
           title = 'Standardize Parallel Coordinate Plot for Other Quantitative Variables',
           alphaLines = 0.5,
           aes(color = IncomeStatus)) +
  scale_color_manual(values = c("HIC" = "grey", "LMC" = "blue", "UMC" = "darkgreen", "LIC" = "grey")) +
  theme(plot.title = element_text(hjust=0.5))


## Comments
#
#

## Q5: The smoothed trends between 
# (i) both types of plastic waste and GDP (the wealth of the country)
library(scales)
par(mfrow=c(2,2))
plot(x=plastic$GDP, y=plastic$PWaste,
     xlab = 'GDP Per Capita (USD)',
     ylab = 'PWaste Per Capita (kg/day)',
     main = 'GDP vs Plastic Waste')
axis(side = 1, at = seq(0, max(plastic$GDP), by = 10000), labels = FALSE) 
fitN <- ksmooth(plastic$GDP, plastic$PWaste, kernel = "normal", bandwidth = 7)
lfit_gdp_pw <- loess(PWaste ~ GDP, data=plastic, span = 0.5)
lpred_gdp_pw <- predict(lfit_gdp_pw, data.frame(GDP = fitN$x), se=TRUE)
polygon(x=c(fitN$x,rev(fitN$x)), y=c(lpred_gdp_pw$fit- qt(0.975,lpred_gdp_pw$df)*lpred_gdp_pw$se,
                                     rev(lpred_gdp_pw$fit+ qt(0.975,lpred_gdp_pw$df)*lpred_gdp_pw$se)),
        col=alpha('red',0.2),border=NA)
lines(x=fitN$x, y=lpred_gdp_pw$fit, col=2, lwd =2)

plot(x=plastic$GDP, y=plastic$MPWaste,
     xlab = 'GDP Per Capita (USD)',
     ylab = 'MPWaste Per Capita (kg/day)',
     main = 'GDP vs Mismanaged Plastic Waste')
axis(side = 1, at = seq(0, max(plastic$GDP), by = 10000), labels = FALSE) 
fitN <- ksmooth(plastic$GDP, plastic$MPWaste, kernel = "normal", bandwidth = 7)
lfit_gdp_mpw <- loess(MPWaste ~ GDP, data=plastic, span = 0.4)
lpred_gdp_mpw <- predict(lfit_gdp_mpw, data.frame(GDP = fitN$x), se=TRUE)
polygon(x=c(fitN$x,rev(fitN$x)), y=c(lpred_gdp_mpw$fit- qt(0.975,lpred_gdp_mpw$df)*lpred_gdp_mpw$se,
                                     rev(lpred_gdp_mpw$fit+ qt(0.975,lpred_gdp_mpw$df)*lpred_gdp_mpw$se)),
        col=alpha('red',0.2),border=NA)
lines(x=fitN$x, y=lpred_gdp_mpw$fit, col=2, lwd =2)

# (ii) both types of plastic waste and the size of urban population.
plot(x=plastic$UrbanPopPC, y=plastic$PWaste,
     xlab = 'Urban Population (%)',
     ylab = 'PWaste Per Capita (kg/day)',
     main = 'Urban Population vs Plastic Waste')
axis(side = 1, at = seq(0, max(plastic$UrbanPopPC), by = 10), labels = FALSE) 
fitN <- ksmooth(plastic$UrbanPopPC, plastic$PWaste, kernel = "normal", bandwidth = 7)
lfit_upop_pw <- loess(PWaste ~ UrbanPopPC, data=plastic, span = 0.25)
lpred_upop_pw <- predict(lfit_upop_pw, data.frame(UrbanPopPC = fitN$x), se=TRUE)
polygon(x=c(fitN$x,rev(fitN$x)), y=c(lpred_upop_pw$fit- qt(0.975,lpred_upop_pw$df)*lpred_upop_pw$se,
                                     rev(lpred_upop_pw$fit+ qt(0.975,lpred_upop_pw$df)*lpred_upop_pw$se)),
        col=alpha(col=3,0.25),border=NA)
lines(x=fitN$x, y=lpred_upop_pw$fit, col=3, lwd =2)

plot(x=plastic$UrbanPopPC, y=plastic$MPWaste,
     xlab = 'Urban Population (%)',
     ylab = 'MPlastic Waste Per Capita (kg/day)',
     main = 'Urban Population vs Mismanaged Plastic Waste')
axis(side = 1, at = seq(0, max(plastic$UrbanPopPC), by = 10), labels = FALSE)
fitN <- ksmooth(plastic$UrbanPopPC, plastic$MPWaste, kernel = "normal", bandwidth = 7)
lfit_upop_mpw <- loess(MPWaste ~ UrbanPopPC, data=plastic, span = 0.3)
lpred_upop_mpw <- predict(lfit_upop_mpw, data.frame(UrbanPopPC = fitN$x), se=TRUE)
polygon(x=c(fitN$x,rev(fitN$x)), y=c(lpred_upop_mpw$fit- qt(0.975,lpred_upop_mpw$df)*lpred_upop_mpw$se,
                                     rev(lpred_upop_mpw$fit+ qt(0.975,lpred_upop_mpw$df)*lpred_upop_mpw$se)),
        col=alpha(col=3,0.25),border=NA)
lines(x=fitN$x, y=lpred_upop_mpw$fit, col=3, lwd =2)

mtext(expression(bold('Gaussian Smoothed Estimated Trends')), side = 3, line = -1.2, outer = TRUE)
par(mfrow=c(1,1))
