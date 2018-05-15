options("scipen"=100, "digits"=4)
#### mosaic plot

# Load all packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggthemes)

library(PerformanceAnalytics)
chart.Correlation(d.samp[59:61])

install.packages("ggthemes")

# Script generalized into a function
mosaicGG <- function(data, X, FILL) {
  
  # Proportions in raw data
  DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
  DF$groupSum <- rowSums(DF)
  DF$xmax <- cumsum(DF$groupSum)
  DF$xmin <- DF$xmax - DF$groupSum
  DF$X <- row.names(DF)
  DF$groupSum <- NULL
  DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
  library(dplyr)
  DF_melted <- DF_melted %>% 
    group_by(X) %>% 
    mutate(ymax = cumsum(value/sum(value)),
           ymin = ymax - value/sum(value))
  
  # Chi-sq test
  results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
  resid <- melt(results$residuals)
  names(resid) <- c("FILL", "X", "residual")
  
  # Merge data
  DF_all <- merge(DF_melted, resid)
  
  # Positions for labels
  DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
  index <- DF_all$xmax == max(DF_all$xmax)
  DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
  
  # plot:
  g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin, 
                          xmax = xmax, fill = residual)) + 
    geom_rect(col = "white") +
    geom_text(aes(x = xtext, label = X),
              y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
    geom_text(aes(x = max(xmax),  y = ytext, label = FILL),
              size = 3, hjust = 1, show.legend = FALSE) +
    scale_fill_gradient2("Residuals") +
    scale_x_continuous("Individuals", expand = c(0,0)) +
    scale_y_continuous("Proportion", expand = c(0,0)) +
    theme_tufte() +
    theme(legend.position = "bottom")
  print(g)
}


# BMI described by age
mosaicGG(data3, "HV237_Anything_done_to_water_to_make_safe_to_drink","HV270_The_wealth_index")

table(HV204_Time_to_get_to_water,HV270_The_wealth_index)

table(data3$HV201_Major_source_of_drinking_water, data3$HV237_Anything_done_to_water_to_make_safe_to_drink)


# Poverty described by age
mosaicGG(data4, "HV271_Wealth_index_factor_score","HV025_residence_urban_or_rural")

# mtcars: am described by cyl
mosaicGG(mtcars, "cyl", "am")

# Vocab: vocabulary described by education
library(car)
mosaicGG(Vocab, "education", "vocabulary")








#HEATMAP

# Create color palette
myColors <- brewer.pal(9, "Reds")

# Build the heat map from scratch

ggplot(data4, aes(x = HV210_A_bicycle, y = HV201_Major_source_of_drinking_water, fill = data4$HV204_Time_to_get_water_new)) +
  geom_tile() +
  facet_wrap(~HV024_Region_of_residence, ncol = 1) +
  scale_fill_gradientn(colors = myColors)

#heat map alternative

# The heat map we want to replace
# Don't remove, it's here to help you!
myColors <- brewer.pal(9, "Reds")
ggplot(data4, aes(x = HV201_Major_source_of_drinking_water, y = HV210_A_bicycle, fill = data4$HV204_Time_to_get_water_new)) +
  geom_tile() +
  facet_wrap( ~ HV024_Region_of_residence, ncol = 1) +
  scale_fill_gradientn(colors = myColors)

# Line plots
ggplot(data4, aes(x=HV210_A_bicycle, y = data4$HV204_Time_to_get_water_new, col = HV201_Major_source_of_drinking_water, group = HV201_Major_source_of_drinking_water)) +
  geom_line() +
  facet_wrap(~ site, nrow = 1)

#alternative 2

# Create overlapping ribbon plot from scratch
ggplot(data4, aes(x = HV210_A_bicycle, y = data4$HV204_Time_to_get_water_new, col = HV024_Region_of_residence, group = HV024_Region_of_residence, fill = HV024_Region_of_residence)) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "ribbon", alpha = 0.1, col = NA)


