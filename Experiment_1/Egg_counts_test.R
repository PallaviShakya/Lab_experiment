setwd("~/directory")
install.packages("htmltools")


#libraries used:
library(devtools)
library(tidyverse)
#library(rstatix)
#devtools::install_github("jbkunst/highcharter")
#packageVersion("highcharter")
library(dplyr)
library("highcharter")
library(readr)
library(ggplot2)
library(viridis)
library(ggsignif)
library(ggprism)
library(ggstatsplot)
library(DescTools)


data = read.csv("Egg_counts_Beans_exp1.csv")
head(data)

data %>% group_by(Bean_Type) %>% select(Total.egg.count) %>% summarise_all(list(mean = mean, 
                                                                               stderr = sd, 
                                                                               min = min, 
                                                                               max = max))

#saving data in three different vectors 

Red_Kidney_eggcounts = data %>% filter(Bean_Type == "Red Kidney ") %>% pull(Total.egg.count)

BlackValentine_eggcounts = data %>% filter(Bean_Type == "Black Valentine") %>% pull(Total.egg.count)

NemaSnap_eggcounts = data %>% filter(Bean_Type == "NemaSnap") %>% pull(Total.egg.count)


# Test for normal distribution: 

shapiro.test(Red_Kidney_eggcounts) #W = 0.9219, p-value = 0.5476

## The null hypothesis is: the sample being tested is normally distributed. 
## Interpretation: p value is not less than 0.05. Hence we do not reject the null hypothesis. 

shapiro.test(BlackValentine_eggcounts) #W = 0.90979, p-value = 0.4813

## The null hypothesis is: the sample being tested is normally distributed. 
## Interpretation: p value is not less than 0.05. Hence we do not reject the null hypothesis. 

shapiro.test(NemaSnap_eggcounts) #W = 0.70109, p-value = 0.01196

## The null hypothesis is: the sample being tested is normally distributed. 
## Interpretation: p value is less than 0.05. Hence we reject the null hypothesis. 

### Might be some outliers?

## Boxplot to figure out outliers
outlier_values_RK <- boxplot.stats(data$Total.egg.count)$out
boxplot(Total.egg.count ~ Bean_Type, data = data, col = c("purple", "green", "red"))

tiff("firstegg_boxplot.tiff", units = "in", width = 5, height = 5, res = 300)


data %>% ggplot(aes(x = Bean_Type, Total.egg.count, fill = Bean_Type)) + 
  geom_boxplot() + 
  scale_fill_brewer(type = "Bean_Type", palette = "Accent")+  
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) + 
  theme_linedraw() + theme(plot.title = element_text(size = 15, hjust = 0.5), 
                           plot.title.position = "panel", 
                           plot.subtitle = element_text(size = 11, hjust = 0.5)) + 
  xlab("Type of Beans") + ylab("Total Egg Counts") + 
  labs(title = "VW9 Egg Counts per Each Bean Type", 
       subtitle = "Plot of Bean type by egg counts", 
       caption = "Small dots in the plot denote data points.
                  Big dots in the plot denote outlier.")

dev.off()
## Removing the outliers
# Removing outliers

list_quantiles <- tapply(data$Total.egg.count, data$Bean_Type, quantile)
Q1s = sapply(1:3, function(i) list_quantiles[[i]][2])
Q3s = sapply(1:3, function(i) list_quantiles[[i]][4])

IQRs = tapply(data$Total.egg.count, data$Bean_Type, IQR)


Lowers = Q1s - 1.5*IQRs
Uppers = Q3s + 1.5*IQRs

datas = split(data, data$Bean_Type)

data_no_outlier <- NULL
for (i in 1:3){
  out <- subset(datas[[i]], datas[[i]]$Total.egg.count > Lowers[i] & datas[[i]]$Total.egg.count < Uppers[i])
  data_no_outlier <- rbind(data_no_outlier, out)
}

## Boxplot after removing the outliers
data_no_outlier %>% ggplot(aes(x = Bean_Type, Total.egg.count, fill = Bean_Type)) + 
  geom_boxplot() + 
  scale_fill_brewer(type = "Bean_Type", palette = "Accent")+ 
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) + 
  theme_linedraw() + theme(plot.title = element_text(size = 15, hjust = 0.5), 
                           plot.title.position = "panel", 
                           plot.subtitle = element_text(size = 11, hjust = 0.5)) + 
  xlab("Type of Beans") + ylab("Total Egg Counts") + 
  labs(title = "VW9 Egg Counts per Each Bean Type", 
       subtitle = "Plot of Bean type by egg counts", 
       caption = "Small dots in the plot denote data points and big dots in the plot denote outlier")


#saving data in three different vectors 

RK_eggcounts = data_no_outlier%>% filter(Bean_Type == "Red Kidney ") %>% pull(Total.egg.count)

BV_eggcounts = data_no_outlier %>% filter(Bean_Type == "Black Valentine") %>% pull(Total.egg.count)

NS_eggcounts = data_no_outlier %>% filter(Bean_Type == "NemaSnap") %>% pull(Total.egg.count)

## Test for normal distribution again: 

# Test for normal distribution: 

shapiro.test(RK_eggcounts) #W = 0.9219, p-value = 0.5476

## The null hypothesis is: the sample being tested is normally distributed. 
## Interpretation: p value is not less than 0.05. Hence we do not reject the null hypothesis. 

shapiro.test(BV_eggcounts) #W = 0.90979, p-value = 0.4813

## The null hypothesis is: the sample being tested is normally distributed. 
## Interpretation: p value is not less than 0.05. Hence we do not reject the null hypothesis. 

shapiro.test(NS_eggcounts) #W = 0.70109, p-value = 0.6369

## The null hypothesis is: the sample being tested is normally distributed. 
## Interpretation: p value is less than 0.05. Hence we reject the null hypothesis. 

## Correlation between weight and total egg count

cor.test(data_no_outlier$Egg.count.per.ml, data_no_outlier$Weight) #P.value = 0.03

## Null hypothesis = True correlation is zero/there is no correlation/they are independent
## P value is less than 0.03. We reject the null hypothesis, i.e. there is correlation. 

## T test
#A paired t-test is designed to compare the means of the same group or item under 
#two separate scenarios. 
#An unpaired t-test compares the means of two independent or unrelated groups.

t.test(RK_eggcounts, NS_eggcounts, paired = FALSE)
t.test(BV_eggcounts, NS_eggcounts, paired = FALSE)

tiff("final_boxplot.tiff", units = "in", width = 5, height = 5, res = 300)

data_no_outlier %>% ggplot(aes(x = Bean_Type, Total.egg.count, fill = Bean_Type)) + 
  geom_boxplot() + geom_signif(comparisons = list(c("NemaSnap", "Black Valentine")), 
                               test = "t.test",
                               map_signif_level = FALSE) +
  scale_fill_brewer(type = "Bean_Type", palette = "Accent")+ 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  theme_linedraw() + theme(plot.title = element_text(size = 15, hjust = 0.5), 
                           plot.title.position = "panel", 
                           plot.subtitle = element_text(size = 11, hjust = 0.5)) + 
  xlab("Type of Beans") + ylab("Total Egg Counts") + 
  labs(title = "VW9 Egg Counts per Each Bean Type", 
       subtitle = "Plot of Bean type by egg counts", 
       caption = "p-value = 0.045, 
                  The small black points indicated the data points.")
dev.off()
