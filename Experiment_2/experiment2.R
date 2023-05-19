setwd("~/directory/")


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
library(ggsci)
library(ggpubr)

data = read.csv("vw9vslmx.csv")
head(data)

data_NS = data[1:12, ]
data_NS

data_BV = data[13:24, ]
data_BV

data %>% group_by(Treatment) %>% select(Total.egg.count) %>% summarise_all(list(mean = mean, 
                                                                                stderr = sd, 
                                                                                min = min, 
                                                                                max = max))

data2 = data.frame(data)
#saving data in three different vectors to test for normal distribution
BV_Lmx = data%>% filter(Treatment == "BV_Lmx") %>% pull(Total.egg.count)

BV_VW9 = data %>% filter(Treatment == "BV_VW9") %>% pull(Total.egg.count)

NS_Lmx = data %>% filter(Treatment == "NS_Lmx") %>% pull(Total.egg.count)

NS_VW9 = data %>% filter(Treatment == "NS_VW9") %>% pull(Total.egg.count)


# Test for normal distribution: 

shapiro.test(BV_Lmx) #W = 0.91513, p-value = 0.471

## The null hypothesis is: the sample being tested is normally distributed. 
## Interpretation: p value is not less than 0.05. Hence we do not reject the null hypothesis. 

shapiro.test(BV_VW9) #W = 0.9394, p-value = 0.6543

## The null hypothesis is: the sample being tested is normally distributed. 
## Interpretation: p value is not less than 0.05. Hence we do not reject the null hypothesis. 

shapiro.test(NS_Lmx) #W = 0.94682, p-value = 0.7144

## The null hypothesis is: the sample being tested is normally distributed. 
## Interpretation: p value is not less than 0.05. Hence we do not reject the null hypothesis. 

shapiro.test(NS_VW9) # W = 0.79172, p-value = 0.05
## The null hypothesis is: the sample being tested is normally distributed. 
## Interpretation: p value is less than 0.05. Hence we do not reject the null hypothesis. 


tiff("firstegg_boxplot.tiff", units = "in", width = 5, height = 5, res = 300)


#split(data, cumsum(1:nrow(data)%in%13)) 

data%>% ggplot(aes(x = Treatment, Total.egg.count, fill = Treatment)) + 
  geom_boxplot() + 
  scale_fill_brewer(type = "Treatment", palette = "Accent")+  
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) + 
  theme_linedraw() + theme(plot.title = element_text(size = 15, hjust = 0.5), 
                           plot.title.position = "panel", 
                           plot.subtitle = element_text(size = 11, hjust = 0.5)) + 
  xlab("Treatment") + ylab("Total Egg Counts") + 
  labs(title = "Egg Counts per Each Bean Type", 
       subtitle = "Plot of Bean type by egg counts", 
       caption = "BV = Black Valentine, 
                  NS = NemaSnap, 
                  Lmx and VW9 are the strains of M hapla")

dev.off()

tiff("grayeggcounts.tiff", units = "in", width = 5, height = 5, res = 300)


data %>% ggplot(aes(x = Treatment, Total.egg.count, fill = Treatment)) + 
  geom_boxplot() + 
  scale_fill_brewer(type = "Treatment", palette = "Greys")+  
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) + 
  theme_linedraw() + theme(plot.title = element_text(size = 15, hjust = 0.5), 
                           plot.title.position = "panel", 
                           plot.subtitle = element_text(size = 11, hjust = 0.5)) + 
  xlab("Treatment") + ylab("Total Egg Counts") + 
  labs(title = "Egg Counts per Each Bean Type", 
       subtitle = "Plot of Bean type by egg counts", 
       caption = "BV = Black Valentine, NS = NemaSnap, 
       Lmx and VW9 are the strains of M hapla")

dev.off()
## Removing the outliers
# Removing outliers

#list_quantiles <- tapply(data$Total.egg.count, data$Bean_Type, quantile)
#Q1s = sapply(1:3, function(i) list_quantiles[[i]][2])
#Q3s = sapply(1:3, function(i) list_quantiles[[i]][4])

#IQRs = tapply(data$Total.egg.count, data$Bean_Type, IQR)


#Lowers = Q1s - 1.5*IQRs
#Uppers = Q3s + 1.5*IQRs

#datas = split(data, data$Bean_Type)

#data_no_outlier <- NULL
#for (i in 1:3){
  #out <- subset(datas[[i]], datas[[i]]$Total.egg.count > Lowers[i] & datas[[i]]$Total.egg.count < Uppers[i])
  #data_no_outlier <- rbind(data_no_outlier, out)
#}


## Correlation between weight and total egg count

cor.test(data_no_outlier$Egg.count.per.ml, data_no_outlier$Weight) #P.value = 0.03

## Null hypothesis = True correlation is zero/there is no correlation/they are independent
## P value is less than 0.03. We reject the null hypothesis, i.e. there is correlation. 

## T test
#A paired t-test is designed to compare the means of the same group or item under 
#two separate scenarios. 
#An unpaired t-test compares the means of two independent or unrelated groups.

t.test(NS_VW9, NS_Lmx, paired = FALSE) #t = -3.99, p-value = 0.0093
t.test(BV_VW9, BV_Lmx, paired = FALSE) #t = -0.56, p-value = 0.5947

tiff("final_boxplot.tiff", units = "in", width = 7, height = 7, res = 300)

data %>% ggplot(aes(x = Treatment, Total.egg.count, fill = Treatment)) + 
  geom_boxplot() + geom_signif(comparisons = list(c("NS_Lmx", "NS_VW9"), c("BV_Lmx", "BV_VW9")), 
                               test = "t.test",
                               map_signif_level = TRUE) +
  scale_fill_brewer(type = "Treatment", palette = "Paired")+ 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  theme_linedraw() + theme(plot.title = element_text(size = 15, hjust = 0.5), 
                           plot.title.position = "panel", 
                           plot.subtitle = element_text(size = 11, hjust = 0.5)) + 
  xlab("Treatment") + ylab("Total Egg Counts") + 
  labs(title = "Comparison of egg counts between NS and BV beans 
       infected by M hapla strains VW9 and LMx", 
       caption = "The small black points indicate data points.
       NS = NemaSnap Bean, 
       BV = Black Valentine Bean, 
       p-value notation: 
        NS. = not significant, 
       *, **, ***= significant")
dev.off()

## Separate figures
tiff("NSbeans.tiff", units = "in", width = 9, height = 9, res = 300)

data_NS %>% ggplot(aes(x = Treatment, Total.egg.count, fill = Treatment)) + 
  geom_boxplot() + geom_signif(comparisons = list(c("NS_Lmx", "NS_VW9")), 
                               test = "t.test",
                               map_signif_level = TRUE) +
scale_fill_brewer(type = "Treatment", palette = "Pastel1")+ 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  theme_linedraw() + theme(plot.title = element_text(size = 15, hjust = 0.5), 
                           plot.title.position = "panel", 
                           plot.subtitle = element_text(size = 11, hjust = 0.5)) + 
  xlab("Treatment") + ylab("Total Egg Counts") + 
  labs(title = "Comparison of egg counts between NS beans 
       infected with Lmx and VW9 strains", 
       caption = "The small black points indicate data points.
       NS = NemaSnap Bean,
       p-value notation: 
        NS. = not significant, 
       *, **, ***= significant")
dev.off()

tiff("BVbeans.tiff", units = "in", width = 9, height = 9, res = 300)

data_BV %>% ggplot(aes(x = Treatment, Total.egg.count, fill = Treatment)) + 
  geom_boxplot() + geom_signif(comparisons = list(c("BV_Lmx", "BV_VW9")), 
                               test = "t.test",
                               map_signif_level = TRUE) +
  scale_fill_brewer(type = "Treatment", palette = "Pastel2")+ 
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  theme_linedraw() + theme(plot.title = element_text(size = 15, hjust = 0.5), 
                           plot.title.position = "panel", 
                           plot.subtitle = element_text(size = 11, hjust = 0.5)) + 
  xlab("Treatment") + ylab("Total Egg Counts") + 
  labs(title = "Comparison of egg counts between BV beans 
       infected with Lmx and VW9 strains", 
       caption = "The small black points indicate data points.
       NS = NemaSnap Bean,
       p-value notation: 
        NS. = not significant, 
       *, **, ***= significant")
dev.off()
