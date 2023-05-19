## Importing the data: 


data = read.csv("Egg_counts_Beans_exp1.csv")
head(data)
tail(data)
library(ggplot2)
library(tidyverse)
#install.packages("ggpubr")
library(ggsignif)
library(ggprism)
library(ggstatsplot)
library(DescTools)


head(data)

## Grouping the data by bean type

data %>% group_by(Bean_Type)%>% 
  select(Total.egg.count)%>%
  summarise_all(list(mean = mean, 
                     stderr = sd, 
                     min = min, 
                     max = max))


## Boxplot to figure out outliers
p = ggplot(data, aes(Bean_Type, Total.egg.count, fill = Bean_Type)) 
p + geom_boxplot()


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

data
# t test
t_test_data1 = data_no_outlier[1:7,] # for BV and NS
t_test_data2 = data_no_outlier[5:11,] # NS vs RK


t.test(Total.egg.count ~ Bean_Type, data = t_test_data1)
t.test(Total.egg.count ~ Bean_Type, data = t_test_data2)
# anova comparison

model = aov(Total.egg.count ~ Bean_Type, data = data_no_outlier)
summary(model)

DunnettTest(x = data_no_outlier$Total.egg.count, g = data_no_outlier$Bean_Type)




#print(data_no_outlier)

p1 = ggplot(data_no_outlier, aes(Bean_Type, Total.egg.count, fill = Bean_Type)) 
p1 + geom_boxplot() + stat_summary(fun.y = mean, geom = "point", shape=23, size=2) + 
  labs(Title = "Meloidogyne hapla egg counts in three bean types", 
       x = "Bean Type", 
       y = " Egg Count") + geom_signif(comparisons = list(c("NemaSnap", "Red Kidney")), 
                                       test = "t.test", 
                                       map_signif_level = TRUE)


p1 + geom_violin() + stat_summary(fun.y = mean, geom = "point", shape=23, size=2) + 
  labs(Title = "Meloidogyne hapla egg counts in three bean types", 
       x = "Bean Type", 
       y = " Egg Count") + geom_signif(comparisons = list(c("NemaSnap", "Black Valentine")), 
                                       test = "t.test", 
                                       map_signif_level = TRUE)


