library(readxl)
soru1 <- read_excel("C:/Users/Lenovo/Desktop/soru1.xlsx")
View(soru1)

##İŞARET TESTİ
install.packages("DescTools")
library(DescTools)
diyet4=c(19,20,18,19,18,19)
SignTest(x=diyet4,mu=15,alternative="greater")

##WİLCOXON TESTİ
install.packages("stats")
library(stats)
diyet1=c(12,10,13,11,12,9)
wilcox.test(x,mu=17,alternative="less") 

##MANN-WHITNEY U TESTİ
diyet1 <- c(12,10,13,11,12,9)
diyet2 <- c(18,19,18,18,19,19)
wilcox.test(diyet1,diyet2)

##MANN-WHITNEY U TESTİ
diyet1 <- c(12,10,13,11,12,9)
diyet4 <- c(19,20,18,19,18,19)
wilcox.test(diyet1,diyet4)

##KRUSKAL-WALLİS 
veri <- read_excel("C:/Users/Lenovo/Desktop/soru1.xlsx")
grup <- rep(1:6, each = 8)
veri <- unlist(veri)
grup <- unlist(grup)
df <- data.frame(
  value = as.numeric(veri),
  group = as.factor(grup)
)
result <- kruskal.test(value ~ group, data = df)
print(result)


##UYGULAMA 3
####SORU-1####
library(stats)
install.packages("PMCMRplus")
library(PMCMRplus)

soru1=read.table(file.choose(),header=TRUE,sep=",")
attach(soru1)
head(soru1,3)

boxplot(Sure~Grup)

####ggplot2 paketi ile 
library(ggplot2)
ggplot(soru1, aes(x=as.factor(Grup), y=Sure)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Grup")

kruskal.test(Sure~Grup,data=soru1)
#posthoc.kruskal.nemenyi.test(x=soru1$Sure,soru1$Grup, method="Chisq")
PMCMRplus::kwAllPairsNemenyiTest(x=soru1$Sure,soru1$Grup, method="Chisq")

diyet1=c(12,10,13,11,12,9)
diyet2=c(18,19,18,18,19,19)
diyet3=c(10,12,13,16,14,13)
diyet4=c(19,20,18,19,18,19)
wilcox.test(diyet1,diyet2)
wilcox.test(diyet2,diyet3)
wilcox.test(diyet3,diyet4)
wilcox.test(diyet1,diyet4)
pairwise.wilcoxon(diyet1, diyet2, diyet3, diyet4, p.adjust.method = "BH")

library(stats)

# Verileri tanımlayın
diyet1 <- c(12, 10, 13, 11, 12, 9)
diyet2 <- c(18, 19, 18, 18, 19, 19)
diyet3 <- c(10, 12, 13, 16, 14, 13)
diyet4 <- c(19, 20, 18, 19, 18, 19)

# Kruskal-Wallis testi uygulayın
kruskal.test(diyet1, diyet2, diyet3, diyet4)

# Post hoc test olarak Mann-Whitney U testini uygulayın
pairwise.wilcoxon(diyet1, diyet2, diyet3, diyet4, p.adjust.method = "BH")

library(tidyverse)

# Verileri tanımlayın
diyet1 <- c(12, 10, 13, 11, 12, 9)
diyet2 <- c(18, 19, 18, 18, 19, 19)
diyet3 <- c(10, 12, 13, 16, 14, 13)
diyet4 <- c(19, 20, 18, 19, 18, 19)

# Kruskal-Wallis testi uygulayın
kruskal.test(diyet1, diyet2, diyet3, diyet4)

# Post hoc test olarak Mann-Whitney U testini uygulayın
diyet1 %>% wilcox.test(diyet2, p.adjust.method = "BH")
diyet1 %>% wilcox.test(diyet3, p.adjust.method = "BH")
diyet1 %>% wilcox.test(diyet4, p.adjust.method = "BH")
diyet2 %>% wilcox.test(diyet3, p.adjust.method = "BH")
diyet2 %>% wilcox.test(diyet4, p.adjust.method = "BH")
diyet3 %>% wilcox.test(diyet4, p.adjust.method = "BH")
