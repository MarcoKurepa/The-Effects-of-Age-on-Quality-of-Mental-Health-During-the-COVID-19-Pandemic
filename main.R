# installing the packages
install.packages('tidyverse', dependencies = TRUE)
install.packages('ggsignif', dependencies = TRUE)
install.packages('ggpubr', dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("car", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)

# importing the libraries
library(tidyverse)
library(ggsignif)
library(ggpubr)
library(dplyr)
library(car)
library(ggplot2)
library(ggsignif)

# import age data
age_data <- read.csv("C:/Users/Jacob/Desktop/Visual Studio Code/R/age_data.csv")

lm <- lm(age_data$Age_num ~ age_data$GAD)
summary(lm)

# running the Student's t-test on smth vs. age
good_mental_t <- t.test(good.mental ~ age_2, data = age_data)
bad_mental_t <- t.test(Bad.mental ~ age_2, data = age_data)
gad_t <- t.test(GAD ~ age_2, data = age_data)
mdd_t <- t.test(MDD ~ age_2, data = age_data)
ptsd_t <- t.test(PTSD ~ age_2, data = age_data)

# Shapiro test to test for parametric data
bad_mental_shap_young <- shapiro.test(age_data$Bad.mental[age_data$age_2 == "Ages <50"])
gad_shap_young <- shapiro.test(age_data$GAD[age_data$age_2 == "Ages <50"])
mdd_shap_young <- shapiro.test(age_data$MDD[age_data$age_2 == "Ages <50"])
ptsd_shap_young <- shapiro.test(age_data$PTSD[age_data$age_2 == "Ages <50"])

bad_mental_shap_old <- shapiro.test(age_data$Bad.mental[age_data$age_2 == "Ages >=50"])
gad_shap_old <- shapiro.test(age_data$GAD[age_data$age_2 == "Ages >=50"])
mdd_shap_old <- shapiro.test(age_data$MDD[age_data$age_2 == "Ages >=50"])
ptsd_shap_old <- shapiro.test(age_data$PTSD[age_data$age_2 == "Ages >=50"])

# outputing the p-values of the shapiro tests
bad_mental_shap_young$p.value
gad_shap_young$p.value
mdd_shap_young$p.value
ptsd_shap_young$p.value

bad_mental_shap_old$p.value
gad_shap_old$p.value
mdd_shap_old$p.value
ptsd_shap_old$p.value

# doing linear regressions on smth vs. age
bad_mental_lm <- lm(Bad.mental ~ Age_num, data = age_data)
gad_lm <- lm(GAD ~ Age_num, data = age_data)
mdd_lm <- lm(MDD ~ Age_num, data = age_data)
ptsd_lm <- lm(PTSD ~ Age_num, data = age_data)

summary(gad_lm) # getting the output of one of the linear regressions

# doing anova tests on each of the linear regressions (idrk what there are, just gpt it)
good_mental_anova <- aov(good_mental_lm)
bad_mental_anova <- aov(bad_mental_lm)
gad_anova <- aov(gad_lm)
mdd_anova <- aov(mdd_lm)
ptsd_anova <- aov(ptsd_lm)

# doing a post hoc tukey test on the anova values
good_mental_tukey <- TukeyHSD(good_mental_anova)
bad_mental_tukey <- TukeyHSD(bad_mental_anova)
gad_tukey <- TukeyHSD(gad_anova)
mdd_tukey <- TukeyHSD(mdd_anova)
ptsd_tukey <- TukeyHSD(ptsd_anova)

# gets the person coeficiant (kinda like a linear regression)
good_mental_cor <- cor.test(age_data$Age_num, age_data$good.mental, method = "pearson")
bad_mental_cor <- cor.test(age_data$Age_num, age_data$Bad.mental, method = "pearson")
gad_cor <- cor.test(age_data$Age_num, age_data$GAD, method = "pearson")
mdd_cor <- cor.test(age_data$Age_num, age_data$MDD, method = "pearson")
ptsd_cor <- cor.test

gad_cor

age_data$PTSD
# outputing a bunch of the tests
bad_mental_t$p.value
gad_t$p.value
mdd_t$p.value
ptsd_t$p.value

summary(bad_mental_lm)$r.squared
summary(gad_lm)$r.squared
summary(mdd_lm)$r.squared
summary(ptsd_lm)$r.squared

summary(bad_mental_lm)
summary(gad_lm)
summary(mdd_lm)
summary(ptsd_lm)

bad_mental_cor
gad_cor
mdd_cor
ptsd_cor


bad_mental_shap$p.value
gad_shap$p.value
mdd_shap$p.value
ptsd_shap$p.value


# boxplots but more detailed
gad_box_plot <- ggplot(age_data, aes(x=age_2, y=GAD)) + 
  stat_boxplot(geom = "errorbar", width = 0.2) + #adds the whiskers
  geom_boxplot(fill="skyblue") + #makes it an orange boxplot 
  labs(x="Age groups", y="Percentage of Canadians with GAD", 
       title="Percentage of Canadians with GAD vs. Age") +
  scale_x_discrete(limits=c("Ages <50", "Ages >=50")) + 
  geom_signif(comparisons = list(c("Ages <50", "Ages >=50")), # adding the significance asktriks
              test = "t.test", p_threshold = 0.05, 
              map_signif_level=TRUE) + 
  theme(text = element_text(hjust = 0.5, size = 20), # hjust = 0.5 means centre alligned
        plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = 0.3, y = 2.4, label = "A", size = 13) + #labeling the graph as Figure A
  coord_cartesian(xlim = c(1, 2), ylim = c(5, 25), clip = "off") # making it so that we can annotate off of the graph

show(gad_box_plot)

mdd_box_plot <- ggplot(age_data, aes(x=age_2, y=MDD)) + 
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(fill="skyblue") + 
  labs(x="Age groups", y="Percentage of Canadians with MDD", 
       title="Percentage of Canadians with MDD vs. Age") +
  scale_x_discrete(limits=c("Ages <50", "Ages >=50")) + 
  geom_signif(comparisons = list(c("Ages <50", "Ages >=50")), 
              test = "t.test", p_threshold = 0.05, 
              map_signif_level=TRUE) + 
  theme(text = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = 0.275, y = 1, label = "B", size = 13) +
  coord_cartesian(xlim = c(1, 2), ylim = c(5, 32), clip = "off")

bad_mental_box_plot <- ggplot(age_data, aes(x=age_2, y=Bad.mental)) + 
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(fill="skyblue") + 
  labs(x="Age groups", y="Percentage of Canadians with Fair or Poor Mental Health", 
       title="Percentage of Canadians with Fair or Poor Mental Health vs. Age") +
  scale_x_discrete(limits=c("Ages <50", "Ages >=50")) + 
  geom_signif(comparisons = list(c("Ages <50", "Ages >=50")), 
              test = "t.test", p_threshold = 0.05, 
              map_signif_level=TRUE) + 
  theme(text = element_text(hjust = 0.5, size = 15),
        plot.title = element_text(hjust = 0.5))


age_data$Age <- factor(age_data$Age) # turns the qualitative age ranges into ints (1, 2, 3, 4)

# scatter plot
plot(Bad.mental ~ Age_num, data = age_data, 
     xaxt = "n", # gets rid of the x-axis labels so that we can use custom ones
     col = "black", 
     pch = 16, cex = 1, # pch = 16 means filled dots; any with cex controls size
     xlab = "Age", 
     ylab = "Percentage of Canadians with Fair or Poor Mental Health", 
     main = "Percentage of Canadians with Fair or Poor Mental Health vs. Age", 
     cex.axis = 1.1,
     cex.lab = 1.2,
     cex.main = 1.6) #size of the title
axis(1, at = unique(age_data$Age_num), labels = unique(age_data$Age), cex.axis = 1.1) #adding out custom x-axis labels
abline(bad_mental_lm, col = "red", lwd = 4) #adding the linear regression model to the graph
text(3.8, 15.5, bquote(R^2 ~ "=" ~ 0.8881579)) #puttin the r^2 value on the graph

plot(GAD ~ Age_num, data = age_data, 
     xaxt = "n", 
     col = "black", 
     pch = 16, cex = 1, 
     xlab = "Age", 
     ylab = "Percentage of Canadians with GAD", 
     main = "Percentage of Canadians with GAD vs. Age", 
     cex.axis = 1.1,
     cex.lab = 1.25,
     cex.main = 1.6)
axis(1, at = unique(age_data$Age_num), labels = unique(age_data$Age), cex.axis = 1.1)
abline(gad_lm, col = "red", lwd = 4)
text(3.8, 24, bquote(R^2 ~ "=" ~ 0.8518456))
text(0.575, 0.05, "A", cex = 3.5, adj = c(0,0), xpd = TRUE)


plot(MDD ~ Age_num, data = age_data, 
     xaxt = "n", 
     col = "black", 
     pch = 16, cex = 1, 
     xlab = "Age", 
     ylab = "Percentage of Canadians with MDD", 
     main = "Percentage of Canadians with MDD vs. Age", 
     cex.axis = 1.1,
     cex.lab = 1.25,
     cex.main = 1.6)
axis(1, at = unique(age_data$Age_num), labels = unique(age_data$Age), cex.axis = 1.1)
abline(mdd_lm, col = "red", lwd = 4)
text(3.9, 31, bquote(R^2 ~ "=" ~ 0.8614))
text(0.575, -0.25, "B", cex = 3.5, adj = c(0,0), xpd = TRUE)


#drawing the boxplots
show(gad_box_plot)
show(mdd_box_plot)
show(bad_mental_box_plot)



