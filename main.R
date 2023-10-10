

# importing the libraries
library(tidyverse)
library(ggsignif)
library(ggpubr)
library(dplyr)
library(car)
library(ggplot2)
library(ggsignif)

# import age data
age_data <- read.csv("C:/Users/alanb/Downloads/The-Effects-of-Age-on-Quality-of-Mental-Health-During-the-COVID-19-Pandemic/age_data.csv")

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

# outputting the p-values of the shapiro tests
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

age_data$PTSD
# outputting a bunch of the tests
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


gad_box_plot <- ggplot(age_data, aes(x = age_2, y = GAD)) + 
  stat_boxplot(geom = "errorbar", width = 0.2) + 
  geom_boxplot(fill = "skyblue") + 
  labs(x = "Age groups", y = "Percentage of Canadians with GAD", 
       title = "") +
  scale_x_discrete(limits = c("Ages <50", "Ages >=50")) + 
  geom_signif(comparisons = list(c("Ages <50", "Ages >=50")),
              test = "t.test", p_threshold = 0.05, 
              map_signif_level = TRUE) + 
  theme_minimal() +
  theme(
    text = element_text(size = 20, face = "bold"),  # Make the text bold
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")  # Center and make the title bold
  ) +
  annotate("text", x = 2.5, y = 2.4, label = "A", size = 13) +  # Adjust x-coordinate to move "A" to the right
  coord_cartesian(xlim = c(1, 2), ylim = c(5, 25), clip = "off")

show(gad_box_plot)

mdd_box_plot <- ggplot(age_data, aes(x = age_2, y = MDD)) + 
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(fill = "skyblue") + 
  labs(x = "Age groups", y = "Percentage of Canadians with MDD", 
       title = "") +
  scale_x_discrete(limits = c("Ages <50", "Ages >=50")) + 
  geom_signif(comparisons = list(c("Ages <50", "Ages >=50")), 
              test = "t.test", p_threshold = 0.05, 
              map_signif_level = TRUE) + 
  theme_minimal() +  # Apply the same theme as before
  theme(
    text = element_text(size = 20, face = "bold"),  # Make the text bold
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")  # Center and make the title bold
  ) +
  annotate("text", x = 2.5, y = 1, label = "B", size = 13) +  # Adjust x-coordinate to move "B" to the right
  coord_cartesian(xlim = c(1, 2), ylim = c(5, 32), clip = "off")

show(mdd_box_plot)



bad_mental_box_plot <- ggplot(age_data, aes(x = age_2, y = Bad.mental)) + 
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(fill = "skyblue") + 
  labs(x = "Age groups", 
       y = "Percentage of Canadians with\nFair or Poor Perceived\nMental Health",  # Use "\n" to insert a line break
       title = "") +
  scale_x_discrete(limits = c("Ages <50", "Ages >=50")) + 
  geom_signif(comparisons = list(c("Ages <50", "Ages >=50")), 
              test = "t.test", p_threshold = 0.05, 
              map_signif_level = TRUE) + 
  theme_minimal() +  # Apply the same theme as before
  theme(
    text = element_text(size = 20, face = "bold"),  # Make the text bold
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")  # Center and make the title bold
  )

show(bad_mental_box_plot)




age_data$Age <- factor(age_data$Age) # turns the qualitative age ranges into ints (1, 2, 3, 4)

# Set global par settings for bold axes labels
par(font.lab = 2)  # 2 represents bold font

# Set global par settings for bold axes labels and adjust the margin
par(font.lab = 2, mar = c(5, 7, 4, 2) + 0.1)  # 2 represents bold font, adjust margin as needed

# Define the label text with line breaks
y_axis_label <- "Percentage of Canadians with\nFair or Poor Perceived Mental Health"

# Scatter plot
plot(Bad.mental ~ Age_num, data = age_data, 
     xaxt = "n",  # Gets rid of the x-axis labels so that we can use custom ones
     col = "black", 
     pch = 16, cex = 1,  # pch = 16 means filled dots; cex controls size
     xlab = "Age", 
     ylab = y_axis_label,  # Empty y-axis label
     main = "", 
     cex.axis = 1.1,
     cex.lab = 1.2,
     cex.main = 1.6)  # Size of the title
axis(1, at = unique(age_data$Age_num), labels = unique(age_data$Age), cex.axis = 1.1)  # Adding our custom x-axis labels
abline(bad_mental_lm, col = "red", lwd = 4)  # Adding the linear regression model to the graph
text(3.8, 15.5, bquote(R^2 ~ "=" ~ 0.888))  # Putting the R^2 value on the graph

# Reset the global par settings (optional)
par(font.lab = 1, mar = c(5, 4, 4, 2) + 0.1)  # 1 represents the default font, reset margin


# Plot for GAD
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
text(3.8, 24, bquote(R^2 ~ "=" ~ 0.852))
text(0.575, 0.05, "A", cex = 3.5, adj = c(0,0), xpd = TRUE)

# Plot for MDD
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
text(3.9, 31, bquote(R^2 ~ "=" ~ 0.861))
text(0.575, -0.25, "B", cex = 3.5, adj = c(0,0), xpd = TRUE)

# Reset the global par settings (optional)
par(font.lab = 1)  # 1 represents the default font