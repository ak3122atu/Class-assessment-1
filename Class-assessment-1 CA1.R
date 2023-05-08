#reading the sleep data and storing 
# it in a dataframe called sleep_data
sleep_data <- read.csv("sleep(1).csv")

#displaying the structure of sleep_data
str(sleep_data)

#stress level = dependent var
#sleeping hours = independent var

#examinig the correlation between the dataset using the default pairs
pairs(sleep_data, labels = colnames(sleep_data), main = "Sleep dataset correlation plot")

# labels starts with what is assigned to lower value first
# eg 0 = low/normal, 1 = medium low and so on
sleep_data$stress <- factor(sleep_data$sl, labels = c("low/normal", "medium low", 
                                                      "medium", "medium high", "high"
))
#displaying the sleep_data
sleep_data

#installing the psych package to improve the plot
install.packages("psych")
library(psych)

pairs.panels(sleep_data,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals


tapply(sleep_data$stress, median)
# summary data can be used to show this as well
# Shows the temperature details for no and yes answers
tapply(sr, stress, summary)

library(lattice)

#Q1

attach(sleep_data)
histogram(~sr | stress, 
          data = sleep_data, 
          main = "Distribution of Sleep data", 
          xlab = "Sleeping Hours", 
          ylab = "Stress level")
detach(sleep_data)

attach(sleep_data)
qqnorm(sr)
qqline(sr, col = "red")

with(sleep_data,(
  qqnorm(sr[stress == "low/normal"],
         main = "Low/normal stress Data",
         qqline(sr[stress == "low/normal"])
  )))

with(sleep_data,(
  qqnorm(sr[stress == "medium low"],
         main = "Medium low stress Data",
         qqline(sr[stress == "medium low"])
  )))

with(sleep_data,(
  qqnorm(sr[stress == "medium"],
         main = "Medium stress Data",
         qqline(sr[stress == "medium"])
  )))

with(sleep_data,(
  qqnorm(sr[stress == "medium high"],
         main = "Medium high stress Data",
         qqline(sr[stress == "medium high"])
  )))

with(sleep_data,(
  qqnorm(sr[stress == "high"],
         main = "High stress Data",
         qqline(sr[stress == "high"])
  )))

par(opar)

#shapiro-wilks test for normality
#p-value indicates if sample is from a normal distribution
#< 0.5 then not normally distributed

normality_test <- shapiro.test(sleep_data$sr)
normality_test$p.value

normality_test <- shapiro.test(sleep_data$sl)
normality_test$p.value

tapply(sr, stress, shapiro.test)

#sr (dependent var) = not normally distributed
#stress (independent var) = not normally distributed 

#format of the test is Kruskal Wallis test
kruskal.test(sr, stress)

#p value < 0.05 so the null hypothesis is rejected 
#and it is concluded that the stress leel is affected by sleeping hours
#p - value < 0.05

#q2

attach(sleep_data)
histogram(~lm | stress, 
          data = sleep_data, 
          main = "Distribution of Sleep data", 
          xlab = "limb movements", 
          ylab = "Stress level")
detach(sleep_data)

attach(sleep_data)
qqnorm(lm)
qqline(lm, col = "red")

with(sleep_data,(
  qqnorm(lm[stress == "low/normal"],
         main = "Low/normal stress Data",
         qqline(lm[stress == "low/normal"])
  )))

with(sleep_data,(
  qqnorm(lm[stress == "medium low"],
         main = "Medium low stress Data",
         qqline(lm[stress == "medium low"])
  )))

with(sleep_data,(
  qqnorm(lm[stress == "medium"],
         main = "Medium stress Data",
         qqline(lm[stress == "medium"])
  )))

with(sleep_data,(
  qqnorm(lm[stress == "medium high"],
         main = "Medium high stress Data",
         qqline(lm[stress == "medium high"])
  )))

with(sleep_data,(
  qqnorm(lm[stress == "high"],
         main = "High stress Data",
         qqline(lm[stress == "high"])
  )))

par(opar)

#shapiro-wilks test for normality
#p-value indicates if sample is from a normal distribution
#< 0.5 then not normally distributed

normality_test <- shapiro.test(sleep_data$lm)
normality_test$p.value

normality_test <- shapiro.test(sleep_data$sl)
normality_test$p.value

tapply(sr, stress, shapiro.test)

#lm (dependent var) = not normally distributed
#stress (independent var) = not normally distributed 

#format of the test is Kruskal Wallis test
kruskal.test(lm, stress)

#p value < 0.05 so the null hypothesis is rejected 
#and it is concluded that the stress level is affected by limb movements
#p - value < 0.05

#q3
attach(sleep_data)
histogram(bo, 
          data = sleep_data, 
          main = "Distribution of Blood Oxygen data", 
          xlab = "Blood Oxygen", 
          ylab = "Respiration rate")
detach(sleep_data)

attach(sleep_data)
histogram(rr, 
          data = sleep_data, 
          main = "Distribution of Respiration rate data", 
          xlab = "Blood Oxygen", 
          ylab = "Respiration rate")
detach(sleep_data)

attach(sleep_data)
qqnorm(bo, main = "QQ Plot for BO")
qqline(bo, col = "red")
detach(sleep_data)

attach(sleep_data)
qqnorm(rr, main = "QQ Plot for RR")
qqline(rr, col = "red")
detach(sleep_data)
par(opar)
#shapiro-wilks test 
# p value < 0.05 not normally distributed 
# p value > 0.05 normally distributed 

normality_test <- shapiro.test(sleep_data$bo)
normality_test$p.value

normality_test <- shapiro.test(sleep_data$rr)
normality_test$p.value

#spearman test 
cor.test(bo, rr, method = "spearman")


#Q4
attach(sleep_data)
histogram(~rem | stress, 
          data = sleep_data, 
          main = "Distribution of Sleep data", 
          xlab = "Rapid eye Movement ", 
          ylab = "Stress level")
detach(sleep_data)

attach(sleep_data)
qqnorm(rem)
qqline(rem, col = "red")

with(sleep_data,(
  qqnorm(rem[stress == "low/normal"],
         main = "Low/normal stress Data",
         qqline(rem[stress == "low/normal"])
  )))

with(sleep_data,(
  qqnorm(rem[stress == "medium low"],
         main = "Medium low stress Data",
         qqline(rem[stress == "medium low"])
  )))

with(sleep_data,(
  qqnorm(rem[stress == "medium"],
         main = "Medium stress Data",
         qqline(rem[stress == "medium"])
  )))

with(sleep_data,(
  qqnorm(rem[stress == "medium high"],
         main = "Medium high stress Data",
         qqline(rem[stress == "medium high"])
  )))

with(sleep_data,(
  qqnorm(rem[stress == "high"],
         main = "High stress Data",
         qqline(rem[stress == "high"])
  )))

par(opar)

#shapiro-wilks test for normality
#p-value indicates if sample is from a normal distribution
#< 0.5 then not normally distributed

normality_test <- shapiro.test(sleep_data$rem)
normality_test$p.value

normality_test <- shapiro.test(sleep_data$sl)
normality_test$p.value

tapply(rem, stress, shapiro.test)

#sl (dependent var) = not normally distributed
#rem (independent var) = not normally distributed 

#format of the test is Kruskal Wallis test
kruskal.test(rem, stress)

#Q5
attach(sleep_data)
histogram(bo, 
          data = sleep_data, 
          main = "Distribution of Blood Oxygen data", 
          xlab = "Blood Oxygen", 
          ylab = "Sleeping hours")
detach(sleep_data)

attach(sleep_data)
histogram(sr, 
          data = sleep_data, 
          main = "Distribution of Sleeping hours data", 
          xlab = "Blood Oxygen", 
          ylab = "Sleeping hours")
detach(sleep_data)

attach(sleep_data)
qqnorm(bo, main = "QQ Plot for BO")
qqline(bo, col = "red")
detach(sleep_data)

attach(sleep_data)
qqnorm(sr, main = "QQ Plot for SR")
qqline(sr, col = "red")
detach(sleep_data)
par(opar)
#shapiro-wilks test 
# p value < 0.05 not normally distributed 
# p value > 0.05 normally distributed 

normality_test <- shapiro.test(sleep_data$bo)
normality_test$p.value

normality_test <- shapiro.test(sleep_data$sr)
normality_test$p.value

#spearman test 
cor.test(bo, sr, method = "spearman")

