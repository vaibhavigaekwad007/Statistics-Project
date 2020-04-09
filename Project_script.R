#authors: Zechen Li, Vaibhavi Gaekwad

# Survey Data

data <- data.frame(read.csv(file = './data.csv', header = T))
summary(data)
head(data)

# Dataset Graphs

## Graph 1: Relationship between Mother's height & Child's height among different Maternal Age

library(dplyr)
useful_data <- data %>%
  mutate(height_difference = child_height - mother_height) %>%
  filter(milk == 'Yes' | milk == 'No', 
         exercise == 'Yes' | exercise == 'No', 
         maternal_age != "I don't know")
summary(useful_data)

library(ggplot2)
ggplot(useful_data, aes(x= mother_height , 
                        y=child_height, 
                        color = gender, 
                        size = height_difference)) + 
  facet_wrap(~maternal_age) +
  geom_point(alpha = 0.7) +
  scale_size_area(breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8),
                  max_size = 8) +
  labs(x = "Mother's Height", 
       y = "Child's Height", 
       size = "Height Difference", 
       color = 'Gender') +
  ggtitle("The Relationship between Mother's Height 
          and Child's Height among Different Mother's Maternal Age")



## Graph 2: Differences of Milk and Exercise on height difference based on maternal age 

explanatoryData <- useful_data %>%
  filter(maternal_age == '20-25 years' | maternal_age == '26-30 years')
ggplot(explanatoryData,  aes(height_difference))+
  facet_grid(exercise~milk,labeller = label_both) +
  geom_histogram(aes(fill = maternal_age), bins=30) +
  xlab('The Height Difference Between The Child and Mother') +
  ggtitle("Representation of height difference & 
          effect of milk and exercise on two groups")



# Box-plot


ggplot(useful_data, aes(x=maternal_age, y=height_difference)) + 
  geom_boxplot(aes(fill= maternal_age))  +
  labs(title="Plot of Height Difference among Different Mother's Maternal Age",
       x="Mother's Maternal Age", 
       y = "Height Difference (feets)")



# Whole Population Q-Q Plot

qqnorm(useful_data$height_difference, col = "darkred", main = "Normal Q-Q Plot")
qqline(useful_data$height_difference, col = "darkblue", lwd = 3)


# Histogram of Sampling Distribution

require(mosaic)
set.seed(1)
group20to25 <- useful_data %>%
  filter(maternal_age == '20-25 years')
group20to25_1000 <- do(1000) * mean(sample(group20to25$height_difference,15))
hist(group20to25_1000$mean, 
     main = "Sampling Distribution with Size = 15 and Simulations = 1000", 
     xlab = "Mean of Height Difference of Maternal Age Group 20 to 25 Years Old", 
     prob = T, 
     col = "darkred")
lines(density(group20to25_1000$mean),
      col = "darkblue", 
      lwd = 2)

group26to30 <- useful_data %>%
  filter(maternal_age == '26-30 years')
group26to30_1000 <- do(1000) * mean(sample(group26to30$height_difference,15))
hist(group26to30_1000$mean,
     main = "Sampling Distribution with Size = 15 and Simulations = 1000", 
     xlab = "Mean of Height Difference of Maternal Age Group 26 to 30 Years Old", 
     prob = T,
     col = "darkred")
lines(density(group26to30_1000$mean), 
      col = "darkblue",
      lwd = 2)

twoGroup_1000 <- do(1000) *mean(sample(group26to30$height_difference,15) - 
                                  sample(group20to25$height_difference,15))
hist(twoGroup_1000$mean, 
     main = "Sampling Distribution", 
     xlab = "Mean Difference Between Above Two Groups",
     prob = T, 
     col = "darkred")
lines(density(twoGroup_1000$mean), 
      col = "darkblue", 
      lwd = 2)


# Two Samples Q-Q Plot Graphs

group20to25 <- useful_data %>%
  filter(maternal_age == '20-25 years')
qqnorm(group20to25$height_difference,
       main = "Normal Q-Q Plot of Maternal Age Group 20 to 25 Years Old")
qqline(group20to25$height_difference, 
       col = "darkblue", 
       lwd = 3)
summary(group20to25)

group26to30 <- useful_data %>%
  filter(maternal_age == '26-30 years')
qqnorm(group26to30$height_difference,
       main = "Normal Q-Q Plot of Maternal Age Group 26 to 30 Years Old")
qqline(group26to30$height_difference, 
       col = "darkblue",
       lwd = 3)
summary(group26to30)


# Two Samples T-Test


t.test(group20to25$height_difference, group26to30$height_difference, var.equal = F)


# sample means
x_bar_20to25 <- mean(group20to25$height_difference)
x_bar_26to30 <- mean(group26to30$height_difference)

# null hypothesized population mean difference between the two groups
mu_0 <- 0

# sample variances
s_20to25_sq <- sd(group20to25$height_difference) ** 2
s_26to30_sq <- sd(group26to30$height_difference) ** 2

# sample size
n_20to25 <- length(group20to25$height_difference)
n_26to30 <- length(group26to30$height_difference)

# t-test test statistic
t <- (x_bar_20to25 - x_bar_26to30 - mu_0)/sqrt((s_20to25_sq/n_20to25) + 
                                                 (s_26to30_sq/n_26to30))

# one sided upper p-value
two_sided_diff_t_pval <- pt(q = t, df = min(n_20to25, n_26to30)-1, lower.tail = TRUE)*2
two_sided_diff_t_pval



