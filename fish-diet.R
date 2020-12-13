#Libraries:

library(dplyr)
library(ggplot2)
library(corrplot)

#Importing the Fish Dataset:

fish_df<- read.csv(file.choose())

#Analysis start

head(fish_df)

str(fish_df)

summary(fish_df)

levels(fish_df$fish_in_diet) #shows the categories in the selected column
levels(fish_df$cancer) #shows the categories in the selected column

#Visualizing the data with the help of graphs
fish_df %>% 
  ggplot(aes(fish_in_diet)) +
  geom_bar()+ labs(title = "All Data - Count of Fish In Diet")

fish_df %>% 
  ggplot(aes(cancer)) + 
  geom_bar()+ labs(title = "All Data - Cancer Patients")

fish_df %>% filter(cancer == "Yes") %>% 
  ggplot(aes(fish_in_diet)) + geom_bar() +
  labs(title="Count of Fish In Diet (People with Cancer)")

fish_df %>% filter(cancer == "No") %>% 
  ggplot(aes(fish_in_diet)) + geom_bar() +
  labs(title="Count of Fish In Diet (People without Cancer)")


# Tables:

table(fish_df$fish_in_diet)
table(fish_df$cancer)
chi_table <- table(fish_df$cancer, fish_df$fish_in_diet)
chi_table


#Hypothesis:
# a. Null = Fish in Diet and Cancer are Independent
# b. Alternative = There is a relationship between Fish In Diet & Cancer

#Chisquare Test
chi_sqr <- chisq.test(chi_table)
chi_sqr


#since p-value (probability) is greater than 0.05 (0.2985 by chi_square test) we fail to reject
#null hypothesis (we accept it) and there is no relation between Fish in Diet and Cancer

#Visualizing the Residuals:

corrplot((chi_sqr$residuals), is.corr = F)
#Residuals = actual - predicted



