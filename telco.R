library(ggplot2)
library(patchwork)
library(tidyverse)
library(reshape2)

dataB <- read.csv("data/Company B - Data.csv")
states <- read.csv("data/States.csv", fileEncoding="UTF-8-BOM")

dataB <- dataB %>%
  mutate(International.plan=ifelse(International.plan=="Yes", 1, 0),
         Voice.mail.plan=ifelse(Voice.mail.plan=="Yes", 1, 0))

dataB %>%
  group_by(Churn) %>%
  summarise(count=n()) %>%
  ggplot() + 
  geom_col(aes(Churn, count, fill=Churn)) # 483 churned and 2850 not churned 

# correlations
library(corrplot)
correlations <- cor(dataB[,-1])
corrplot(correlations)

# by area code
dataB %>%
  group_by(Area.code, Churn) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from = Churn, values_from = count) %>%
  rename(
    'Churned' = 'TRUE',
    'Not_churned' = 'FALSE'
  ) %>%
  mutate(Churn.Rate = Churned / (Churned + Not_churned)) %>%
  arrange(Churn.Rate) %>%
  ggplot() + 
  geom_col(aes(reorder(Area.code, -Churn.Rate, sum), Churn.Rate, fill=Churn.Rate)) +
  xlab("Area Code") + ylab("Churn Ratio") +
  ggtitle("Churn Ratio by Area Code")

# vmail messages
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Number.vmail.messages))
dataB %>%
  group_by(Churn) %>%
  summarise(Number.vmail.messages=mean(Number.vmail.messages)) %>% 
  ggplot() +
  geom_col(aes(Churn, Number.vmail.messages, fill=Churn)) 
dataB %>%
  group_by(Account.length) %>%
  summarise(Churn.Rate=mean(Churn)) %>% 
  ggplot() +
  geom_col(aes(Account.length, Churn.Rate)) ###### interesting

# account length
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Account.length))
dataB %>%
  ggplot() +
  geom_density(aes(Account.length, fill=Churn, alpha=0.3)) # account length by churn
dataB %>%
  group_by(State) %>%
  summarise(Account.length=mean(Account.length)) %>%
  ggplot() +
  geom_col(aes(State, Account.length, fill=State, alpha=0.3)) # account length by state

# service calls
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Customer.service.calls)) ###### interesting
dataB %>%
  group_by(Churn) %>%
  summarise(Customer.service.calls=mean(Customer.service.calls)) %>% 
  ggplot() +
  geom_col(aes(Churn, Customer.service.calls)) ###### interesting
dataB %>%
  group_by(Customer.service.calls) %>%
  summarise('Not Churned'=1-mean(Churn), Churned=mean(Churn)) %>% 
  melt(id.var="Customer.service.calls", value.name = "Rate") %>% 
  ggplot() +
  geom_bar(aes(Customer.service.calls, Rate, fill=variable), stat="identity") +
  scale_fill_manual(values=c("#00BFC4", "#F8766D")) + 
  theme(legend.title = element_blank()) +
  ylab("Proportion of Customers") +
  scale_x_discrete(name ="Customer Service Calls", limits=c(0,1,2,3,4,5,6,7,8,9)) +
  ggtitle("Proportion of Customers Churning based on Number of Service Calls") ###### interesting

# day calls
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Total.day.minutes)) ###### interesting
dataB %>%
  ggplot() +
  geom_density(aes(Total.day.minutes, fill=Churn, alpha=0.3)) +
  xlab("Total Day Minutes") +
  ggtitle("Distribution of Total Day Minutes of Customers by Churn")
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Total.day.calls))
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Total.day.charge)) ###### interesting
dataB %>%
  ggplot() +
  geom_density(aes(Total.day.charge, fill=Churn, alpha=0.3)) +
  xlab("Total Day Charge") +
  ggtitle("Distribution of Total Day Charge of Customers by Churn")

# evening calls
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Total.eve.minutes))
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Total.eve.calls))
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Total.eve.charge))

# night calls
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Total.night.minutes))
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Total.night.calls))
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Total.night.charge))

# international calls
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Total.intl.minutes))
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Total.intl.calls))
dataB %>%
  ggplot() +
  geom_boxplot(aes(Churn, Total.intl.charge))

# by international plan - interesting
dataB %>%
  group_by(International.plan, Churn) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from = Churn, values_from = count) %>%
  rename(
    'Churned' = 'TRUE',
    'Not_churned' = 'FALSE'
  ) %>%
  mutate(Churn.Rate = Churned / (Churned + Not_churned)) %>%
  arrange(Churn.Rate) %>%
  ggplot() + 
  geom_col(aes(reorder(International.plan, -Churn.Rate, sum), Churn.Rate)) +
  xlab("International Plan") + ylab("Churn Ratio") +
  ggtitle("Churn Ratio by International Plan")

dataB %>%
  mutate(International.plan=ifelse(International.plan==1, "Yes", "No")) %>%
  group_by(International.plan) %>%
  summarise('Not Churned'=1-mean(Churn), Churned=mean(Churn)) %>% 
  melt(id.var="International.plan", value.name = "Rate") %>% 
  ggplot() +
  geom_bar(aes(International.plan, Rate, fill=variable), stat="identity") +
  scale_fill_manual(values=c("#00BFC4", "#F8766D")) + 
  theme(legend.title = element_blank()) +
  xlab("Has International Plan?") + ylab("Proportion of Customers") +
  ggtitle("Proportion of Customers Churning based on International Plan") ###### interesting

# by voice mail plan - interesting
dataB %>%
  group_by(Voice.mail.plan, Churn) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from = Churn, values_from = count) %>%
  rename(
    'Churned' = 'TRUE',
    'Not_churned' = 'FALSE'
  ) %>%
  mutate(Churn.Rate = Churned / (Churned + Not_churned)) %>%
  arrange(Churn.Rate) %>%
  ggplot() + 
  geom_col(aes(reorder(Voice.mail.plan, -Churn.Rate, sum), Churn.Rate)) +
  xlab("Voicemail Plan") + ylab("Churn Ratio") +
  ggtitle("Churn Ratio by Voicemail Plan")







# by state - interesting
# exclude minutes as it is essentially the same as charge
dataB %>%
  group_by(State) %>%
  summarise(Account.length=mean(Account.length),
            International.plan.rate=mean(International.plan),
            Voice.mail.plan.rate=mean(Voice.mail.plan),
            Number.vmail.messages=mean(Number.vmail.messages),
            Total.day.calls=mean(Total.day.calls),
            Total.day.charge=mean(Total.day.charge),
            Total.eve.calls=mean(Total.eve.calls),
            Total.eve.charge=mean(Total.eve.charge),
            Total.night.calls=mean(Total.night.calls),
            Total.night.charge=mean(Total.night.charge),
            Total.intl.calls=mean(Total.intl.calls),
            Total.intl.charge=mean(Total.intl.charge),
            Customer.service.calls=mean(Customer.service.calls),
            Churn.Rate = mean(Churn)) %>%
  full_join(states) %>%
  mutate(Land.Country.Border=ifelse(Land.Country.Border=="Yes", 1, 0)) -> dataB_states

# write.csv(dataB_states, "Company B by state.csv", row.names=FALSE)

dataB_states %>%
  ggplot() + 
  geom_col(aes(reorder(State, -Churn.Rate, sum), Churn.Rate, fill=Churn.Rate)) +
  xlab("State") + ylab("Churn Rate") +
  ggtitle("Churn Rate by State")

# correlations
correlations <- cor(dataB_states[,-1])
corrplot(correlations)
# total day charge, total intl charge, customer service calls,
# and foreign born percentage show highest correlations to churn rate; 
# account length and total intl calls show negative correlation instead

# compare to state foreign population
dataB_states %>%
  ggplot() + 
  geom_point(aes(Foreign.born.percentage, Churn.Rate)) +
  xlab("Foreign Born Percentage") + ylab("Churn Ratio") +
  ggtitle("Churn Ratio against Foreign Born Percentage by State")

# compare to state median household income
dataB_states %>%
  ggplot() + 
  geom_point(aes(Median.Household.Income, Churn.Rate)) +
  xlab("Median Household Incomee") + ylab("Churn Ratio") +
  ggtitle("Churn Ratio against Median Household Income by State")

# compare to state by country border
dataB_states %>%
  ggplot() + 
  geom_boxplot(aes(Land.Country.Border, Churn.Rate)) +
  xlab("Has Land Country Border") + ylab("Churn Ratio") +
  ggtitle("Churn Ratio by State")
dataB_states %>%
  group_by(Land.Country.Border) %>%
  summarise(Churn.Rate=mean(Churn.Rate)) %>% 
  ggplot() + 
  geom_col(aes(Land.Country.Border, Churn.Rate)) +
  xlab("Has Land Country Border") + ylab("Churn Ratio") +
  ggtitle("Mean Churn Ratio by State")






## Modelling by state

# leaps
# leaps and bounds to select best features
library(leaps)
# excluding new features
#best_subset <- leaps(x = dataB_states[, c(2:14)], y = dataB_states$Churn.Rate, 
#                     nbest = 3, method = "adjr2",
#                     names = colnames(dataB_states[, c(2:14)]))

# with new features
best_subset <- leaps(x = dataB_states[, c(2:14, 16:18)], y = dataB_states$Churn.Rate, 
                     nbest = 3, method = "adjr2",
                     names = colnames(dataB_states[, c(2:14, 16:18)]))

# create dataframe
data.frame(Size = best_subset$size, AdjR2 = round(best_subset$adjr2, 3),
           best_subset$which, row.names = NULL)

# plot
plot(best_subset$size, best_subset$adjr2,
     ylab="Adjusted R-squared",
     xlab="Number of variables (including intercept)")

# suggests best model has 10 features + intercept:
# Account.length, Total.day.charge, Total.eve.calls, Total.eve.charge,
# Total.intl.calls, Total.intl.charge, Customer.service.calls,
# Foreign.born.percentage, Median.Household.Income, Land.Country.Border

# Excluding new ones, best model has 7 features + intercept:
# Account.length, International.plan.rate, Total.day.charge, Total.intl.calls, Customer.service.calls,
# Total.intl.charge, Total.eve.charge


# create linear model
model <- lm(Churn.Rate ~ Account.length + Total.day.charge + Total.eve.calls +
              Total.eve.charge + Total.intl.calls + Total.intl.charge + 
              Customer.service.calls + Foreign.born.percentage +
              Median.Household.Income + Land.Country.Border, data=dataB_states)
summary(model) 
  # For 10 features: R-squared of 0.48 and adjusted R-squared of 0.35 - explains about 48% of variability in churn rate
# best models without the new features only resulted in adjusted R-squared of 0.22 and 0.19 and higher p-values


# create dfs with predicted and real values for comparison
prediction_df <- data.frame(State=dataB_states$State, Real=dataB_states$Churn.Rate, Predicted=predict(model))
prediction_df2 <- pivot_longer(prediction_df, cols=c('Real', 'Predicted'), names_to='Type', values_to="Rate")

# plot model predictions
ggplot(prediction_df2) + 
  geom_bar(aes(State, Rate, fill=Type), stat = 'identity', position = 'dodge') +
  xlab("State") + ylab("Churn Rate") + ggtitle("Model Prediction of Churn Rate vs Real Values")

# RMSE
sqrt(mean((prediction_df$Real-prediction_df$Predicted)^2))

# foreign born percent seems to have a noticeable influence, and 
# join new features only instead of states as potential predictors to normal dataset
dataB %>%
  full_join(dataB_states[, c(1, 16:18)], by="State") -> new_dataB

# write.csv(new_dataB, "Company B - Data Expanded.csv", row.names=FALSE)
