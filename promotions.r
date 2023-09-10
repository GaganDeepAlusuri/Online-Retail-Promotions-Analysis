rm(list=ls())
library(rio)
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(pscl)
library(MASS)
library(AER)
cust_data = import("OnlineRetailPromotions.xlsx")


str(cust_data)
summary(cust_data)
dim(cust_data)
cust_data$historysegment = as.factor(cust_data$historysegment)
cust_data$historysegment = relevel(cust_data$historysegment, "7) $1,000 +")
 
cust_data$channel = as.factor(cust_data$channel)
cust_data$campaign = as.factor(cust_data$campaign)
cust_data$campaign = relevel(cust_data$campaign, "No E-Mail")
cust_data$conversion = as.factor(cust_data$conversion)
cust_data$newcustomer = as.factor(cust_data$newcustomer)
cust_data$zipcode <- ifelse(cust_data$zipcode == "Surburban", "Urban", cust_data$zipcode)
cust_data$zipcode = as.factor(cust_data$zipcode)
cust_data$historycategory <- ifelse(cust_data$history > 160,1,0)
cust_data$historycategory <- as.factor(cust_data$history) 
#Variable to see if the customer recieved a promotional email or not
cust_data$email <- ifelse(cust_data$campaign == "No E-Mail", 0, 1)

#Create cat1, cat2 and cat3 based on last year's merch bought and kind of promotion sent.
cust_data <- cust_data %>% 
  mutate(cat = if_else(mens == 1 & womens == 0 & campaign == "Mens E-Mail", "cat1",
                       if_else(mens == 0 & womens == 1 & campaign == "Womens E-Mail", "cat2",
                               "cat3")))
cust_data$cat = as.factor(cust_data$cat)

# Check for missing values
sum(is.na(cust_data))



# Create frequency table for categorical variables
table(cust_data$historysegment, cust_data$channel, cust_data$campaign, cust_data$visit, cust_data$conversion)

# create histograms of the historysegment
cust_data %>% 
  ggplot(aes(x = historysegment)) +
  geom_bar()
# create histogram of recency
cust_data %>% 
  ggplot(aes(x = recency)) +
  geom_bar()

# create histogram of channels
cust_data %>% 
  ggplot(aes(x = channel)) +
  geom_bar()

# Create histogram for spend
ggplot(cust_data, aes(x = spend)) +
  geom_histogram(bins = 20)  +
  labs(x = "Spend", y = "Count")

#adjusting for limits 
ggplot(cust_data, aes(x = spend)) +
  geom_histogram(bins = 20)  + xlim(0, 500) + ylim(0,80) +
  labs(x = "Spend", y = "Count")

#Log of spend
ggplot(cust_data, aes(x = log(spend))) +
  geom_histogram(bins = 20)   +
  labs(x = "Spend", y = "Count")

boxplot(cust_data$spend) 

# Create a histogram of the spend variable
ggplot(cust_data, aes(x = spend))  +
  geom_histogram(binwidth = 5, color = "black", fill = "blue") +
  labs(x = "Spend", y = "Count") +
  ggtitle("Histogram of Customer Spend")

# Check for skewness
moments::skewness(cust_data$spend)

# Apply log transformation to the spend variable
cust_data$log_spend <- log(cust_data$spend + 1)
# Create a histogram of the log-transformed spend variable
ggplot(cust_data, aes(x = log_spend)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "blue") +
  labs(x = "Log(Spend + 1)", y = "Count") +
  ggtitle("Histogram of Log-Transformed Customer Spend")
# Check for normality
qqnorm(cust_data$log_spend)
qqline(cust_data$log_spend)

#Draw the corrplot
numeric_cols <- cust_data %>%
  select_if(is.numeric)
corr_matrix <- cor(numeric_cols)
# Plot the correlation matrix
corrplot(corr_matrix, type="upper", method="number", order="hclust")


#Q) How did the promotion campaigns work relative to the control group? 
#Did the men's promotions work better than the women's promotion (or vice versa) and by how much?

# Calculate average spend for each group
avg_spend = cust_data %>%
  group_by(campaign) %>%
  summarise(avg_spend = mean(spend))

# Calculate difference in average spend between each campaign group and control group
control_spend = subset(cust_data, campaign == "No E-Mail")$spend
men_spend = subset(cust_data, campaign == "Mens E-Mail")$spend
women_spend <- subset(cust_data, campaign == "Womens E-Mail")$spend

men_diff  =  mean(men_spend) - mean(control_spend)
women_diff = mean(women_spend) - mean(control_spend)

# Conduct t-tests
t.test(men_spend, control_spend) 
# This indicates that, on average, customers who received the men's promotion spent $0.77 more than those who did not receive any promotion.
t.test(women_spend, control_spend)
# This indicates that, on average, customers who received the women's promotion spent between $0.17 and $0.68 more than those who did not receive any promotion.

t.test(men_spend, women_spend)
#on average, men who received the men's promotion spent between $0.03 and $0.66 more than women who received the women's promotion.

#Q) Should we target these promotions to new customers (who joined over the last 12 months) rather than to established customers, or vice versa?

# Subset the cust_data to only include customers who received an email
email_cust_data <- cust_data[cust_data$email== 1,]

# Subset the cust_data to only include customers who received an email and are new or old

new_cust_spend  = email_cust_data[email_cust_data$newcustomer == 1,]$spend
old_cust_spend  = email_cust_data[email_cust_data$newcustomer == 0,]$spend

# Perform a two-sample t-test
t.test(new_cust_spend, old_cust_spend)

#we fail to reject the null hypothesis and conclude that there is not enough evidence to suggest that there is a statistically significant difference in mean spend between new and established customers who received an email.


#Q) Should we target these promotions to customers who have a higher (or lower) history of spending over the last year?

summary(email_cust_data$history)
hist(email_cust_data$history)
# Divide the customers into two groups based on their median history of spending in the past year
high_history_spend <- email_cust_data[email_cust_data$history > median(email_cust_data$history),]$spend
low_history_spend <- email_cust_data[email_cust_data$history <= median(email_cust_data$history),]$spend


t.test(low_history_spend, high_history_spend)
#Therefore, based on this analysis, it may be more effective to target promotions to customers with a higher history of spending in the past year.

#Q) Did the promotions work better for phone or web channel?
phone_spend <- email_cust_data[email_cust_data$channel == "Phone",]$spend
web_spend <- email_cust_data[email_cust_data$channel == "Web",]$spend

t.test(phone_spend, web_spend)
#we cannot conclude whether the promotions worked better for phone or web channel based on this t-test.

#Q) Will the promotions work better if the men's promotion is targeted at customers 
#who bought men's merchandise over the last year (compared to those who purchased women's merchandise), 
#and if the women's promotion would work better if targeted at customers who bought women's merchandise over the last year? 

# Step 1: Create four groups based on purchase history
M <- subset(cust_data, mens == 1 & womens == 0)
W <- subset(cust_data, mens == 0 & womens == 1)
B <- subset(cust_data, mens == 1 & womens == 1)
N <- subset(cust_data, mens == 0 & womens == 0)

# Step 2: Compute conversion rates for each group and promotion
M_mens <- sum(M$conversion[M$campaign == "Mens E-Mail"]) / sum(M$visit[M$campaign == "Mens E-Mail"])
M_womens <- sum(M$conversion[M$campaign == "Womens E-Mail"]) / sum(M$visit[M$campaign == "Womens E-Mail"])
W_mens <- sum(W$conversion[W$campaign == "Mens E-Mail"]) / sum(W$visit[W$campaign == "Mens E-Mail"])
W_womens <- sum(W$conversion[W$campaign == "Womens E-Mail"]) / sum(W$visit[W$campaign == "Womens E-Mail"])

# Step 3: Perform chi-square test for Men's promotion
M_expected <- c(sum(M$visit[M$campaign == "Mens E-Mail"]), sum(M$visit[M$campaign != "Mens E-Mail"]))
M_actual <- c(sum(M$conversion[M$campaign == "Mens E-Mail"]), sum(M$conversion[M$campaign != "Mens E-Mail"]))
M_chisq <- chisq.test(M_actual, p = M_expected / sum(M_expected))
M_chisq

# Step 4: Perform chi-square test for Women's promotion
W_expected <- c(sum(W$visit[W$campaign == "Womens E-Mail"]), sum(W$visit[W$campaign != "Womens E-Mail"]))
W_actual <- c(sum(W$conversion[W$campaign == "Womens E-Mail"]), sum(W$conversion[W$campaign != "Womens E-Mail"]))
W_chisq <- chisq.test(W_actual, p = W_expected / sum(W_expected))
W_chisq
# Step 5: Perform t-tests for each promotion
# Men's promotion
M_mens_spent <- M$spend[M$campaign == "Mens E-Mail"]
M_womens_spent <- M$spend[M$campaign == "Womens E-Mail"]
M_ttest <- t.test(M_mens_spent, M_womens_spent)
M_ttest

# Women's promotion
W_mens_spent <- W$spend[W$campaign == "Mens E-Mail"]
W_womens_spent <- W$spend[W$campaign == "Womens E-Mail"]
W_ttest <- t.test(W_womens_spent, W_mens_spent)
W_ttest

#https://data.library.virginia.edu/getting-started-with-hurdle-models/


#Model 1 : Poisson

pois_model <- glm(spend ~ recency  + historysegment + history + zipcode + campaign + newcustomer + visit + conversion, data = cust_data, family = "poisson")
summary(pois_model)
#DO quassi
"# predict expected mean count
mu <- predict(mod1, type = "response")
# sum the probabilities of a 0 count for each mean
exp <- sum(dpois(x = 0, lambda = mu))
# predicted number of 0's
round(exp) 
# observed number of 0's
sum(cust_data$spend < 1)                      
"
#Problem 1- Overdispersion
library(AER)
dispersiontest(pois_model) # estimated overdispersion parameter > 1 hence overdispersion.
#Problem 2 - Excess zeroes
#Zero-inflation: If there are excess zeros in the response variable, a Poisson model may not be appropriate. This can lead to biased estimates and model misspecification.



#M1
qpoisson <- glm(spend ~ recency + historycategory + zipcode  + campaign + newcustomer + visit + conversion + cat , data = cust_data, family = quasipoisson (link=log))
summary(qpoisson)


"nb = glm.nb(spend ~ recency  + history + zipcode + campaign + newcustomer + visit + conversion, data = cust_data)
summary(nb)
model_nb2 = glm.nb(spend ~ recency  + historysegment + history + zipcode + campaign + newcustomer + visit + conversion, data = cust_data)"

#change y to whole nums
cust_data2 = cust_data

cust_data2[, c("spend")] <- round(cust_data2[, c("spend")],0) 
#M2
hpoisson <- hurdle(spend ~ recency + historycategory  + zipcode + newcustomer + channel
                   + campaign + cat |  visit + conversion, data=cust_data2, link= 'logit',dist="poisson")
summary(hpoisson)

#M3
zip <- zeroinfl(spend ~ recency + historycategory + mens + womens + zipcode + newcustomer + channel
                + campaign + cat | visit + conversion, data=cust_data2, dist="poisson", link = "logit")
summary(zip)

library(stargazer)
stargazer(qpoisson, hpoisson, zip , type="text", single.row=TRUE, out="promotions.html")

#Compared to the control group, The customers who received a mens promotion spent 3.5% less.
#Compared to the control group, The customers who received a women's promotion spent 19% more.
#4. 1.With every unit increase in mens and womens promotion campaign, the women spent 22.2% more than the men.

#Compared to the existing customers, The new customers spent 1.1 % more. Yes.
#Compared to the high history customers, lower history customers tend to spend 14.5 % more. Hence targeting them would benefit.
#Customers using phone channel, tend to spend 0.2 % more compared to the web.

#Customers who bough a female merch last year and were given a female promotion, tend to spend 32.5 % less than the customers
#who bought a men merch last year and were give a men promotion. promotions work better if the men's promotion is targeted at customers who bought men's merchandise over the last year

#Assumptions
#1. Overdispersion
#2. Excess Zero
#3. Independence
#4. The Y is whole number.


car::durbinWatsonTest(poisson)
