
library(magrittr)
library(tidyverse)
library(mice)
library(zoo)
library(bestNormalize)
library(caret)
library(nnet)
library(stats)
library(MASS)
library(data.table)
library(dplyr)



# ### read the data
credit <- read.csv("C:/Users/Eren/Desktop/Ders/Stat412/train.csv")
head(credit,10)
# 
class(credit)
# ### it is a data frame so we will go with this
# 
dim(credit)
# ### we have 100000 observation and 28 variables in the credit data
# 
set.seed(412)
str(credit)
# ### age , annual income , num of loan, num of delayed payment, changed credit limit, outstanding debt, amount invested monthly, monthly balance are chr columns but they should be integer so we will change them.
# 
credit$Age <- as.numeric(credit$Age)
credit$Annual_Income <- as.numeric(credit$Annual_Income)
credit$Outstanding_Debt <- as.numeric(credit$Outstanding_Debt)
credit$Num_of_Loan <- as.numeric(credit$Num_of_Loan)
credit$Num_of_Delayed_Payment <- as.numeric(credit$Num_of_Delayed_Payment)
credit$Changed_Credit_Limit <- as.numeric(credit$Changed_Credit_Limit)
credit$Amount_invested_monthly <- as.numeric(credit$Amount_invested_monthly)
credit$Monthly_Balance <- as.numeric(credit$Monthly_Balance)


library(magrittr)
library(dplyr)
colnames(credit)
credit <- credit %>%
  mutate_at(c(5,8,13,16,17,20,25,27), as.numeric)
str(credit)
# 
# ### we change all columns we want our column names looks good maybe I might think the change all lower cases but it is preference
# 
summary(credit)
# ### we have outliers in age and so much missing values also, also num_bank_accounts and num_of_loan, delay from due date, num_of delayed_payment has negative values so we should fix them.
# 
library(tidyverse)
colnames(credit) <- str_to_lower(colnames(credit))
credit <- credit %>% 
  filter(abs(age) < 150)
credit <- credit %>%
  filter(abs(num_bank_accounts) < 20) %>%
  mutate(num_bank_accounts = abs(num_bank_accounts))
credit <- credit %>%
  mutate(num_of_loan = abs(num_of_loan)) %>%
  filter(num_of_loan < 25)
credit <- credit %>%
  mutate(delay_from_due_date = abs(delay_from_due_date))
credit <- credit %>%
  mutate(num_of_delayed_payment = abs(num_of_delayed_payment)) %>%
  filter(num_of_delayed_payment < 100)
credit <- credit %>% 
  mutate(changed_credit_limit = abs(changed_credit_limit))
credit <- credit[,-c(1,2,4,6,7)]
summary(credit)
# ### we fix the issues by throwing the some outliers but we have still so much outliers so we need to find that is there reasonable outputs or not.
# 
dim(credit)
credit_num <- credit[sapply(credit, is.numeric)]
credit_num_sc <- scale(credit_num)
for (i in colnames(credit_num_sc)) {
  boxplot(credit_num_sc[,i], main = i)
  }
# 
# 
# 
credit_ctg <- credit[sapply(credit, is.character)]
for (i in colnames(credit_ctg)) {
tab <- table(credit_ctg[i])
barplot(tab,main= i,
        ylab="Frequency", col= "red")
}


# ### I know all of them is not categorical but I want to see that which one I use as a categorical data and how many categories I have.
# And Also I see that same values doesn't make any sense so I need to fix that.
# ### I will delete the data which I don't need such as customer_id, name, ssn (social security number)
# 
# 
credit_ctg
which(credit_ctg$payment_of_min_amount == "NM")
which(credit_ctg$payment_behaviour == "!@9#%8")
# 
# ###payment of min amount is a binary operator and it is yes or no and I can't find the explanation for "NM" but it is too much so i can't delete them.
# ### Also in payment behaviour "!@9#%8" is a unique value too I don't know the meaning of it too but it is not an outlier since it repeats so much
# 
# ### After this steps I will deal with missing values
# 
credit2 <- as.data.frame(bind_cols(credit_num,credit_ctg))
# ### I make the new data set with variables I use
# 
sum(is.na(credit2))
colSums(is.na(credit2))
# ### We have 34113 missing values :(  and most of missing values in monthly inhand salary so I should check do I need to delete the column or not
# 
credit_complete = credit2[complete.cases(credit2), ]   #known as complete case
cat("We have", dim(credit2),"dimension before cleaning the missing values", "if we clear all the rows with missing values we will have",dim(credit_complete))
# ### So we need to solve this issue
# 
colSums(is.na(credit2))/nrow(credit2)
# ### since no columns have missing values higher than 60% they all stay for now.
# ### since md.pattern looks like mess I will eleminate the variables for missing part
# 
library(mice)
credit_miss <- as.data.frame(credit2[,c(2,3,10,11,12,15,16,21)])
md.pattern(credit_miss,rotate.names = T)
# ## still It is not clear but we will solve this
# 
library(zoo)
credit_num <- as.data.frame(na.approx(credit_num))
sum(is.na(credit_num_sc))

# ### we deal with numerical na's by using na.approx from zoo package this package fill na's by interpolated values lets make a new data and check na's for categorical
# 
credit3 <- as.data.frame(bind_cols(credit_num,credit_ctg))
credit3 <- credit3[,-c(17)]
sum(is.na(credit3))
# 
colSums(is.na(credit3))
durations <- credit3$credit_history_age
convert_to_months <- function(duration) {
  if (is.na(duration)) {
    return(NA)
  }
  years <- as.numeric(sub("^(\\d+) Years.*$", "\\1", duration))
  months <- as.numeric(sub("^.* and (\\d+) Months$", "\\1", duration))
  total_months <- (years * 12) + months
  return(total_months)
}
total_months <- sapply(durations, convert_to_months)
total_months <- as.numeric(unlist(sapply(durations, convert_to_months)))
credit3$credit_history_age <- total_months
credit3_num <- credit3[sapply(credit3, is.numeric)]
credit3_num <- as.data.frame(na.approx(credit3_num))
credit4 <- as.data.frame(bind_cols(credit3_num,credit_ctg))
credit4 <- credit4[,-c(18,21)]
col_name <- colnames(credit4)
col_name[17] <- "credit_history_age"
colnames(credit4) <- col_name
sum(is.na(credit4))
credit4 <- na.omit(credit4)
# 
# ### missing values was in credit_history_age since it is a date I convert that to months so I use as a numeric then fill missing values with na.approx function
# 
# ### After deal with missing values I want to normalize my data I will do logistic regression and it doesn't have normality assumption but I have so much outliers and it is still have linearity with indep and log odds
# 
library(bestNormalize)
credit4_num <- credit4[sapply(credit4, is.numeric)]
credit4_norm_num <- credit4_num
for (i in colnames(credit4_norm_num)) {
  normal <- bestNormalize(credit4_norm_num[[i]], standardize = TRUE)
  credit4_norm_num[[i]] <- normal$x.t
}

# ### lets look at the boxplots
# 
for (i in colnames(credit4_norm_num)) {
  boxplot(credit4_norm_num[[i]], main = i)
}
# 
# ### They look better so finally lets make the last data.frame and continue with questions
# 
credit4_ctg <- credit4[sapply(credit4, is.character)]
credit5_norm <- as.data.frame(bind_cols(credit4_norm_num,credit4_ctg))
summary(credit5_norm)
# 
credit5_norm <- credit5_norm %>% 
  filter(abs(num_credit_card) < 150)
credit5_norm <- credit5_norm %>% 
  filter(abs(num_credit_inquiries) < 150)

# 
# 
# ###Q1) How does annual income influence the number of bank accounts?
q1 <- credit5_norm[,c(2,4,5)]
ggplot(q1, aes(x = annual_income)) + 
  geom_histogram(binwidth = 10000, fill = "blue", color = "black") +
  labs(title = "Distribution of Annual Income", x = "Annual Income", y = "Frequency")

ggplot(q1, aes(x = num_bank_accounts)) + 
  geom_bar(fill = "green", color = "black") +
  labs(title = "Distribution of Number of Bank Accounts", x = "Number of Bank Accounts", y = "Frequency")

q1$num_bank_accounts <- as.factor(q1$num_bank_accounts)


box_plot_bank_accounts <- ggplot(q1, aes(x = num_bank_accounts, y = annual_income, group = num_credit_card, fill = num_credit_card)) +
  geom_col(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Annual Income vs Number of Bank Accounts",
       x = "Number of Bank Accounts",
         y = "Annual Income")

# Print the box plot for Bank Accounts
print(box_plot_bank_accounts)

q1$num_bank_accounts <- as.numeric(q1$num_bank_accounts)
#Scatter plot for relationship visulization
ggplot(q1, aes(x = annual_income, y = num_bank_accounts)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") + 
  labs(title = "Annual Income vs. Number of Bank Accounts", x = "Annual Income", y = "Number of Bank Accounts")

# ### We have two left skewed data maybe we can apply transformation
# 
credit5_norm_num <- credit5_norm[sapply(credit5_norm, is.numeric)]
credit5_norm_num_sc <- as.data.frame(scale(credit5_norm_num))
#Calculate correlation coefficients
cor(credit5_norm_num_sc$annual_income, credit5_norm_num_sc$num_bank_accounts)
cor(credit5_norm_num_sc$annual_income, credit5_norm_num_sc$num_credit_card)
## We don't have multicollinearity since it is simple linear regression

# Linear regression
model_bank_accounts <- lm(annual_income ~ num_bank_accounts, data = credit5_norm_num_sc)
summary(model_bank_accounts)

# ### annual income looks significant for both number of credit card and number of bank accounts also residuals look good but our R-squared is very low so we could explain the variance of the data in number of credit card nearly 4% and in number of bank account 6%. So annual income has an effect but very little. Annual income has a negative coefficient for both model this means when annual income increase one unit number of credit cards decrease for their coefficient.
# 
# #Q2)How does the number of bank accounts influence the credit score across different age groups?
q2 <- credit4
q2 <- na.omit(q2)
q2 <- q2 %>%
  mutate(age_group = cut(age, breaks = c(14, 24, 34, 44, 54, 64, 74, 84, 94, 104),
                         labels = c('14-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-84', '85-94', '95+')))

ggplot(q2, aes(x = credit_score, y = num_bank_accounts, fill = age_group)) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge()) +
  labs(title = "Average Credit Score by Number of Bank Accounts and Age Group",
       x ="Average Credit Score" , y = "Number of Bank Accounts") +
  theme_minimal()
levels(q2$credit_score)

### Encode credit_score with label encoding
library(caret)
library(nnet)
credit5_norm$credit_score <- factor(credit5_norm$credit_score, levels = c('Poor', 'Standard', 'Good'), labels = c(1, 2, 3))
credit5_norm$credit_score <- as.numeric(as.character(credit5_norm$credit_score))
# 
# 
library(nnet)
model_credit_score <- multinom(credit_score ~ interest_rate + outstanding_debt, data = credit5_norm)
summary(model_credit_score)
coefficients <- summary(model_credit_score)$coefficients
print(coefficients)
# Calculate odds ratios
odds_ratios <- exp(coefficients)
print(odds_ratios)
# Calculate confidence intervals for the coefficients
standard_errors <- summary(model_credit_score)$standard.errors
z <- coefficients / standard_errors
p_values <- (1 - pnorm(abs(z), 0, 1)) * 2
# Confidence intervals
conf_int_lower <- exp(coefficients - 1.96 * standard_errors)
conf_int_upper <- exp(coefficients + 1.96 * standard_errors)
# Display odds ratios with confidence intervals
odds_ratios_ci <- data.frame(
  OR = odds_ratios,
  CI_lower = conf_int_lower,
  CI_upper = conf_int_upper,
  p_values = p_values
)
print(odds_ratios_ci)
# ## Intercept represent "Poor".First look coefficents For a one-unit increase in the interest rate, the log-odds of having a "Standard" credit score (versus "Poor") decrease by 0.3621638.For a one-unit increase in outstanding debt, the log-odds of having a "Standard" credit score (versus "Poor") decrease by 0.6468804.Similarly in the Good vs Poor we have a negative coef.For a one-unit increase in the interest rate, the log-odds of having a "Good" credit score (versus "Poor") decrease by 1.1864338.For a one-unit increase in outstanding debt, the log-odds of having a "Good" credit score (versus "Poor") decrease by 0.8583294.When we get the odds ratios. For a one-unit increase in the interest rate, the odds of having a "Standard" credit score (versus "Poor") are multiplied by approximately 0.696 (a decrease in odds).For a one-unit increase in outstanding debt, the odds of having a "Standard" credit score (versus "Poor") are multiplied by approximately 0.524 (a decrease in odds). For a one-unit increase in the interest rate, the odds of having a "Good" credit score (versus "Poor") are multiplied by approximately 0.305 (a decrease in odds).For a one-unit increase in outstanding debt, the odds of having a "Good" credit score (versus "Poor") are multiplied by approximately 0.424 (a decrease in odds). 
# ## Both interest_rate and outstanding_debt have a negative impact on the likelihood of having a "Standard" or "Good" credit score compared to a "Poor" credit score. This is evidenced by the odds ratios being less than 1 for both predictors in both comparisons, indicating a decrease in the odds of having a better credit score with higher interest rates and outstanding debts.
# ## Both of them are significant since their p-values < 0.05 and since CI's not containing 1 one unit increase has an effect. For every one unit increase in interest rate; The odds of having a "Standard" credit score versus a "Poor" credit score decrease by approximately 30.38%.The odds of having a "Good" credit score versus a "Poor" credit score decrease by approximately 69.47%.For every one-unit increase in outstanding debt; The odds of having a "Standard" credit score versus a "Poor" credit score decrease by approximately 47.63%.The odds of having a "Good" credit score versus a "Poor" credit score decrease by approximately 57.61%.
# 
# #Q3)Is there a significant association between the type of loan (e.g., mortgage, car loan, personal loan) and the frequency of late payments?
# 
str(credit5_norm)
contingency_table <- table(credit5_norm$type_of_loan, credit5_norm$num_of_delayed_payment)

# Chi-square test
chi_sq_test <- chisq.test(contingency_table)
print(chi_sq_test)

# Calculate Cramer's V 
cramers_v <- sqrt(chi_sq_test$statistic / sum(contingency_table) * (min(nrow(contingency_table), ncol(contingency_table)) - 1))
print(cramers_v)
# Since Cramer's V = 17.11 type of loan and the frequency of delayed payments has a strong association. This shows that type of loan has a significant impact on the frequency of delayed payments.
# 
# Q4) How does the monthly in-hand salary influence the amount invested monthly?
library(ggmosaic)
str(credit5_norm)
q4 <- credit5_norm
mis_norm <- bestNormalize(q4$monthly_inhand_salary)
q4$monthly_inhand_salary <- mis_norm$x.t
mis_norm2 <- bestNormalize(q4$amount_invested_monthly)
q4$amount_invested_monthly <- mis_norm2$x.t
ggplot(q4,aes(x = monthly_inhand_salary))+
  geom_density()

ggplot(q4,aes(x = amount_invested_monthly))+
  geom_density()

ggplot(q4, aes(x = monthly_inhand_salary, y = amount_invested_monthly)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +  # Add linear regression line
  labs(title = "Relationship between Monthly Inhand Salary and Amount invested monthly",
       x = "Monthly Inhand Salary",
       y = "Amount invested monthly") +
  theme_minimal()

# Q5)How does age influence the average number of days delayed from the payment due date?
data <- q4  # Replace with your actual dataframe

# Ensure the relevant columns are factors
data$credit_score <- as.factor(data$credit_score)
data$credit_mix <- as.factor(data$credit_mix)
data$payment_of_min_amount <- as.factor(data$payment_of_min_amount)

# Create Mosaic Plot
ggplot(data = data) +
  geom_mosaic(aes(x = product(credit_score, credit_mix), fill = payment_of_min_amount)) +
  labs(title = "Relationship between Credit Score, Type of Loan, and Payment of Minimum Amount",
       x = "Credit Score and payment behaviour",
       y = "Count",
       fill = "Payment of Minimum Amount") +
  theme_minimal()





correlation <- cor(credit5_norm$age, credit5_norm$delay_from_due_date, method = "pearson")

# Print correlation coefficient
cat("Pearson's correlation coefficient:", correlation, "\n")

# Perform correlation test (optional)
cor.test(credit5_norm$age, credit5_norm$delay_from_due_date)

# Scatter plot with correlation coefficient
ggplot(credit5_norm, aes(x = age, y = delay_from_due_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  annotate("text", x = min(credit5_norm$age) + 5, y = max(credit5_norm$Delay_from_due_date) - 5,
           label = paste("Correlation:", round(correlation, 2)),
           color = "black", size = 4) +
  labs(title = "Relationship between Delay from Due Date and Age",
       x = "Age",
       y = "Delay from Due Date (days)") +
  theme_minimal()
# 
# credit mix encoding
credit5_norm$credit_mix[credit5_norm$credit_mix == "_"] <- "undefined"
credit5_norm$credit_mix <- factor(credit5_norm$credit_mix, levels = c("undefined",'Bad', 'Standard', 'Good'), labels = c(0, 1, 2, 3))
credit5_norm$credit_mix <- as.numeric(as.character(credit5_norm$credit_mix))
# type of loan encoding
credit5_norm$type_of_loan[credit5_norm$type_of_loan == ""] <- "undefined"
count_encoding <- data.table(credit5_norm)[, .(count = .N), by = type_of_loan]
credit5_norm <- merge(credit5_norm, count_encoding, by = "type_of_loan", all.x = TRUE)
setnames(credit5_norm, "count", "type_of_loan_encoded")
#payment_behaviour encoding
count_encoding <- data.table(credit5_norm)[, .(count = .N), by = payment_behaviour]
credit5_norm <- merge(credit5_norm, count_encoding, by = "payment_behaviour", all.x = TRUE)
setnames(credit5_norm, "count", "payment_behaviour_encoded")
colSums(is.na(credit5_norm))
credit5_norm <- credit5_norm[,-c(1,2)]
# payment_of_min_amount encoding
credit5_norm$payment_of_min_amount <- as.factor(credit5_norm$payment_of_min_amount)
credit5_norm$payment_of_min_amount <- factor(credit5_norm$payment_of_min_amount, levels = c("NM", "Yes", "No"), labels = c(0.5, 1, 0))
levels(credit5_norm$payment_of_min_amount)
credit5_norm$payment_of_min_amount <- as.numeric(credit5_norm$payment_of_min_amount)


credit5_norm$credit_mix <- as.numeric(credit5_norm$credit_mix)
credit5_norm$type_of_loan_encoded <- as.numeric(credit5_norm$type_of_loan_encoded)
credit5_norm$payment_behaviour_encoded <- as.numeric(credit5_norm$payment_behaviour_encoded)

str(credit5_norm)
set.seed(412)
credit5_norm$credit_score <- as.factor(credit5_norm$credit_score)
credit5_norm$credit_mix <- as.factor(credit5_norm$credit_mix)
credit5_norm$type_of_loan_encoded <- as.factor(credit5_norm$type_of_loan_encoded)
credit5_norm$payment_behaviour_encoded <- as.factor(credit5_norm$payment_behaviour_encoded)
credit_model <- credit5_norm
credit_model <- credit_model[sample(nrow(credit_model), 3000, replace = FALSE), ]

# Split data
train_index <- createDataPartition(credit_model$credit_score, p = 0.8, list = FALSE)
train_data <- credit5_norm[train_index, ]
test_data <- credit5_norm[-train_index, ]
# cross-validation
cv_control <- trainControl(method = "cv", number = 5, sampling = "up")
# Train multinomial logistic regression model with PCA
multinom_model <- train(
  credit_score ~ ., 
  data = train_data, 
  method = "multinom", 
  trControl = cv_control,
  preProcess = c("center", "scale"),
  tuneLength = 10
)
# Predict credit scores on test set
predictions_pca <- predict(multinom_model, newdata = test_data)

# Model performance
confusionMatrix(predictions_pca, test_data$credit_score)
# ### The confusion matrix shows the counts of correct and incorrect predictions for each class. It looks good but it can be better also Models accuracy is 61.24%. Model accuracy achieved by always predicting the most frequent class around 53.26%. A measure of agreement between actual and predicted classes, adjusted for chance agreement. Here, it's around 0.3343, indicating fair agreement. Sensitivity is higher for class 2 72.9% but sensitivity in the other class below 50% so we should be deal with that.
# 
library(corrplot)
corr = cor(credit5_norm_num)
corrplot(corr, method = "number", type = "upper", diag = FALSE,number.cex = 0.7)
# 
#neural network
nn_model <- train(
  credit_score ~ ., 
  data = train_data, 
  method = "nnet", 
  trControl = cv_control,
  preProcess = c("center", "scale"),
  tuneLength = 10
)
predictions_nn <- predict(nn_model, newdata = test_data)
confusionMatrix(predictions_nn, test_data$credit_score)

#random forest
rf_pca_model <- train(
  credit_score ~ ., 
  data = train_data, 
  method = "rf", 
  trControl = cv_control,
  preProcess = c("center", "scale"),
  tuneLength = 10
)
predictions_rf <- predict(rf_pca_model, newdata = test_data)
confusionMatrix(predictions_rf, test_data$credit_score)

#svm
svm_pca_model <- train(
  credit_score ~ ., 
  data = train_data, 
  method = "svmRadial", 
  trControl = cv_control,
  preProcess = c("center", "scale"),
  tuneLength = 10
)
predictions_svm <- predict(svm_pca_model, newdata = test_data)
confusionMatrix(predictions_svm, test_data$credit_score)

#gradient boost
xgb_tree_model <- train(
  credit_score ~ ., 
  data = train_data, 
  method = "xgbTree", 
  trControl = cv_control,
  preProcess = c("center", "scale"),
  tuneLength = 10,
  verbosity = 0
)
predictions_xgb_tree <- predict(xgb_tree_model, newdata = test_data)
confusionMatrix(predictions_xgb_tree, test_data$credit_score)
