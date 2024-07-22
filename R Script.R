library(car)
library(readxl)
library(lmtest)
library(openxlsx)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggpubr)

setwd("C:\\Users\\andre\\OneDrive\\Documentos\\Nova IMS\\1º ano\\1º Semestre\\Statistics for Data Science\\Project\\Final Delivery")
df = read_excel("dataset_final.xlsx")

model <- lm(log(Wages) ~ Hobby + Country + Education+ OrgSize + Undergrad + YearsCodePro , data = df)

summary(model)

model_summary <- summary(model)

# Extract coefficients and related information
coefficients_table <- coef(model_summary)

# Print the coefficients table
print(coefficients_table)

# Extract standard errors, t-values, and p-values
se <- coef(model_summary)[, "Std. Error"]
t_values <- coef(model_summary)[, "t value"]
p_values <- coef(model_summary)[, "Pr(>|t|)"]

# Combine them into a data frame
table_data <- data.frame(Coefficient = names(coef(model)),
                         Estimate = coef(model),
                         Std_Error = se,
                         t_value = t_values,
                         p_value = p_values)

# Print the table data
print(table_data)

#---------------------------------------------------------------------#
#Data Exploration

abbreviate_labels <- function(labels, max_length = 8) {
  substr(labels, 1, max_length)
}

# Abbreviate long labels in df$Country
df$Country <- abbreviate_labels(df$Country)

# Plot Country
p1 <- ggplot(df, aes(x = Country)) +
  geom_bar(fill = "lightblue") +
  geom_text(stat = "count", aes(label = stat(count)), vjust = 1.5) +
  labs(x = "Country", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Abbreviate long labels in df$Education
df$Education <- abbreviate_labels(df$Education)

# Plot Education
p2 <- ggplot(df, aes(x = Education)) +
  geom_bar(fill = "lightgreen") +
  geom_text(stat = "count", aes(label = stat(count)), vjust = 1.5) +
  labs(x = "Education", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Abbreviate long labels in df$Undergrad
df$Undergrad <- abbreviate_labels(df$Undergrad)

# Plot Undergrad
p3 <- ggplot(df, aes(x = Undergrad)) +
  geom_bar(fill = "lightcoral") +
  geom_text(stat = "count", aes(label = stat(count)), vjust = 1.5) +
  labs(x = "Undergrad", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Abbreviate long labels in df$OrgSize
df$OrgSize <- abbreviate_labels(df$OrgSize)

# Plot OrgSize
p4 <- ggplot(df, aes(x = OrgSize)) +
  geom_bar(fill = "lightpink") +
  geom_text(stat = "count", aes(label = stat(count)), vjust = 1.5) +
  labs(x = "OrgSize", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Arrange plots in a 1x4 grid
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


#---------------------------------------------------------------------#
#Now, we conduct tests to check for the classical OLS assumptions

#LINEARITY OF DATA:
  
plot(model, which = 1)  # Residuals vs Fitted

#The red line is flat, indicating a linear relationship
#between the dependent variable and the independent variables

#---------------------------------------------------------------------#
#NORMALITY OF RESIDUALS

plot(model, which = 2)  # Normal Q-Q plot

# Most of our points follow a line around 0, so we can assume normality 
#of residuals.
#Plus, we have a big sample (743 observations), so we can perform 
#statistical inference

#---------------------------------------------------------------------#
#HOMOSKEDASTICITY

plot(model, which = 3)  # Scale-Location plot

# Breusch-Pagan test for Heteroskedasticity
#H0 = Homoskedasticity
#H1 = Heteroskedasticity

bptest(model)

# We fail to reject the null hypothesis.

# White test for heteroskedasticity
bptest(formula = model, varformula = ~ Hobby  + Hobby*Country + Hobby* Education  +Hobby*Undergrad + Hobby * YearsCodePro  + Country  + Country*Education  + Country*Undergrad + Country* YearsCodePro + Education+ Education*Undergrad + Education * YearsCodePro +  Undergrad  +YearsCodePro + I(YearsCodePro^2), data = df)

# White special test for heteroskedasticity
bptest(model, ~fitted(model) + I(fitted(model)^2))

# Again, we fail to reject the null hypothesis.
#---------------------------------------------------------------------#
#AUTOCORRELATION

#H0 = No Autocorrelation
#H1 = Autocorrelation

durbinWatsonTest(model)

# We fail to reject the null hypothesis.
#No autocorrelation detected through the Durbin-Watson Test

#---------------------------------------------------------------------#
#MULTICOLLINEARITY

vif(model)

#All the VIF results are around 1, which does not raise concerns about
#multicollinearity.


#---------------------------------------------------------------------#
### Test for model mispecification

#H0: Model is correctly specified

#H1: Model is misspecified

resettest(model)

#We reject the null hypothesis at a 5% significance level.

#The p-value of this test leads us towards concluding that our model is misspecified.
#However, we only have one variable in our dataset that is not a dummy variable.
#We will include exponentials of the numerical variable to see if it improves our specification.

model2 = lm(log(Wages) ~ Hobby + Country + Education + OrgSize + Undergrad + YearsCodePro + I(YearsCodePro^2), data = df)

summary(model2)

resettest(model2)

#Even when adding 'YearsCodePro\^2' (and we also attempted to add \^3 and \^4) our model still shows as misspecified.
#Unfortunately, and given the limitations of our dataset, we must proceed as is.

