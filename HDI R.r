#Libraries
install.packages("dplyr")
install.packages("readxl")

library(dplyr)
library(readxl)

#Initialising the Data in a Dataframe
data <- read_excel('HDI.xlsx') 
print(data)

# Dropping all Null Values
data_complete <- na.omit(data)  

get_HDI <- function(country){
  hdi_2010 <- filter(data, Country == country & Year == 2010)[4]
  hdi_2019 <- filter(data, Country == country & Year == 2019)[4]
  percent_change <- round(((hdi_2019 - hdi_2010) * 100 / hdi_2010), 4)
  return(percent_change)
}

table <- data.frame(matrix(vector(), 0, 2, dimnames = list(c(), c("Country", "Percentage_Change"))), stringsAsFactors = FALSE)

for (country in unique(data_complete$Country)){
  row <- data.frame(Country = country, Percentage_Change = get_HDI(country))
  table <- rbind(table, row)
}

colnames(table) <- c("Country", "Percentage_Change")
print(table)

arrange(filter(table, Percentage_Change < 0), Percentage_Change) #Countries where HDI declined from 2010 - 2019
table %>% arrange(desc(Percentage_Change)) %>% slice(1:15) #Top 15 Countries with the highest percentage increase in HDI

#Linear Regression

hdi_2021_data = read_excel('HDI_2021.xlsx')
colnames(hdi_2021_data) <- c("Country", "Life_expectancy_at_birth", "Expected_years_of_schooling", "Mean_years_of_schooling", "Gross_national_income_per_capita", "Human_Development_Index")
print(hdi_2021_data)

#Feature Scaling 
Health_Index <- c(round(((hdi_2021_data$Life_expectancy_at_birth - 20) / 65), 6))
Education_Index <- c(round((((hdi_2021_data$Expected_years_of_schooling / 18) + (hdi_2021_data$Mean_years_of_schooling / 15)) / 2), 6))
Income_Index <- c(round((log(hdi_2021_data$Gross_national_income_per_capita) - log(100)) / (log(75000) - log(100)), 6))

hdi_2021_data <- cbind(hdi_2021_data, Health_Index, Education_Index, Income_Index)
# print(hdi_2021_data)

# Model Building 
set.seed(42)

sample <- sample.split(hdi_2021_data, SplitRatio = 0.8)
train_set  <- subset(hdi_2021_data, sample == TRUE)
test_set   <- subset(hdi_2021_data, sample == FALSE)
model <- lm(formula = Human_Development_Index ~ Health_Index + Education_Index + Income_Index, data = train_set)
y_pred <- predict(model, newdata = test_set)
print(paste("OLS R2:", summary(model)$r.squared))

#Linear Regression on the Optimum i
best_model <- lm(formula = Human_Development_Index ~ I(Health_Index ^ (0.83) + Education_Index ^ (0.83) + Income_Index ^ (0.83)), data = train_set)
improved_y_pred <- predict(best_model, newdata = test_set)

print(paste("Modified Regression R2:", summary(best_model)$r.squared))

x <- test_set$Human_Development_Index
y <- improved_y_pred

plot(x = test_set$Human_Development_Index,y = improved_y_pred,
   xlab = "Actual Y",
   ylab = "Predicted Y",
   main = "Actual vs Predicted Plot for Modified Linear Regression",
   pch = 19, col = "#DA70D6"
)

abline(lm(y ~ x), col = "black")