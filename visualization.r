
library(ggplot2)
library(readr)
library(dplyr)

bank <- read_csv("BankChurners.csv")

print(dim(bank))             
print(sum(is.na(bank)))        
print(sum(duplicated(bank)))   


# Data Cleaning
bank_clean <- bank %>%
  select(-starts_with("Naive_Bayes")) %>%      # remove 2 model-output columns
  #mutate(Education_Level = na_if(Education_Level, "Unknown"),
         #Marital_Status  = na_if(Marital_Status,  "Unknown"),
         #Income_Category = na_if(Income_Category, "Unknown")) %>%   # "Unknown" = missing
  distinct() %>%                               # remove duplicate rows
  filter(!is.na(Dependent_count),
         !is.na(Credit_Limit),
         Credit_Limit >= 0,
         Dependent_count >= 0)                 # keep only valid values in our columns

print(dim(bank_clean))         # rows and columns after cleaning
write_csv(bank_clean, "BankChurners_clean.csv")

dependants   <- bank_clean$Dependent_count
credit_limit <- bank_clean$Credit_Limit

stats <- data.frame(
  Metric = c("Mean", "Median"),
  Value = c(mean(bank_clean$Credit_Limit, na.rm = TRUE),
            median(bank_clean$Credit_Limit, na.rm = TRUE))
)

# print(c(Min    = min(credit_limit),
#         Median = median(credit_limit),
#         Mean   = mean(credit_limit),
#         Max    = max(credit_limit)))
# 
# print(c(Min    = min(dependants),
#         Median = median(dependants),
#         Mean   = mean(dependants),
#         Max    = max(dependants)))


n <- nrow(bank_clean)   # number of customers
bar_width <- 1000         # each bar covers $1,000

# The bell curve, sized to match the bars
normal_curve <- function(x) {
  dnorm(x, mean(credit_limit), sd(credit_limit)) * n * bar_width
}

histogram <- ggplot(bank_clean, aes(x = Credit_Limit)) +
  geom_histogram(binwidth = bar_width, boundary = 0,
                 fill = "steelblue", colour = "white") +
  stat_function(fun = normal_curve, aes(colour = "Normal curve"),
                linewidth = 1) +
  expand_limits(x = 40000, y = 2500) +
  labs(title  = "Histogram of Borrowing limit with normal curve",
       x      = "Borrowing Limit (US$)",
       y      = "Number of customers",
       colour = NULL) +
  geom_vline(data = stats,
             aes(xintercept = Value, color = Metric, linetype = Metric),
             linewidth = 1) +
  scale_color_manual(values = c("Mean" = "red", "Median" = "blue")) +
  scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dashed"))

print(histogram)

ggplot(bank_clean, aes(x = factor(Dependent_count), y = Credit_Limit)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               shape = 3, size = 3.5, color = "red") +
  labs(title = "Comparison of Borrowing Limit by Number of Financial Dependents",
       subtitle = "Horizontal line = Median | Red plus = Mean",
       x = "Number of Financial Dependents",
       y = "Borrowing Limit (US$)") +
  theme_minimal()
