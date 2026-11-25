
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
  mutate(Gender = na_if(Gender, "Unknown")) %>% 
         #Marital_Status  = na_if(Marital_Status,  "Unknown"),
         #Income_Category = na_if(Income_Category, "Unknown")) %>%   # "Unknown" = missing
  distinct() %>%                               # remove duplicate rows
  filter(!is.na(Gender),
         !is.na(Credit_Limit),
         Credit_Limit >= 0,
         Gender >= 0)                 # keep only valid values in our columns

print(dim(bank_clean))         # rows and columns after cleaning
write_csv(bank_clean, "BankChurners_clean.csv")

gender   <- bank_clean$Gender
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
  labs(title  = "Histogram of credit limit with normal curve",
       x      = "Credit Limit (US$)",
       y      = "Number of customers",
       colour = NULL) +
  geom_vline(data = stats,
             aes(xintercept = Value, color = Metric, linetype = Metric),
             linewidth = 1) +
  scale_color_manual(values = c("Mean" = "red", "Median" = "blue")) +
  scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dashed"))

print(histogram)

ggplot(bank_clean, aes(x = factor(gender), y = credit_limit)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               shape = 3, size = 3.5, color = "red") +
  labs(title = "Comparison of Credit Limit between Males and Females",
       subtitle = "Horizontal line = Median | Red plus = Mean",
       x = "Gender",
       y = "Credit Limit (US$)") +
  theme_minimal()
