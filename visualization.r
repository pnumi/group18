# Group 18 - Visualisation
# Research question: Is there a correlation between Credit_Limit and Dependent_count?


# ---- Step 1: Load the tools ----
library(ggplot2)
library(readr)
library(dplyr)


# ---- Step 2: Load and check the data ----
bank <- read_csv("BankChurners.csv")

print(dim(bank))               # number of rows and columns
print(sum(is.na(bank)))        # number of missing values
print(sum(duplicated(bank)))   # number of duplicate rows


# ---- Step 3: Clean the data ----
bank_clean <- bank %>%
  select(-starts_with("Naive_Bayes")) %>%      # remove 2 model-output columns
  mutate(Education_Level = na_if(Education_Level, "Unknown"),
         Marital_Status  = na_if(Marital_Status,  "Unknown"),
         Income_Category = na_if(Income_Category, "Unknown")) %>%   # "Unknown" = missing
  distinct() %>%                               # remove duplicate rows
  filter(!is.na(Dependent_count),
         !is.na(Credit_Limit),
         Credit_Limit > 0,
         Dependent_count >= 0)                 # keep only valid values in our columns

print(dim(bank_clean))         # rows and columns after cleaning
write_csv(bank_clean, "BankChurners_clean.csv")

dependants   <- bank_clean$Dependent_count
credit_limit <- bank_clean$Credit_Limit


# ---- Step 4: Describe the data ----
print(c(Min    = min(credit_limit),
        Median = median(credit_limit),
        Mean   = mean(credit_limit),
        Max    = max(credit_limit)))

print(c(Min    = min(dependants),
        Median = median(dependants),
        Mean   = mean(dependants),
        Max    = max(dependants)))


# ---- Step 5: Histogram of credit limit with a normal curve ----
n         <- nrow(bank_clean)   # number of customers
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
       x      = "Credit limit (US$)",
       y      = "Number of customers",
       colour = NULL)

print(histogram)
ggsave("histogram.png", histogram, width = 8, height = 5)


# ---- Step 6: Correlation test ----
# Credit limit is not normal, so we use Spearman (not Pearson)
test <- cor.test(dependants, credit_limit,
                 method = "spearman", exact = FALSE)

print(test)


# ---- Step 7: Scatterplot with line of best fit ----
scatterplot <- ggplot(bank_clean, aes(x = Dependent_count, y = Credit_Limit)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.2,
              colour = "steelblue") +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(colour = "Line of best fit")) +
  scale_x_continuous(breaks = 0:5) +
  expand_limits(y = 40000) +
  labs(title  = "Credit limit vs number of dependants",
       x      = "Number of dependants (people)",
       y      = "Credit limit (US$)",
       colour = NULL)

print(scatterplot)
ggsave("scatterplot.png", scatterplot, width = 8, height = 5)