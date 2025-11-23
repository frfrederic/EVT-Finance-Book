library(ggplot2)
library(dplyr)

returns = read.csv("snp_returns_70_25.csv")

returns = returns[-c(1, 2, 3), ]
rownames(returns) = NULL
names(returns)[names(returns) == "Price"] = "Date"
names(returns)[names(returns) == "Return.."] = "Return"
returns$Date = as.Date(returns$Date, format = "%Y-%m-%d")
returns$rank = rank(returns$Return)
n = nrow(returns)
p = (returns$rank - 0.5) / n
returns$Return_norm_rank = qnorm(p, mean = 0, sd = 1)
returns$Return_log = log(1 + returns$Return / 100)
returns$Empirical_Prob = returns$rank / n


### Bar chart for unmodified returns ###
ggplot(returns, aes(x = Date, y = Return)) +
  geom_col(fill = "steelblue", width = 2) +
  labs(title = "Daily S&P500 Returns",
       x = "Date",
       y = "Daily Return (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  coord_cartesian(ylim = c(-10, 10), xlim = c(as.Date("1972-01-05"), as.Date("2022-12-31"))) +
  theme_bw()


# Bar chart for ranked normal data
ggplot(returns, aes(x = Date, y = Return_norm_rank)) +
  geom_col(fill = "steelblue", width = 2) +
  labs(title = "Daily Rank-based Normalized S&P500 Returns",
       x = "Date",
       y = "Normal Quantile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  coord_cartesian(ylim = c(-8, 8),
                  xlim = c(as.Date("1972-01-05"), as.Date("2022-12-31"))) +
  theme_bw()

#Scatterplot for the log returns vs empirical probability

returns_tail = returns %>%
  arrange(Empirical_Prob) %>%
  slice(1:100)

ggplot(returns_tail, aes(x = -1 * Return_log, y = Empirical_Prob)) +
  geom_point(color = "firebrick") +
  labs(
    x = "Log Return",
    y = "Empirical Probability (Rank / n)",
    title = "100 Lowest Empirical Probabilities of Log Returns"
  ) +
  theme_bw()

#Same plot but with log probabilities
ggplot(returns_tail, aes(x = -1 * Return_log, y = log(Empirical_Prob))) +
  geom_point(color = "firebrick") +
  labs(
    x = "Log Return",
    y = "Empirical Probability (Rank / n)",
    title = "100 Lowest Empirical Probabilities of Log Returns"
  ) +
  theme_bw()

returns_tail_large = returns %>%
  arrange(Empirical_Prob) %>%
  slice(1:2000)

ggplot(returns_tail_large, aes(x = -1 * Return_log, y = Empirical_Prob)) +
  geom_point(color = "firebrick") +
  labs(
    x = "Log Return",
    y = "Empirical Probability (Rank / n)",
    title = "100 Lowest Empirical Probabilities of Log Returns"
  ) +
  theme_bw()

ggplot(returns_tail_large, aes(x = -1 * Return_log, y = log(Empirical_Prob))) +
  geom_point(color = "firebrick") +
  labs(
    x = "Log Return",
    y = "Empirical Probability (Rank / n)",
    title = "100 Lowest Empirical Probabilities of Log Returns"
  ) +
  theme_bw()
