library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(MASS)
library(scales)
library(patchwork)

set.seed(67)

############## imports and data cleaning #############

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

banks_returns = read_excel("4bedrijvenR.xls",
                           sheet = "Sheet1",
                           range = "$E$1:$F$3283",
                           col_names = FALSE)
names(banks_returns) = c("ING", "ABN AMRO")
banks_returns_z = as.data.frame(scale(banks_returns))

pareto = read_excel("ParetoData.xlsx")
pareto1843 = pareto[pareto$Year == 1843, ]
pareto1843$Frequency = pareto1843$Count / sum(pareto1843$Count)
pareto1880 = pareto[pareto$Year == 1880, ]
pareto1880$Frequency = pareto1880$Count / sum(pareto1880$Count)

################ S&P500 returns graphed ##################

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

#############ING/ABNAMRO scatterplots###################################

ggplot(banks_returns, aes(ING,`ABN AMRO`)) +
  geom_point(shape = "circle", size = 0.8, col = "steelblue") +
  stat_ellipse(level = 0.95, linewidth = 1.5, col = "darkblue") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggplot(banks_returns_z, aes(ING,`ABN AMRO`)) +
  geom_point(shape = "circle", size = 0.8, col = "steelblue") +
  coord_cartesian(xlim = c(-6, 6), ylim = c(-8, 8)) +
  stat_ellipse(level = 0.95, linewidth = 1.5, col = "darkblue") +
  theme_bw() +
  theme(panel.grid = element_blank())

rho = cor(banks_returns)
sim_normal = mvrnorm(n = nrow(banks_returns), mu = c(0,0), Sigma = rho)
sim_normal = as.data.frame(sim_normal)
colnames(sim_normal) = c("ING_sim", "ABNAMRO_sim")

ggplot(sim_normal, aes(ING_sim, ABNAMRO_sim)) +
  geom_point(shape = "circle", size = 0.8, col = "steelblue") +
  coord_cartesian(xlim = c(-6, 6), ylim = c(-8, 8)) +
  stat_ellipse(level = 0.95, linewidth = 1.5, col = "darkblue") +
  theme_bw() +
  theme(panel.grid = element_blank())

rank_sim_ING = rank(sim_normal$ING_sim)
rank_sim_ABNAMRO = rank(sim_normal$ABNAMRO_sim)

sorted_ING = sort(banks_returns$ING)
sorted_ABNAMRO = sort(banks_returns$`ABN AMRO`)

# map the original data according to simulated ranks
ING_new = sorted_ING[rank_sim_ING]
ABNAMRO_new = sorted_ABNAMRO[rank_sim_ABNAMRO]

# combine into a new data frame
banks_reassigned = data.frame(
  ING = ING_new,
  ABNAMRO = ABNAMRO_new
)

ggplot(banks_reassigned, aes(ING,ABNAMRO)) +
  geom_point(shape = "circle", size = 0.8, col = "steelblue") +
  stat_ellipse(level = 0.95, linewidth = 1.5, col = "darkblue") +
  theme_bw() +
  theme(panel.grid = element_blank())

####### Pareto data analysis ####################################

sp1 = ggplot(pareto1843, aes(Income, Count)) +
  geom_point(shape = "circle", size = 2, col = "steelblue") +
  theme_bw() +
  theme(panel.grid = element_blank())

sp2 = ggplot(pareto1880, aes(Income, Count)) +
  geom_point(shape = "circle", size = 2, col = "darkred") +
  theme_bw() +
  scale_y_continuous(labels = label_number()) +
  theme(panel.grid = element_blank())

(sp1 + sp2) + plot_annotation(title = "1843 vs. 1880 Incomes", theme = theme(
  plot.title = element_text(hjust = 0.5)  # hjust = 0.5 centers the title
))


sp_log = ggplot(pareto, aes(log(Income), log(Count), color = factor(Year))) +
  geom_point(shape = 16, size = 2) +
  scale_color_manual(values = c("1843" = "steelblue", "1880" = "darkred"),
                     name = "Year") +
  theme_bw() +
  theme(panel.grid = element_blank())

sp_log

hist1 = ggplot(pareto1843, aes(x = factor(Income), y = Frequency)) +
  geom_col(fill = "steelblue", color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = label_number()) +
  theme_bw() +
  labs(x = "Income", y = "Count")

hist2 = ggplot(pareto1880, aes(x = factor(Income), y = Frequency)) +
  geom_col(fill = "darkred", color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = label_number()) +
  theme_bw() +
  labs(x = "Income", y = "Count")

(hist1 + hist2) + plot_annotation(title = "1843 vs. 1880 Incomes", theme = theme(
  plot.title = element_text(hjust = 0.5)  # hjust = 0.5 centers the title
))

#######Scatterplot for the log returns vs empirical probability######

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
