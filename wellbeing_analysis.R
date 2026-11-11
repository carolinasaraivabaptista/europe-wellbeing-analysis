# Well-being in European regions
# Exploratory analysis, hypothesis tests and bootstrap confidence intervals

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(rstatix)
library(boot)

# ---- Data -------------------------------------------------------------------
# The original spreadsheet uses Portuguese column names; rename them to English.
df <- read_excel("data/BemEstarEuropa.xlsx", sheet = 1) %>%
  rename(any_of(c(
    region            = "região",
    country           = "país",
    subregion         = "continente",
    education         = "educação",
    employment        = "emprego",
    unemployment      = "desemprego",
    income            = "rendimento",
    homicides         = "homicidios",
    mortality         = "mortalidade",
    life_expectancy   = "vida",
    pollution         = "poluição",
    voter_turnout     = "voto",
    internet          = "internet",
    rooms_per_person  = "quartos",
    social_support    = "social",
    life_satisfaction = "satisfação"
  ))) %>%
  mutate(life_satisfaction = as.numeric(as.character(life_satisfaction)))

summary(df)
colSums(is.na(df))

# ---- Part 1: exploratory analysis --------------------------------------------

# Outliers. A value of 0 means missing for these three indicators.
ggplot(df, aes(y = education)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Education (with outliers)")
df %>% identify_outliers(education)

ggplot(df, aes(y = pollution)) +
  geom_boxplot(outlier.colour = "pink", outlier.shape = 16, outlier.size = 3) +
  theme_minimal() +
  labs(title = "Air pollution", y = "PM2.5")
df %>% select(region, pollution) %>% identify_outliers(pollution)

ggplot(df, aes(y = internet)) +
  geom_boxplot(outlier.colour = "pink", outlier.shape = 16, outlier.size = 3) +
  theme_minimal() +
  labs(title = "Internet access", y = "Households with internet (%)")
df %>% select(region, country, internet) %>% identify_outliers(internet)

df_educ <- filter(df, education > 0)
df_pol  <- filter(df, pollution > 0)
df_int  <- filter(df, internet > 0)

# Summary statistics with the regions at the extremes of each indicator
describe_indicator <- function(data, var) {
  values <- data[[var]]
  cat("\n==", var, "==\n")
  print(summary(values))
  cat("Highest:\n")
  print(data[which(values == max(values, na.rm = TRUE)), c("country", "region", var)])
  cat("Lowest:\n")
  print(data[which(values == min(values, na.rm = TRUE)), c("country", "region", var)])
}

describe_indicator(df_educ, "education")
for (v in c("employment", "unemployment", "income", "homicides", "mortality",
            "life_expectancy", "voter_turnout", "rooms_per_person",
            "social_support", "life_satisfaction")) {
  describe_indicator(df, v)
}
describe_indicator(df_pol, "pollution")
describe_indicator(df_int, "internet")

# Mean education by country
edu_country <- aggregate(education ~ country, data = df, mean)
ggplot(edu_country, aes(x = reorder(country, -education), y = education)) +
  geom_col(fill = "lightblue") +
  geom_text(aes(label = round(education, 1)), vjust = -0.3, size = 3) +
  theme_minimal() +
  labs(title = "Mean education by country", x = "Country", y = "Education (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Employment and unemployment by country
by_country <- df %>%
  group_by(country, subregion) %>%
  summarise(employment   = mean(employment, na.rm = TRUE),
            unemployment = mean(unemployment, na.rm = TRUE),
            income       = mean(income, na.rm = TRUE),
            .groups = "drop")

g1 <- ggplot(by_country, aes(x = reorder(country, employment), y = employment, fill = subregion)) +
  geom_col() + coord_flip() + theme_minimal() +
  labs(title = "Mean employment by country", x = "Country", y = "Employment (%)")
g2 <- ggplot(by_country, aes(x = reorder(country, unemployment), y = unemployment, fill = subregion)) +
  geom_col() + coord_flip() + theme_minimal() +
  labs(title = "Mean unemployment by country", x = "Country", y = "Unemployment (%)")
g1 + g2

ggplot(by_country, aes(x = reorder(country, income), y = income, fill = subregion)) +
  geom_col() + coord_flip() + theme_minimal() +
  labs(title = "Mean income by country", x = "Country", y = "Income")

# Life expectancy vs mortality
df %>%
  select(country, life_expectancy, mortality) %>%
  pivot_longer(c(life_expectancy, mortality), names_to = "indicator", values_to = "value") %>%
  ggplot(aes(x = reorder(country, value), y = value, fill = indicator)) +
  geom_col(position = "dodge") + coord_flip() + theme_minimal() +
  labs(title = "Life expectancy and mortality by country", x = "Country", y = "Value", fill = "Indicator")

# ---- Hypothesis tests ---------------------------------------------------------

# Q1: Do regions with cleaner air have higher life expectancy?
# Shapiro-Wilk rejects normality, so we use Spearman's rank correlation.
shapiro.test(df$pollution)
shapiro.test(df$life_expectancy)
# H0: no correlation   H1: negative correlation
q1 <- cor.test(df$pollution, df$life_expectancy, method = "spearman", alternative = "less", exact = FALSE)
q1

ggplot(df, aes(x = pollution, y = life_expectancy)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Air pollution (PM2.5)", y = "Life expectancy",
       title = "Air pollution and life expectancy",
       subtitle = sprintf("Spearman rho = %.3f, p-value = %.3f", q1$estimate, q1$p.value))

# Q2: Is life satisfaction higher in Southern Europe?
# H0: no difference   H1: higher in Southern Europe (Wilcoxon rank-sum test)
df$south <- ifelse(df$subregion == "Southern Europe", "Southern Europe", "Rest of Europe")
wilcox.test(life_satisfaction ~ south, data = df, alternative = "less")

ggplot(df, aes(x = south, y = life_satisfaction, fill = south)) +
  geom_boxplot() +
  labs(title = "Life satisfaction by region", x = "", y = "Life satisfaction")

# Same test without the outliers of Southern Europe
south <- df$life_satisfaction[df$south == "Southern Europe"]
q <- quantile(south, c(0.25, 0.75), na.rm = TRUE)
iqr <- q[2] - q[1]
df_no_out <- df[!(df$south == "Southern Europe" &
                  (df$life_satisfaction < q[1] - 1.5 * iqr | df$life_satisfaction > q[2] + 1.5 * iqr)), ]
wilcox.test(life_satisfaction ~ south, data = df_no_out, alternative = "less")

# Q3: Does unemployment differ between European subregions? (Kruskal-Wallis)
q3 <- kruskal.test(unemployment ~ subregion, data = df)
q3
ggplot(df, aes(x = subregion, y = unemployment, fill = subregion)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Unemployment by subregion",
       subtitle = sprintf("Kruskal-Wallis: chi2(%d) = %.2f, p = %.3g", q3$parameter, q3$statistic, q3$p.value),
       x = "Subregion", y = "Unemployment (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- Bootstrap-t confidence intervals for life expectancy ---------------------
life <- df$life_expectancy
B <- 1000        # bootstrap samples
alpha <- 0.05    # 95% confidence
R_nested <- 25   # inner samples to estimate each standard error

trimmed_mean <- function(x, trim = 0.05) mean(x, trim = trim, na.rm = TRUE)
median_na <- function(x) median(x, na.rm = TRUE)

# Implemented by hand
bootstrap_t <- function(x, statistic, B, alpha, R_nested) {
  n <- length(x)
  t0 <- statistic(x)
  t_boot <- numeric(B)
  t_stud <- numeric(B)
  for (i in 1:B) {
    s <- sample(x, n, replace = TRUE)
    t_boot[i] <- statistic(s)
    se <- sd(replicate(R_nested, statistic(sample(s, n, replace = TRUE))))
    t_stud[i] <- (t_boot[i] - t0) / se
  }
  q <- quantile(t_stud, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
  list(estimate = t0, ci = t0 - rev(q) * sd(t_boot), samples = t_boot)
}

set.seed(123)
mean_manual   <- bootstrap_t(life, trimmed_mean, B, alpha, R_nested)
median_manual <- bootstrap_t(life, median_na, B, alpha, R_nested)

# Same intervals with the boot package
with_variance <- function(statistic) {
  function(x, i) {
    s <- x[i]
    c(statistic(s), var(replicate(R_nested, statistic(sample(s, length(s), replace = TRUE)))))
  }
}
ci_mean_boot   <- boot.ci(boot(life, with_variance(trimmed_mean), B), type = "stud")$student[4:5]
ci_median_boot <- boot.ci(boot(life, with_variance(median_na), B), type = "stud")$student[4:5]

results <- data.frame(
  statistic   = c("5% trimmed mean", "Median"),
  ci_manual   = c(sprintf("[%.2f, %.2f]", mean_manual$ci[1], mean_manual$ci[2]),
                  sprintf("[%.2f, %.2f]", median_manual$ci[1], median_manual$ci[2])),
  ci_boot_pkg = c(sprintf("[%.2f, %.2f]", ci_mean_boot[1], ci_mean_boot[2]),
                  sprintf("[%.2f, %.2f]", ci_median_boot[1], ci_median_boot[2]))
)
print(results)

par(mfrow = c(1, 2))
hist(mean_manual$samples, main = "Trimmed mean (bootstrap)", xlab = "")
abline(v = mean_manual$ci, col = "red", lty = 2)
hist(median_manual$samples, main = "Median (bootstrap)", xlab = "")
abline(v = median_manual$ci, col = "red", lty = 2)
par(mfrow = c(1, 1))
