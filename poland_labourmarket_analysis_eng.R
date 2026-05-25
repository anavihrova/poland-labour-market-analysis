# Poland labour market analysis
# Does unemployment rate and labour activity rate explain population changes?
# Data from Eurostat, 2010 onwards. Parts 1-3: regression + plots,
# part 4: custom missing value function, part 5: CLT demo.

library(dplyr)
library(ggplot2)
library(eurostat)
library(moments)  # needed for skewness() and kurtosis()


# ---- part 1-3: regression analysis ----

# pulling three Eurostat datasets for Poland, total population (all sexes)
# demo_pjan  — annual population
# une_rt_m   — monthly unemployment rate as % of active population
# lfsa_argan — annual activity rate for people aged 15-64

populacja  <- get_eurostat("demo_pjan",  filters = list(geo = "PL", sex = "T", age = "TOTAL"))
bezrobocie <- get_eurostat("une_rt_m",   filters = list(geo = "PL", sex = "T", unit = "PC_ACT", age = "TOTAL"))
aktywnosc  <- get_eurostat("lfsa_argan", filters = list(geo = "PL", sex = "T", age = "Y15-64"))


# unemployment is monthly, the other two are annual — so we average it by year
# to make all three datasets compatible before joining

population_filtered <- populacja %>%
  filter(time >= as.Date("2010-01-01")) %>%
  group_by(time) %>%
  summarise(Population = sum(values, na.rm = TRUE))

unemployment_filtered <- bezrobocie %>%
  filter(time >= as.Date("2010-01-01")) %>%
  mutate(year = format(time, "%Y")) %>%
  group_by(year) %>%
  summarise(Unemployment_Rate = mean(values, na.rm = TRUE)) %>%
  mutate(time = as.Date(paste0(year, "-01-01")))

activity_filtered <- aktywnosc %>%
  filter(time >= as.Date("2010-01-01")) %>%
  group_by(time) %>%
  summarise(Activity_Rate = mean(values, na.rm = TRUE))


# inner join so we only keep years where all three variables are available
data_filtered <- population_filtered %>%
  inner_join(unemployment_filtered, by = "time") %>%
  inner_join(activity_filtered,     by = "time") %>%
  na.omit()


# linear regression: does unemployment + activity rate predict population size?
model <- lm(Population ~ Unemployment_Rate + Activity_Rate, data = data_filtered)
summary(model)

# the model is statistically significant overall (p = 0.01456)
# R² = 0.5365 — these two variables explain about 54% of the variation in population,
# which is a decent fit for a simple two-predictor model on this kind of data


# skewness and kurtosis to check if distributions are symmetric and how heavy the tails are
descriptive_stats <- data_filtered %>%
  summarise(across(c(Population, Unemployment_Rate, Activity_Rate), list(
    mean     = ~mean(.),
    median   = ~median(.),
    sd       = ~sd(.),
    skewness = ~skewness(.),
    kurtosis = ~kurtosis(.)
  )))

print(descriptive_stats)


# scatter plots with regression line — visually shows direction of each relationship
# expecting negative slope for unemployment (higher unemployment = lower population)
# and positive slope for activity rate

plot_unemployment <- ggplot(data_filtered, aes(x = Unemployment_Rate, y = Population)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Population vs. Unemployment Rate",
    x     = "Unemployment Rate (%)",
    y     = "Population"
  ) +
  theme_minimal()

print(plot_unemployment)
ggsave("unemployment_rate_vs_population.png", plot = plot_unemployment)

plot_activity <- ggplot(data_filtered, aes(x = Activity_Rate, y = Population)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Population vs. Labour Market Activity Rate",
    x     = "Activity Rate (%)",
    y     = "Population"
  ) +
  theme_minimal()

print(plot_activity)
ggsave("activity_rate_vs_population.png", plot = plot_activity)


# histograms + Q-Q plots for all three variables
# Q-Q plots are useful here because the regression assumes normally distributed residuals —
# checking the variables themselves gives a first idea of the data shape

# population
histogram_population <- ggplot(data_filtered, aes(x = Population)) +
  geom_histogram(binwidth = 100000, fill = "blue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram — Population", x = "Population", y = "Frequency")

print(histogram_population)
ggsave("histogram_population.png", plot = histogram_population)

qq_population <- ggplot(data_filtered, aes(sample = Population)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(title = "Q-Q Plot — Population", x = "Theoretical quantiles", y = "Observed quantiles")

print(qq_population)
ggsave("qq_population.png", plot = qq_population)

# unemployment rate
histogram_unemployment <- ggplot(data_filtered, aes(x = Unemployment_Rate)) +
  geom_histogram(binwidth = 0.5, fill = "green", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram — Unemployment Rate", x = "Unemployment Rate (%)", y = "Frequency")

print(histogram_unemployment)
ggsave("histogram_unemployment.png", plot = histogram_unemployment)

qq_unemployment <- ggplot(data_filtered, aes(sample = Unemployment_Rate)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(title = "Q-Q Plot — Unemployment Rate", x = "Theoretical quantiles", y = "Observed quantiles")

print(qq_unemployment)
ggsave("qq_unemployment.png", plot = qq_unemployment)

# activity rate
histogram_activity <- ggplot(data_filtered, aes(x = Activity_Rate)) +
  geom_histogram(binwidth = 0.5, fill = "red", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram — Activity Rate", x = "Activity Rate (%)", y = "Frequency")

print(histogram_activity)
ggsave("histogram_activity.png", plot = histogram_activity)

qq_activity <- ggplot(data_filtered, aes(sample = Activity_Rate)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(title = "Q-Q Plot — Activity Rate", x = "Theoretical quantiles", y = "Observed quantiles")

print(qq_activity)
ggsave("qq_activity.png", plot = qq_activity)


# ---- part 4: custom missing value imputation function ----

# three strategies for filling NAs in a dataframe:
#   "previous" — carry the last known value forward (good for time series)
#   "zero"     — replace with 0 (only makes sense if 0 is a meaningful value)
#   "mean"     — interpolate using the average of the neighbours on each side

fill_missing_values <- function(data, method = c("previous", "zero", "mean")) {
  method      <- match.arg(method)
  data_filled <- data

  for (col_name in names(data)) {
    if (any(is.na(data[[col_name]]))) {

      if (method == "previous") {
        for (i in 2:nrow(data_filled)) {
          if (is.na(data_filled[[col_name]][i])) {
            data_filled[[col_name]][i] <- data_filled[[col_name]][i - 1]
          }
        }

      } else if (method == "zero") {
        data_filled[[col_name]] <- replace(data[[col_name]], is.na(data[[col_name]]), 0)

      } else if (method == "mean") {
        for (i in 2:(nrow(data_filled) - 1)) {
          if (is.na(data_filled[[col_name]][i])) {
            data_filled[[col_name]][i] <- mean(
              c(data_filled[[col_name]][i - 1], data_filled[[col_name]][i + 1]),
              na.rm = TRUE
            )
          }
        }
      }
    }
  }

  return(data_filled)
}


# small example to show how each method behaves differently on the same NAs
set.seed(123)
data_example <- data.frame(
  time  = as.Date("2020-01-01") + 0:5,
  value = c(1, NA, 3, NA, 5, 6)
)

data_filled_previous <- fill_missing_values(data_example, method = "previous")
data_filled_zero     <- fill_missing_values(data_example, method = "zero")
data_filled_mean     <- fill_missing_values(data_example, method = "mean")

print("method: carry forward previous value")
print(data_filled_previous)

print("method: replace with zero")
print(data_filled_zero)

print("method: replace with mean of neighbours")
print(data_filled_mean)


# ---- part 5: central limit theorem demo ----

# the CLT says that sample means from almost any distribution will look
# approximately normal if the sample size is large enough — even if the
# underlying distribution is not normal itself
#
# here we test it using the t-distribution with df=3.5, which has heavier
# tails than a normal. we draw k=1000 samples, compute the mean, repeat
# n=100 times, and check if those means look normal

generate_means <- function(n, k, df) {
  # n  — how many sample means to generate
  # k  — size of each sample (large k is what makes CLT kick in)
  # df — degrees of freedom of the t-distribution
  means <- replicate(n, mean(rt(k, df = df)))
  return(means)
}

n  <- 100   # number of sample means
k  <- 1000  # sample size per draw — large enough for CLT to hold
df <- 3.5   # t-distribution with df=3.5 is noticeably heavy-tailed

means <- generate_means(n, k, df)

str(means)
head(means)


# if CLT holds: histogram should look bell-shaped, Q-Q points should follow the line
# with k=1000 we'd expect it to work well even with this heavy-tailed distribution

hist_means <- ggplot(data.frame(means = means), aes(x = means)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "white") +
  labs(
    title = "Histogram of sample means (t-distribution, df = 3.5)",
    x     = "Sample mean",
    y     = "Frequency"
  ) +
  theme_minimal()

print(hist_means)

qq_means <- ggplot(data.frame(means = means), aes(sample = means)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(
    title = "Q-Q plot of sample means (t-distribution, df = 3.5)",
    x     = "Theoretical quantiles",
    y     = "Observed quantiles"
  ) +
  theme_minimal()

print(qq_means)
