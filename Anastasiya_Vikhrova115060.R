
library(dplyr)
library(ggplot2)
library(eurostat)
library(moments)
#Zadanie 1-3

populacja <- get_eurostat("demo_pjan", filters = list(geo = "PL", sex = "T", age = "TOTAL"))
bezrobocie <- get_eurostat("une_rt_m", filters = list(geo = "PL", sex = "T", unit = "PC_ACT", age = "TOTAL"))
aktywnosc <- get_eurostat("lfsa_argan", filters = list(geo = "PL", sex = "T", age = "Y15-64"))

# Przygotowanie danych
populacja_filtered <- populacja %>%
  filter(time >= as.Date("2010-01-01")) %>%
  group_by(time) %>%
  summarise(Populacja = sum(values, na.rm = TRUE))

bezrobocie_filtered <- bezrobocie %>%
  filter(time >= as.Date("2010-01-01")) %>%
  mutate(year = format(time, "%Y")) %>%
  group_by(year) %>%
  summarise(Stopa_Bezrobocia = mean(values, na.rm = TRUE)) %>%
  mutate(time = as.Date(paste0(year, "-01-01")))

aktywnosc_filtered <- aktywnosc %>%
  filter(time >= as.Date("2010-01-01")) %>%
  group_by(time) %>%
  summarise(Aktywnosc = mean(values, na.rm = TRUE))

# Łączenie danych
data_filtered <- populacja_filtered %>%
  inner_join(bezrobocie_filtered, by = "time") %>%
  inner_join(aktywnosc_filtered, by = "time") %>%
  na.omit()

# Model regresji liniowej
model <- lm(Populacja ~ Stopa_Bezrobocia + Aktywnosc, data = data_filtered)
summary(model)
#Model jako całość jest statystycznie istotny (p = 0.01456).R² wskazuje, że model wyjaśnia umiarkowaną część zmienności w populacji (53,65%).

# Statystyki opisowe
statystyki_opisowe <- data_filtered %>%
  summarise(across(c(Populacja, Stopa_Bezrobocia, Aktywnosc), list(
    mean = ~mean(.),
    median = ~median(.),
    sd = ~sd(.),
    skewness = ~skewness(.),
    kurtosis = ~kurtosis(.)
  )))
print(statystyki_opisowe)

# Wykresy dla zmiennych niezależnych
plot_bezrobocie <- ggplot(data_filtered, aes(x = Stopa_Bezrobocia, y = Populacja)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Zależność Populacji od Stopy Bezrobocia", x = "Stopa Bezrobocia (%)", y = "Populacja") +
  theme_minimal()
print(plot_bezrobocie)
ggsave("stopa_bezrobocia_vs_populacja.png", plot = plot_bezrobocie)

plot_aktywnosc <- ggplot(data_filtered, aes(x = Aktywnosc, y = Populacja)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Zależność Populacji od Aktywności Zawodowej", x = "Aktywność Zawodowa (%)", y = "Populacja") +
  theme_minimal()
print(plot_aktywnosc)
ggsave("aktywnosc_vs_populacja.png", plot = plot_aktywnosc)

# Histogram dla populacji
histogram_populacja <- ggplot(data_filtered, aes(x = Populacja)) +
  geom_histogram(binwidth = 100000, fill = "blue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram Populacji", x = "Populacja", y = "Częstość")
print(histogram_populacja)
ggsave("histogram_populacja.png", plot = histogram_populacja)

# Wykres Q-Q dla populacji
kwantyl_populacja <- ggplot(data_filtered, aes(sample = Populacja)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(title = "Wykres Q-Q dla Populacji", x = "Teoretyczne kwantyle", y = "Obserwowane kwantyle")
print(kwantyl_populacja)
ggsave("kwantyl_populacja.png", plot = kwantyl_populacja)

# Histogram dla stopy bezrobocia
histogram_bezrobocie <- ggplot(data_filtered, aes(x = Stopa_Bezrobocia)) +
  geom_histogram(binwidth = 0.5, fill = "green", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram Stopy Bezrobocia", x = "Stopa Bezrobocia (%)", y = "Częstość")
print(histogram_bezrobocie)
ggsave("histogram_bezrobocie.png", plot = histogram_bezrobocie)

# Wykres Q-Q dla stopy bezrobocia
kwantyl_bezrobocie <- ggplot(data_filtered, aes(sample = Stopa_Bezrobocia)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(title = "Wykres Q-Q dla Stopy Bezrobocia", x = "Teoretyczne kwantyle", y = "Obserwowane kwantyle")
print(kwantyl_bezrobocie)
ggsave("kwantyl_bezrobocie.png", plot = kwantyl_bezrobocie)

# Histogram dla aktywności zawodowej
histogram_aktywnosc <- ggplot(data_filtered, aes(x = Aktywnosc)) +
  geom_histogram(binwidth = 0.5, fill = "red", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram Aktywności Zawodowej", x = "Aktywność Zawodowa (%)", y = "Częstość")
print(histogram_aktywnosc)
ggsave("histogram_aktywnosc.png", plot = histogram_aktywnosc)

# Wykres Q-Q dla aktywności zawodowej
kwantyl_aktywnosc <- ggplot(data_filtered, aes(sample = Aktywnosc)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(title = "Wykres Q-Q dla Aktywności Zawodowej", x = "Teoretyczne kwantyle", y = "Obserwowane kwantyle")
print(kwantyl_aktywnosc)
ggsave("kwantyl_aktywnosc.png", plot = kwantyl_aktywnosc)

# Zadanie 4
fill_missing_values <- function(data, method = c("previous", "zero", "mean")) {
  method <- match.arg(method)
  data_filled <- data
  
  for (col_name in names(data)) {
    if (any(is.na(data[[col_name]]))) {
      if (method == "previous") {
        for (i in 2:nrow(data_filled)) {
          if (is.na(data_filled[[col_name]][i])) {
            data_filled[[col_name]][i] <- data_filled[[col_name]][i-1]
          }
        }
      } else if (method == "zero") {
        data_filled[[col_name]] <- replace(data[[col_name]], is.na(data[[col_name]]), 0)
      } else if (method == "mean") {
        for (i in 2:(nrow(data_filled) - 1)) {
          if (is.na(data_filled[[col_name]][i])) {
            data_filled[[col_name]][i] <- mean(c(data_filled[[col_name]][i-1], data_filled[[col_name]][i+1]), na.rm = TRUE)
          }
        }
      }
    }
  }
  
  return(data_filled)
}

# Przykład
set.seed(123)
data_example <- data.frame(
  time = as.Date('2020-01-01') + 0:5,
  value = c(1, NA, 3, NA, 5, 6)
)

# Metody uzupełniania
data_filled_previous <- fill_missing_values(data_example, method = "previous")
data_filled_zero <- fill_missing_values(data_example, method = "zero")
data_filled_mean <- fill_missing_values(data_example, method = "mean")

# Wyniki
print("Dane z uzupełnieniem poprzednią wartością:")
print(data_filled_previous)

print("Dane z uzupełnieniem zerem:")
print(data_filled_zero)

print("Dane z uzupełnieniem średnią sąsiednich wartości:")
print(data_filled_mean)

# Zadanie 5
generuj_srednie <- function(n, k, df) {
  # Generowanie n średnich z k-elementowych próbek z rozkładu t-Studenta
  srednie <- replicate(n, mean(rt(k, df = df)))
  return(srednie)
}

# Parametry
n <- 100
k <- 1000
df <- 3.5

# Generowanie średnich
srednie <- generuj_srednie(n, k, df)

str(srednie)
head(srednie)

# Histogram średnich
hist_srednie <- ggplot(data.frame(srednie = srednie), aes(x = srednie)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "white") +
  labs(title = "Histogram średnich (rozkład t-Studenta)", x = "Średnia", y = "Częstość") +
  theme_minimal()
print(hist_srednie)

# Wykres QQ dla średnich
qq_srednie <- ggplot(data.frame(srednie = srednie), aes(sample = srednie)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Wykres QQ dla średnich (rozkład t-Studenta)", x = "Teoretyczne kwantyle", y = "Obserwowane kwantyle") +
  theme_minimal()
print(qq_srednie)



