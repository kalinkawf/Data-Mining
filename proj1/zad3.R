# Wczytanie danych z pliku "savings.txt" z separatorem przecinka
data <- read.table("savings.txt", header = TRUE, sep = ",")

# Wyświetlenie pierwszych kilku wierszy danych
head(data)

summary(data) # Podsumowanie danych
model <- lm(Savings ~ dpi + ddpi + pop15 + pop75, data = data)
summary(model) # Podsumowanie modelu

# b) Reszty
residuals <- residuals(model)

qqnorm(residuals, main = "Wykres normalny dla reszt")
qqline(residuals)

min_residual_idx <- which.min(residuals)
max_residual_idx <- which.max(residuals)

country_min_residual <- data$sr[min_residual_idx]
country_max_residual <- data$sr[max_residual_idx]

min_residual <- residuals[min_residual_idx]
max_residual <- residuals[max_residual_idx]

cat("Kraj z najmniejszą resztą:", country_min_residual, "Reszta:", min_residual, "\n")
cat("Kraj z największą resztą:", country_max_residual, "Reszta:", max_residual, "\n")

# c) Obliczenie dźwigni (leverage)
leverage <- hatvalues(model)

plot(leverage, main = "Wartości dźwigni", xlab = "Numer obserwacji", ylab = "Dźwignia")
student_residuals <- rstudent(model)

large_leverage <- which(leverage > (2 * mean(leverage)))
large_student_residuals <- which(abs(student_residuals) > 2)

cat("Obserwacje z dużymi wartościami dźwigni:", large_leverage, "\n")
cat("Obserwacje z dużymi resztami studentyzowanymi:", large_student_residuals, "\n")

cat("Wartości reszt studentyzowanych dla obserwacji z dużymi resztami studentyzowanymi:", "\n")
cat(student_residuals[c(7, 46)], "\n")
cat("Wartości dźwigni dla obserwacji z dużymi wartościami dźwigni:", "\n")
cat(leverage[c(21, 23, 44, 49)], "\n")

# d) Wyznaczenie miar DFFITS i DFBETAS
dffits_values <- dffits(model)
dfbetas_values <- dfbetas(model)

cook_distance <- cooks.distance(model)

print("Miary DFFITS:")
print(dffits_values)
print("Miary DFBETAS:")
print(dfbetas_values)
print("Odległości Cooka:")
print(cook_distance)

# Próg dla odległości Cooka
threshold <- 0.1

influential_observations <- which(cook_distance > threshold)

print("Obserwacje wpływowe:")
print(influential_observations)

# e) Max Cooke'a
max_cook_index <- which.max(cook_distance)

data_without_outlier <- data[-max_cook_index, ]
new_model <- lm(Savings ~ dpi + ddpi + pop15 + pop75, data = data_without_outlier)

summary(new_model)

# f
coefficients_df <- as.data.frame(summary(new_model)$coefficients)

coefficients_pop <- coefficients_df[c("pop15", "pop75"), "Estimate"]

coefficients_pop_df <- data.frame(variable = c("pop15", "pop75"), Estimate = coefficients_pop)

# wykres
ggplot(coefficients_pop_df, aes(x = variable, y = Estimate)) +
  geom_col(fill = "lightblue") +
  labs(title = "Zmiany wartości współczynników dla zmiennych pop15 i pop75",
       x = "Zmienna", y = "Wartość współczynnika") +
  theme_minimal()

# Wyciągnij współczynniki z modelu
coefficients <- coef(new_model)

print(coefficients["pop15"])
print(coefficients["pop75"])

max_index_pop15 <- which.max(abs(coefficients_df["pop15", "Estimate"]))
country_max_pop15 <- rownames(coefficients_df)[max_index_pop15]
print(country_max_pop15)


