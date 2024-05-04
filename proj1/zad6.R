data <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", header=FALSE)

colnames(data) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

# Podział danych na zbiór treningowy i testowy (np. 60% na treningowy, 40% na testowy)
set.seed(123) # Ustawienie ziarna dla powtarzalności
train_index <- sample(1:nrow(data), 0.6 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

library(rpart)
model <- rpart(Species ~ ., data = train_data, method = "class")

# Pakiet do rysowania
library(rpart.plot)

print(model)
rpart.plot(model)

# b)
predicted_classes <- predict(model, test_data, type = "class")

# macierz błędów
error_matrix <- table(predicted_classes, test_data$Species)
print(error_matrix)

# Obliczenie liczby poprawnie sklasyfikowanych obserwacji
correct_predictions <- sum(diag(error_matrix))

# Obliczenie liczby wszystkich obserwacji oraz dokładności modelu
total_observations <- sum(error_matrix)
accuracy <- correct_predictions / total_observations

print(paste("Dokładność modelu:", accuracy * 100, "%"))

