data <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", header=FALSE)

colnames(data) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

# Wczytanie biblioteki do obsługi danych iris
iris.norm <- as.data.frame(lapply(iris[, 1:4], function(x) (x - min(x)) / (max(x) - min(x))))

# Podział na zbiór treningowy i testowy
set.seed(123) # Ustawienie ziarna dla powtarzalności
train_index <- sample(1:nrow(iris), 0.7 * nrow(iris)) # 70% danych do zbioru treningowego
train_data <- iris.norm[train_index, ]
test_data <- iris.norm[-train_index, ]

# Definicja funkcji do obliczania dokładności klasyfikacji
accuracy <- function(true_labels, predicted_labels) {
  return(mean(true_labels == predicted_labels))
}

# Zdefiniowanie różnych wartości parametru k
k_values <- seq(1, 20, by = 1)

# Wytrenowanie modeli k-najbliższych sąsiadów dla różnych wartości k
accuracies <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  predicted_species <- knn(train_data[, 1:4], test_data[, 1:4], train_data$Species, k = k_values[i])
  accuracies[i] <- accuracy(test_data$Species, predicted_species)
}

# Wykres dokładności klasyfikacji w zależności od liczby sąsiadów k
plot(k_values, accuracies, type = "b", pch = 19, col = "blue", xlab = "Liczba sąsiadów (k)", ylab = "Dokładność klasyfikacji")
