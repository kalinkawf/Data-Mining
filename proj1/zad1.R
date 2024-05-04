library(MASS)

data(Cars93)

cols_to_summary <- c("Manufacturer", "Model", "Min.Price", "MPG.city", "MPG.highway", "Weight", "Origin", "Type")
head(Cars93[cols_to_summary], 20)

# Nowe zmienne
Cars93$Fuel.city <- (1 / Cars93$MPG.city) * 3.8 * 1.6
Cars93$Fuel.highway <- (1 / Cars93$MPG.highway) * 3.8 * 1.6
Cars93$Weight.kg <- Cars93$Weight * 0.4536
Cars93$Price.PLN <- Cars93$Price * 3.35

cols_to_summary <- c("Manufacturer", "Model", "Fuel.city", "Fuel.highway", "Weight.kg", "Price.PLN")
head(Cars93[cols_to_summary], 20)

# c) Kwantyl
price_summary <- summary(Cars93$Price)
quantile_95 <- quantile(Cars93$Price, 0.95)

print(price_summary)
print(quantile_95)

expensive_cars <- subset(Cars93, Price > quantile_95, select=c(Make, Model, Price))
print(expensive_cars)

num_expensive_cars <- nrow(expensive_cars)
print(num_expensive_cars)

# e) Wykresy
barplot(table(Cars93$Type), main="Liczba samochodów według kategorii", xlab="Kategoria", ylab="Liczba samochodów")

pie(table(Cars93$Type), main="Wykres kołowy dla zmiennej Type")

sport_cars <- sum(Cars93$Type == "Sporty")
print(sport_cars)

# f) amerykańskie i nieamerykańskie samochody
american_cars <- Cars93$MPG.city[Cars93$Origin == "USA"]
non_american_cars <- Cars93$MPG.city[Cars93$Origin != "USA"]

max_mpg <- max(max(american_cars), max(non_american_cars))
par(mfrow=c(1,2))
boxplot(american_cars, main="Amerykańskie samochody", ylab="MPG (Mile per Gallon)", ylim=c(0, max_mpg))
boxplot(non_american_cars, main="Nieamerykańskie samochody", ylab="MPG (Mile per Gallon)", ylim=c(0, max_mpg))

# g) Nowe okno wykresów
#cor_matrix <- cor(Cars93[c("MPG.city", "Min.Price", "MPG.highway")])

par(mfrow=c(1,2))

plot(Cars93$Min.Price, Cars93$MPG.city, 
     xlab="Cena podstawowa", ylab="Zużycie benzyny w mieście",
     main="Cena vs. Zużycie w mieście")

plot(Cars93$MPG.city, Cars93$MPG.highway,
     xlab="Zużycie w mieście", ylab="Zużycie na autostradzie",
     main="Zużycie w mieście vs. Na autostradzie")

pairs(Cars93[c("MPG.city", "Min.Price", "MPG.highway")], 
      main="Wykresy rozrzutu i macierz korelacji")

# h) Histogram 
hist(Cars93$Weight, 
     main="Histogram częstości dla wagi samochodu",
     xlab="Waga samochodu",
     ylab="Częstość")


