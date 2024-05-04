data <- read.table("realest.txt", header = TRUE)
head(data)

model <- lm(Price ~ Bedroom + Space + Room + Lot + Tax + Bathroom + Garage + Condition, data = data)

summary(model)

# a) Stworzenie kopii danych
data_alternative <- data
data_alternative$Bedroom <- data_alternative$Bedroom + 1
model_alternative <- lm(Price ~ Bedroom + Space + Room + Lot + Tax + Bathroom + Garage + Condition, data = data_alternative)
summary(model_alternative)

model_bedroom_plus_1 <- lm(Price ~ Bedroom, data = data_alternative)
summary(model_bedroom_plus_1)

# b) szacune cene
my_house <- data.frame(
  Bedroom = 3,  # liczba sypialni + 1
  Space = 1500,            # powierzchnia w stopach kwadratowych
  Room = 8,                # liczba pokoi
  Lot = 40,                # szerokość działki w stopach
  Tax = 1000,              # roczny podatek od nieruchomości
  Bathroom = 5,            # liczba łazienek
  Garage = 1               # liczba miejsc parkingowych w garażu
)

expected_price <- predict(model, new_data = my_house, interval = "confidence", level=0.95)
expected_price

