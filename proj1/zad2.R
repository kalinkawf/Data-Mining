# a) wczytanie
#data <- read.table("C:/studia/Data Mining/airpollution.txt", header = TRUE)
data <- read.table("airpollution.txt", header = TRUE)

summary(data)

library(ggplot2)

ggplot(data, aes(x = NOx, y = Mortality)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Wykres punktowy z linią trendu NOx a Mortality", x = "NOx", y = "Mortality")

model <- lm(Mortality ~ NOx, data = data)
summary(model)

# b) nachylenie
coef_nachylenia <- coef(model)[2]
print(coef_nachylenia)

# błąd standardowy
blad_standardowy <- summary(model)$coefficients[2, 2]
print(blad_standardowy)

# c) N0x
model_logNOx <- lm(Mortality ~ log(NOx), data = data)
summary(model_logNOx)

ggplot(data, aes(x = log(NOx), y = Mortality)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Wykres punktowy z linią trendu log(NOx) a Mortality", x = "log(NOx)", y = "Mortality")

coef_nachylenia_logNOx <- coef(model_logNOx)[2]
print(coef_nachylenia_logNOx)

blad_standardowy_logNOx <- summary(model_logNOx)$coefficients[2, 2]
print(blad_standardowy_logNOx)

# d) 
student_resid <- rstudent(model_logNOx)
outliers <- which(abs(student_resid) > 2)
print(outliers)

model_nowy <- lm(Mortality ~ log(NOx), data = data[-outliers, ])
summary(model_nowy)

R2_original <- summary(model_logNOx)$r.squared
R2_new <- summary(model_nowy)$r.squared

print(paste("R-squared dla oryginalnego modelu:", R2_original))
print(paste("R-squared dla nowego modelu:", R2_new))

