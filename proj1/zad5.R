data <- read.table("gala_data.txt", header = TRUE)
head(data)

# a)
model <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = data)
summary(model)

residuals.glm(model,type="response")
residuals.glm(model,type="pearson")
residuals.glm(model,type="deviance")
