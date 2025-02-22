# Install dan load package jika belum ada
install.packages("MASS") 
library(MASS)

# Data
data <- data.frame(
  Paket = c(12, 15, 20, 18, 25),
  Jam_Kerja = c(8, 9, 10, 9, 11),
  Lokasi = c(3, 4, 5, 4, 6),
  Pelanggan = c(10, 12, 15, 14, 18),
  Keterlambatan = c(1, 0, 2, 1, 0)
)

# Model Regresi Poisson
model <- glm(Paket ~ Jam_Kerja + Lokasi + Pelanggan + Keterlambatan, 
             family = poisson(link = "log"), data = data)

# Hasil Koefisien
summary(model)

# Prediksi untuk X1=10, X2=5, X3=15, X4=1
new_data <- data.frame(Jam_Kerja=10, Lokasi=5, Pelanggan=15, Keterlambatan=1)
predicted_paket <- predict(model, new_data, type = "response")
print(predicted_paket)
