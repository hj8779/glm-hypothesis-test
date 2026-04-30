# ---- Question 4 ----
# Vecteur StatCan : v30124346
# Période : 2003-09 à 2012-12 (112 observations mensuelles)
# Train   : 2003-09 à 2011-12 (100 obs)
# Test    : 2012-01 à 2012-12 (12 obs)
rm(list = ls())

# Enlever les commentaires pour la 1ere execution
# puis les mettre en commentaire une fois installé
# install.packages("cansim")
# install.packages("MASS")

# library(cansim)
library(MASS)

vector_name <- "v30124346"

# Nom du dossier de sortie : img_q4_<vector_name>
img_dir_string <- paste0("img_q4_", vector_name)

# --- Dossier de sortie pour les graphiques ---
img_dir <- file.path(dirname(rstudioapi::getSourceEditorContext()$path),
                     img_dir_string)

# S'il n'y a pas de dossier de sortie, le créer
if (!dir.exists(img_dir)) dir.create(img_dir)

# Compteur pour les graphiques
fig_n <- 0

# ---- 1. Chargement des données depuis le fichier CSV ou StatCan ----

# Enlever le commentaire en cas d'utilisation de StatCan API
# df_raw <- get_cansim_vector(vector_name, refresh = TRUE)




csv_path <- file.path(dirname(rstudioapi::getSourceEditorContext()$path),
                      "cansimData", "1410017401-eng.csv")

# Lecture brute pour sauter les métadonnées en tête et le pied de tableau
raw_lines <- readLines(csv_path, encoding = "UTF-8", warn = FALSE)

# Retourne l'indice de la ligne contenant les données
header_idx <- grep("^\"Beneficiary detail\"", raw_lines)
footer_idx <- grep("^Symbol legend", raw_lines)

# Conserver : header + données
header_line <- raw_lines[header_idx]
all_body_lines <- raw_lines[header_idx:footer_idx]

# On veut le vecteur v30124346 : Beneficiaries receiving regular benefits without reported earnings
target_line <- grep("without reported earnings", all_body_lines, value = TRUE)

# Mettre dans df
df_wide <- read.csv(text = c(header_line, target_line), 
                    check.names = FALSE, stringsAsFactors = FALSE)

# Wide form à long form
col_months <- names(df_wide)[-1]
raw_vals   <- as.character(unlist(df_wide[1, -1]))


dates <- as.Date(paste("01", col_months), format = "%d %B %Y")

# Supprimer les données nulles ".."
vals <- as.numeric(gsub(",", "", ifelse(trimws(raw_vals) == "..", NA_character_, raw_vals)))

df_raw <- data.frame(Date = dates, VALUE = vals)

# Extraire 112 points
df_filtered <- df_raw[df_raw$Date >= as.Date("2003-09-01") & df_raw$Date <= as.Date("2012-12-01"), ]
df_filtered <- df_filtered[order(df_filtered$Date), ]


# ---- 3. Conversion en objet ts ----
series_full <- ts(df_filtered$VALUE, start = c(2003, 9), frequency = 12)

# ---- 4. Découpage : entraînement (100 obs) et validation (12 obs) ----
series_train <- window(series_full, end   = c(2011, 12))
series_test  <- window(series_full, start = c(2012, 1))


# === Graphique de la série originale ===
fig_n <- fig_n + 1
png(file.path(img_dir, paste0("fig", fig_n, "_serie_brute.png")), width = 9,
    height = 4.5, units = "in", res = 400)
par(mfrow = c(1, 1))
ts.plot(series_train,
        main = "Série d'entraînement (100 observations)",
        ylab = "Nombre de bénéficiaires", xlab = "Année")
dev.off()

# ---- a) Transformation : analyse Box-Cox ----

# === Graphique Box Cox ===
fig_n <- fig_n + 1
png(file.path(img_dir, paste0("fig", fig_n, "_boxcox.png")),
    width = 9, height = 5, units = "in", res = 400)
par(mfrow = c(1, 1))
bc <- boxcox(series_train ~ 1, lambda = seq(-2, 2, by = 0.1))
title("Vraisemblance profilée Box-Cox")
dev.off()

lambda_opt <- bc$x[which.max(bc$y)]
cat("Lambda optimal (Box-Cox) :", lambda_opt, "\n")
# Si l'intervalle de confiance de lambda inclut 0,
# on adopte la transformation log.


# Transformation log
series_train_log <- log(series_train)


# === Comparaison visuelle des transformations ===
fig_n <- fig_n + 1
png(file.path(img_dir, paste0("fig", fig_n, "_comparaison_transfo.png")),
    width = 9, height = 7, units = "in", res = 400)
par(mfrow = c(2, 2))
ts.plot(series_train)
title("Sans transformation")
ts.plot(log(series_train))
title("Transformation log")
ts.plot(sqrt(series_train))
title("Transformation racine carrée")
ts.plot(1 / series_train)
title("Transformation inverse")
dev.off()











# ---- b) Identification : ACF / PACF ----


# === ACF/PACF de la série brute ===
fig_n <- fig_n + 1
png(file.path(img_dir, paste0("fig", fig_n, "_acf_pacf_brut.png")),
    width = 9, height = 12, units = "in", res = 400)
par(mfrow = c(4, 1))

# lag = 48 mois
acf(series_train,  lag.max = 48,
    main = "ACF — Série originale, Max lag = 48 (mois)")
pacf(series_train, lag.max = 48,
     main = "PACF — Série originale, Max lag = 48 (mois)")

# lag = 100 mois
acf(series_train, lag.max = 100,
    main = "ACF — Série originale, Max lag = 100 (mois)")
pacf(series_train, lag.max = 100,
     main = "PACF — Série originale Max lag = 100 (mois)")
dev.off()







# === L'allure de log série pour vérifier le trend ===
fig_n <- fig_n + 1
png(file.path(img_dir, paste0("fig", fig_n, "_logdiff_abline.png")),
    width = 9, height = 7, units = "in", res = 400)
time_year <- time(series_train_log)
trend_model_log <- lm(series_train_log ~ time_year)
ts.plot(series_train_log, 
        ylab = "Log(Nombre de bénéficiaires)", 
        main = "Série en log avec droite de tendance")
abline(trend_model_log, col = "red", lwd = 2, lty = 2)
dev.off()








# === Série log + première différence ===
fig_n <- fig_n + 1
png(file.path(img_dir, paste0("fig", fig_n, "_log_diff.png")),
    width = 9, height = 12, units = "in", res = 400)
par(mfrow = c(2, 1))
ts.plot(series_train_log, main = "Série transformée (log)",
        ylab = "log(Bénéficiaires)")
ts.plot(diff(series_train_log, lag = 12), main = "Série log différenciée (D = 1)",
        ylab = "diff log(Bénéficiaires)")
dev.off()






# Application des opérateurs (1-B) et (1-B^12)
series_train_log_diff  <- diff(series_train_log)
series_train_log_sdiff <- diff(diff(series_train_log, lag = 12))






# === ACF/PACF après différenciation ===
fig_n <- fig_n + 1
png(file.path(img_dir, paste0("fig", fig_n, "_acf_pacf_diff.png")),
    width = 9, height = 12, units = "in", res = 400)
par(mfrow = c(4, 1))
acf(series_train_log_sdiff,  lag.max = 48,
    main = "ACF — log, d = 1, D = 1")
pacf(series_train_log_sdiff, lag.max = 48,
     main = "PACF — log, d = 1, D = 1")

acf(series_train_log_sdiff,  lag.max = 100,
    main = "ACF — log, d = 1, D = 1")
pacf(series_train_log_sdiff, lag.max = 100,
     main = "PACF — log, d = 1, D = 1")
dev.off()






# === Comparaison ACF/PACF sans et avec transformation ===
series_train_diff_sdiff     <- diff(diff(series_train), lag = 12)
series_train_log_diff_sdiff <- diff(diff(series_train_log), lag = 12)


fig_n <- fig_n + 1
png(file.path(img_dir, paste0("fig", fig_n, "_comparaison_acf.png")),
    width = 9, height = 7, units = "in", res = 400)
par(mfrow = c(2, 2))
acf(series_train_diff_sdiff, lag.max = 48,
    main = "ACF — Sans transformation")
acf(series_train_log_diff_sdiff, lag.max = 48,
    main = "ACF — Avec transformation (log)")
pacf(series_train_diff_sdiff, lag.max = 48,
     main = "PACF — Sans transformation")
pacf(series_train_log_diff_sdiff, lag.max = 48,
     main = "PACF — Avec transformation (log)")
dev.off()


# Test de theta0
# Wt = (1-B)(1-B^12)log(Z_t)
w_t <- diff(diff(series_train_log), lag = 12)

w_bar <- mean(w_t, na.rm = TRUE)
n <- length(na.omit(w_t))

# sigma_W_bar
se_w_bar <- sd(w_t, na.rm = TRUE) / sqrt(n)

# 4. T stat 
t_stat <- abs(w_bar / se_w_bar)

cat("\n--- Test de theta0 ---\n")
cat("W_bar = ", w_bar, "\n")
cat("T = ", t_stat, "\n")
cat("Resultat : ", ifelse(t_stat > 1.96, "Rejetter H0 (theta0 != 0)", "Accepter H0 (theta0 = 0)"), "\n")











# ---- c) Ajustement du modèle SARIMA(1,0,0)(2,1,0)_12 ----

model_fit1 <- arima(series_train_log,
                   order    = c(1, 1, 0),
                   seasonal = list(order = c(2, 1, 0), period = 12))
print(model_fit1)

model_fit2 <- arima(series_train_log,
                   order    = c(0, 1, 1),
                   seasonal = list(order = c(0, 1, 1), period = 12))
print(model_fit2)


# Tentatives erronées
# model_fit3 <- arima(series_train_log,
#                    order    = c(2, 1, 1),
#                    seasonal = list(order = c(2, 1, 2), period = 12))
# print(model_fit3)

# model_fit4 <- arima(series_train_log,
#                    order    = c(2, 1, 0),
#                    seasonal = list(order = c(2, 1, 1), period = 12))
# print(model_fit4)

# model_fit5 <- arima(series_train_log,
#                    order    = c(1, 1, 0),
#                    seasonal = list(order = c(2, 1, 1), period = 12))
# print(model_fit5)



# === ACF/PACF des résidus ===
fig_n <- fig_n + 1
png(file.path(img_dir, paste0("fig", fig_n, "_resid_acf.png")),
    width = 9, height = 6, units = "in", res = 400)
par(mfrow = c(2, 1))
acf(model_fit1$residuals,  lag.max = 48, main = "ACF des résidus")
pacf(model_fit1$residuals, lag.max = 48, main = "PACF des résidus")
dev.off()




# Test de Ljung-Box (H0 : résidus non corrélés = bruit blanc)
ljung_box <- Box.test(model_fit1$residuals, lag = 24, type = "Ljung-Box")
print(ljung_box)



# ---- d) & e) Prévisions sur 12 mois et intervalles de confiance à 95 % ----

fc <- predict(model_fit1, n.ahead = 12)

# Retour à l'échelle originale (exp)
fc_vals   <- exp(fc$pred)
ci_lower  <- exp(fc$pred - 1.96 * fc$se)
ci_upper  <- exp(fc$pred + 1.96 * fc$se)

cat("\nPrévisions (échelle originale) :\n")
print(data.frame(
  Date     = time(fc_vals),
  Prevision = as.numeric(fc_vals),
  IC_inf   = as.numeric(ci_lower),
  IC_sup   = as.numeric(ci_upper),
  Real = as.numeric(series_test),
  In_IC = (series_test >= ci_lower) & (series_test <= ci_upper)
))

# === Prévisions vs données réelles ===
fig_n <- fig_n + 1
png(file.path(img_dir, paste0("fig", fig_n, "_previsions.png")),
    width = 10, height = 5, units = "in", res = 400)
par(mfrow = c(1, 1))
ts.plot(series_train,
        xlim = c(2003, 2013),
        ylim = c(min(series_train, ci_lower) * 0.97,
                 max(series_train, ci_upper) * 1.03),
        main = "Prévisions — SARIMA(1,1,0)(2,1,0)[12]",
        ylab = "Nombre de bénéficiaires", xlab = "Année")
lines(fc_vals, col = "red",   lwd = 2)
lines(ci_lower, col = "blue",  lty = 2)
lines(ci_upper, col = "blue",  lty = 2)
lines(series_test, col = "darkgreen", lwd = 2)
legend("topright",
       legend = c("Entraînement (100)", "Prévision", "IC 95 %",
                  "Réel (validation 12)"),
       col = c("black", "red", "blue", "darkgreen"),
       lty = c(1, 1, 2, 1),
       lwd = c(1, 2, 1, 2),
       cex = 0.85)
dev.off()
