
library(MASS)

pharma$working_exp_years = (pharma$`Years of working experience`)
pharma$working_exp_years[is.na(pharma$working_exp_years)] = mean(pharma$working_exp_years,na.rm = TRUE)

# Read your data
# df <- read.csv("pharmacy_data.csv")

# Convert categorical variables to factors
pharma$province <- as.factor(pharma$Provinces)
pharma$pharmacy_class <- as.factor(pharma$Class)

pharma$gppscore
pharma$unregistered
pharma$abx_n_total

# Fit negative binomial model with offset
m1_nb<- glm.nb(
  pharma$unregistered ~ 
    # pharma$gppscore +
    pharma$province +
    pharma$pharmacy_class +
    pharma$working_exp_years+
    # pharma$Urban +
    # pharma$`Licensed Pharmacist`+
    # pharma$`Distance to nearest hospital (KM)`+
    offset(log(pharma$abx_n_total))
)

# View model summary
summary(m1_nb)



m1_pois <- glm(pharma$unregistered ~ 
    # pharma$gppscore +
    pharma$province +
    pharma$pharmacy_class +
    pharma$working_exp_years+
    # pharma$Urban +
    # pharma$`Licensed Pharmacist`+
    # pharma$`Distance to nearest hospital (KM)`+
    offset(log(pharma$abx_n_total)),
           family = poisson
    )

summary(m1_pois)

AIC(m1_pois, m1_nb)

# Overdispersion test
dispersion <- sum(residuals(m1_pois, type = "pearson")^2) / m1_pois$df.residual
dispersion

plot(fitted(m1_nb), residuals(m1_nb, type = "pearson"),
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Pearson residuals
res <- residuals(m1_nb, type = "pearson")

# Identify large residuals
which(abs(res) > 2)


cook <- cooks.distance(m1_pois)
# Plot
plot(cook, type = "h", main = "Cook's distance")
# Threshold
abline(h = 4/length(cook), col = "red")
# Identify influential points
which(cook > (4/length(cook)))


# Predicted counts
pred <- predict(m1_pois, type = "response")
plot(pharma$unregistered, pred,
     xlab = "Observed",
     ylab = "Predicted",
     main = "Observed vs Predicted")
abline(0, 1, col = "red")

pharma_nooutlier <- pharma[-which(cook > (4/length(cook))),]

m1_pois_nooutlier <- glm(pharma_nooutlier$unregistered ~ 
                 # pharma_nooutlier$gppscore +
                 pharma_nooutlier$province +
                 pharma_nooutlier$pharmacy_class +
                 pharma_nooutlier$working_exp_years+
                 # pharma_nooutlier$Urban +
                 pharma_nooutlier$`Licensed Pharmacist`+
                 pharma_nooutlier$`Distance to nearest hospital (KM)`+
                 offset(log(pharma_nooutlier$abx_n_total)),
               family = poisson
)


summary(m1_pois_nooutlier)


m1_pois_full <- glm(pharma_nooutlier$unregistered ~ 
                      # pharma_nooutlier$gppscore +
                      pharma_nooutlier$Sex +
                      pharma_nooutlier$abx_watch +
                      pharma_nooutlier$province +
                      pharma_nooutlier$pharmacy_class +
                      pharma_nooutlier$working_exp_years+
                      pharma_nooutlier$Urban +
                      pharma_nooutlier$`Licensed Pharmacist`+
                      pharma_nooutlier$`Distance to nearest hospital (KM)`+
                      offset(log(pharma_nooutlier$abx_n_total)),
                    family = poisson
)
summary(m1_pois_full)
model_pois_step <- step(m1_pois_full, direction = "both")
summary(model_pois_step)

m1_pois_final <- glm(pharma_nooutlier$unregistered ~ 
                      # pharma_nooutlier$gppscore +
                      # pharma_nooutlier$Sex +
                      # pharma_nooutlier$abx_watch +
                      pharma_nooutlier$province +
                      pharma_nooutlier$pharmacy_class +
                      # pharma_nooutlier$working_exp_years+
                      # pharma_nooutlier$Urban +
                      # pharma_nooutlier$`Licensed Pharmacist`+
                      # pharma_nooutlier$`Distance to nearest hospital (KM)`+
                      offset(log(pharma_nooutlier$abx_n_total)),
                    family = poisson
)

summary(m1_pois_final)


# Overdispersion test
dispersion <- sum(residuals(m1_pois_final, type = "pearson")^2) / m1_pois_final$df.residual
dispersion

# AIC(m1_pois, m1_nb,m1_pois_final)

cook <- cooks.distance(m1_pois_final)
# Plot
plot(cook, type = "h", main = "Cook's distance")
# Threshold
abline(h = 4/length(cook), col = "red")
# Identify influential points
which(cook > (4/length(cook)))


# Coefficients
coef_est <- coef(m1_pois_final)
# IRRs
IRR <- exp(coef_est)
# Confidence intervals
CI <- exp(confint(m1_pois_final))
# Combine into table
results <- cbind(IRR, CI)
# Clean column names
colnames(results) <- c("IRR", "CI_lower", "CI_upper")
# View results
print(results)


# Residual plot
plot(fitted(m1_pois_final), residuals(m1_pois_final),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residual plot")
abline(h = 0, col = "red")


# no used in the end
m1_nb_final <- glm.nb(pharma_nooutlier$unregistered ~ 
                        # pharma_nooutlier$gppscore +
                        # pharma_nooutlier$Sex +
                        # pharma_nooutlier$abx_watch +
                        pharma_nooutlier$province +
                        pharma_nooutlier$pharmacy_class +
                        # pharma_nooutlier$working_exp_years+
                        # pharma_nooutlier$Urban +
                        # pharma_nooutlier$`Licensed Pharmacist`+
                        # pharma_nooutlier$`Distance to nearest hospital (KM)`+
                       offset(log(pharma_nooutlier$abx_n_total))
)

summary(m1_nb_final)

AIC(m1_pois_final, m1_nb_final)
length(coef(m1_pois_final))




# Extract coefficients
coef_est <- coef(m1_pois_final)

# Standard errors
se <- sqrt(diag(vcov(m1_pois_final)))

# z-values
z <- coef_est / se

# p-values
p <- 2 * (1 - pnorm(abs(z)))

# IRR
IRR <- exp(coef_est)

# Confidence intervals (for IRR)
CI_lower <- exp(coef_est - 1.96 * se)
CI_upper <- exp(coef_est + 1.96 * se)

# Combine into table
results_table <- data.frame(
  Variable = names(coef_est),
  Beta = coef_est,
  IRR = IRR,
  CI_lower = CI_lower,
  CI_upper = CI_upper,
  p_value = p,
  row.names = NULL
)

results_table$Variable <- gsub("pharma_nooutlier\\$", "", results_table$Variable)

results_table$Variable[results_table$Variable == "(Intercept)"] <- "Intercept"
results_table$Variable[results_table$Variable == "provinceLuangprabang"] <- "Province: Luang Prabang"
results_table$Variable[results_table$Variable == "provinceVientiane Capital"] <- "Province: Vientiane Capital"
results_table$Variable[results_table$Variable == "pharmacy_class2"] <- "Pharmacy class (2 vs 1)"

# Round values
results_table$Beta <- round(results_table$Beta, 3)
results_table$IRR <- round(results_table$IRR, 2)
results_table$CI_lower <- round(results_table$CI_lower, 2)
results_table$CI_upper <- round(results_table$CI_upper, 2)

# Format p-values
results_table$p_value <- ifelse(results_table$p_value < 0.001, "<0.001", round(results_table$p_value, 3))

# Combine CI
results_table$CI <- paste0(results_table$CI_lower, "–", results_table$CI_upper)

# Final table
final_table <- results_table[, c("Variable", "Beta", "IRR", "CI", "p_value")]

print(final_table)

write.csv(x = final_table,file = 'output/model_unregistered_pois.csv',row.names = FALSE)


# Extract coefficients
coef_est <- coef(m1_nb_final)

# Standard errors
se <- sqrt(diag(vcov(m1_nb_final)))

# z-values
z <- coef_est / se

# p-values
p <- 2 * (1 - pnorm(abs(z)))

# IRR
IRR <- exp(coef_est)

# Confidence intervals (for IRR)
CI_lower <- exp(coef_est - 1.96 * se)
CI_upper <- exp(coef_est + 1.96 * se)

# Combine into table
results_table_nb <- data.frame(
  Variable = names(coef_est),
  Beta = coef_est,
  IRR = IRR,
  CI_lower = CI_lower,
  CI_upper = CI_upper,
  p_value = p,
  row.names = NULL
)

results_table_nb$Variable <- gsub("pharma_nooutlier\\$", "", results_table_nb$Variable)

results_table_nb$Variable[results_table_nb$Variable == "(Intercept)"] <- "Intercept"
results_table_nb$Variable[results_table_nb$Variable == "provinceLuangprabang"] <- "Province: Luang Prabang"
results_table_nb$Variable[results_table_nb$Variable == "provinceVientiane Capital"] <- "Province: Vientiane Capital"
results_table_nb$Variable[results_table_nb$Variable == "pharmacy_class2"] <- "Pharmacy class (2 vs 1)"

# Round values
results_table_nb$Beta <- round(results_table_nb$Beta, 3)
results_table_nb$IRR <- round(results_table_nb$IRR, 2)
results_table_nb$CI_lower <- round(results_table_nb$CI_lower, 2)
results_table_nb$CI_upper <- round(results_table_nb$CI_upper, 2)

# Format p-values
results_table_nb$p_value <- ifelse(results_table_nb$p_value < 0.001, "<0.001", round(results_table_nb$p_value, 3))

# Combine CI
results_table_nb$CI <- paste0(results_table_nb$CI_lower, "–", results_table_nb$CI_upper)

# Final table
final_table_nb <- results_table_nb[, c("Variable", "Beta", "IRR", "CI", "p_value")]

print(final_table_nb)

write.csv(x = final_table_nb,file = 'output/model_unregistered_nb.csv',row.names = FALSE)


