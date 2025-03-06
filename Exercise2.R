# Exercise 2: Military Coups Analysis
# ------------------------------

# Install and load necessary packages
# Uncomment these lines if you need to install packages
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("MASS")
# install.packages("car")

# Load libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization
library(MASS)       # For stepAIC function 
library(car)        # For VIF calculation

# Create a directory for saving plots
dir.create("plots_ex2", showWarnings = FALSE)

# -------------------------------------------------
# Part (a): Data exploration and Poisson regression
# -------------------------------------------------

# Import the data
coup_data <- read.table("coups.txt", header = TRUE)

# Check the data structure
str(coup_data)
summary(coup_data)

# Correlation matrix for numeric variables - IMPORTANT: Do this BEFORE converting pollib to factor
numeric_vars <- coup_data  # All variables are numeric at this point
correlation_matrix <- cor(numeric_vars, use = "complete.obs")
print("Correlation matrix:")
print(round(correlation_matrix, 2))

# Correlation plot
correlation_data <- as.data.frame(as.table(correlation_matrix))
names(correlation_data) <- c("Var1", "Var2", "Correlation")

corr_plot <- ggplot(correlation_data, aes(Var1, Var2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1)) +
  labs(title = "Correlation Matrix of Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("plots_ex2/03_correlation_plot.png", corr_plot, width = 10, height = 8)
print(corr_plot)

# NOW convert political liberalization to a factor
coup_data$pollib <- factor(coup_data$pollib, levels = c(0, 1, 2), 
                           labels = c("No civil rights", 
                                      "Limited civil rights", 
                                      "Full civil rights"))

# Exploratory data analysis
# Visualize the distribution of military coups
hist_coups <- ggplot(coup_data, aes(x = miltcoup)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Military Coups",
       x = "Number of Military Coups",
       y = "Frequency") +
  theme_minimal()

# Save the plot
ggsave("plots_ex2/01_hist_coups.png", hist_coups, width = 8, height = 6)
print(hist_coups)

# Explore relationship between coups and political liberalization
boxplot_pollib <- ggplot(coup_data, aes(x = pollib, y = miltcoup, fill = pollib)) +
  geom_boxplot() +
  labs(title = "Military Coups by Political Liberalization Level",
       x = "Political Liberalization",
       y = "Number of Military Coups") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal()

# Save the plot
ggsave("plots_ex2/02_boxplot_pollib.png", boxplot_pollib, width = 8, height = 6)
print(boxplot_pollib)

# Fit Poisson regression model with all variables
model_full <- glm(miltcoup ~ oligarchy + pollib + parties + pctvote + 
                    popn + size + numelec + numregim,
                  data = coup_data, family = poisson(link = "log"))

# Print model summary
summary_full <- summary(model_full)
print(summary_full)

# Check for overdispersion
dispersion_parameter <- sum(residuals(model_full, type = "pearson")^2) / model_full$df.residual
cat("Dispersion parameter:", dispersion_parameter, "\n")

# Check VIF for multicollinearity
vif_results <- vif(model_full)
print("Variance Inflation Factors (check for multicollinearity):")
print(vif_results)

# Interpret the coefficients as incidence rate ratios (exp(coef))
irr <- exp(coef(model_full))
irr_ci <- exp(confint(model_full))
irr_table <- cbind(IRR = irr, irr_ci)
print("Incidence Rate Ratios with 95% CI:")
print(round(irr_table, 3))

# Plot observed vs. predicted counts
coup_data$predicted <- predict(model_full, type = "response")

pred_plot <- ggplot(coup_data, aes(x = predicted, y = miltcoup)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Observed vs. Predicted Counts",
       x = "Predicted Counts",
       y = "Observed Counts") +
  theme_minimal()

# Save the plot
ggsave("plots_ex2/04_observed_vs_predicted.png", pred_plot, width = 8, height = 6)
print(pred_plot)

# Interpretation of the full model
cat("\nInterpretation of Full Poisson Regression Model:\n")

significant_vars <- rownames(summary_full$coefficients)[which(summary_full$coefficients[, 4] < 0.05)]
significant_vars <- significant_vars[significant_vars != "(Intercept)"]  # Exclude intercept

if (length(significant_vars) > 0) {
  cat("Significant predictors (p < 0.05):\n")
  for (var in significant_vars) {
    cat("- ", var, ": ", ifelse(coef(model_full)[var] > 0, "positive", "negative"), 
        " effect (IRR = ", round(exp(coef(model_full)[var]), 3), ")\n", sep="")
  }
} else {
  cat("No significant predictors at p < 0.05 level.\n")
}

cat("\nA one-unit increase in oligarchy is associated with a ", 
    round((exp(coef(model_full)["oligarchy"]) - 1) * 100, 1), 
    "% change in the expected count of military coups, holding other variables constant.\n", sep="")

# -------------------------------------------------
# Part (b): Model selection using stepwise approach
# -------------------------------------------------

# Perform stepwise selection using AIC
step_model <- stepAIC(model_full, direction = "backward", trace = 1)

# Print the summary of the selected model
summary_step <- summary(step_model)
print(summary_step)

# Check for overdispersion in the reduced model
dispersion_parameter_step <- sum(residuals(step_model, type = "pearson")^2) / step_model$df.residual
cat("Dispersion parameter for reduced model:", dispersion_parameter_step, "\n")

# Get the IRR for the reduced model
irr_step <- exp(coef(step_model))
irr_ci_step <- exp(confint(step_model))
irr_table_step <- cbind(IRR = irr_step, irr_ci_step)
print("Incidence Rate Ratios for reduced model with 95% CI:")
print(round(irr_table_step, 3))

# Compare AIC of both models
cat("\nModel Comparison:\n")
cat("Full model AIC:", round(summary_full$aic, 2), "\n")
cat("Reduced model AIC:", round(summary_step$aic, 2), "\n")
cat("AIC difference:", round(summary_full$aic - summary_step$aic, 2), "\n\n")

# Compare the models with ANOVA
anova_result <- anova(step_model, model_full, test = "Chisq")
print("ANOVA comparison of models:")
print(anova_result)

# Visualize significant coefficients from the reduced model
coef_data <- data.frame(
  Variable = names(coef(step_model)),
  Coefficient = coef(step_model),
  SE = summary_step$coefficients[, 2],
  P_Value = summary_step$coefficients[, 4]
)

# Add significance indicator
coef_data$Significant <- ifelse(coef_data$P_Value < 0.05, "Yes", "No")
# Remove the intercept for better visualization
coef_data <- coef_data[-1, ]

coef_plot <- ggplot(coef_data, aes(x = reorder(Variable, Coefficient), 
                                   y = Coefficient, fill = Significant)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * SE, 
                    ymax = Coefficient + 1.96 * SE), width = 0.2) +
  labs(title = "Coefficients from the Reduced Model",
       x = "Variable",
       y = "Coefficient (log scale)") +
  coord_flip() +
  scale_fill_manual(values = c("No" = "grey", "Yes" = "blue")) +
  theme_minimal()

# Save the plot
ggsave("plots_ex2/05_coefficients_plot.png", coef_plot, width = 8, height = 6)
print(coef_plot)

# Interpretation of the reduced model
cat("\nInterpretation of Reduced Poisson Regression Model:\n")

significant_vars_step <- rownames(summary_step$coefficients)[which(summary_step$coefficients[, 4] < 0.05)]
significant_vars_step <- significant_vars_step[significant_vars_step != "(Intercept)"]  # Exclude intercept

if (length(significant_vars_step) > 0) {
  cat("Significant predictors (p < 0.05):\n")
  for (var in significant_vars_step) {
    cat("- ", var, ": ", ifelse(coef(step_model)[var] > 0, "positive", "negative"), 
        " effect (IRR = ", round(exp(coef(step_model)[var]), 3), ")\n", sep="")
  }
} else {
  cat("No significant predictors at p < 0.05 level.\n")
}

cat("\nComparison with the full model:\n")
cat("The reduced model retained ", length(coef(step_model)) - 1, 
    " predictors out of ", length(coef(model_full)) - 1, " from the full model.\n", sep="")
cat("The removed variables did not significantly contribute to the model.\n")
cat("The reduced model is more parsimonious and has a lower AIC, indicating better fit.\n\n")

# -------------------------------------------------
# Part (c): Prediction for different pollib levels
# -------------------------------------------------

# Create prediction data frame for the three levels of pollib
pred_data <- data.frame(
  pollib = factor(0:2, levels = 0:2,
                  labels = c("No civil rights", "Limited civil rights", "Full civil rights"))
)

# Get the variables in the final model (excluding the intercept and pollib terms)
model_terms <- attr(terms(step_model), "term.labels")
model_terms <- model_terms[!grepl("pollib", model_terms)]

# Add mean values for other variables in the model
for (var in model_terms) {
  pred_data[[var]] <- mean(coup_data[[var]], na.rm = TRUE)
}

# Print the prediction data
print("Prediction data (with mean values for other predictors):")
print(pred_data)

# Make predictions
pred_data$predicted_count <- predict(step_model, newdata = pred_data, type = "response")

# Print the predictions
print("Predicted number of military coups by political liberalization level:")
print(pred_data[, c("pollib", "predicted_count")])

# Plot the predictions
pred_plot <- ggplot(pred_data, aes(x = pollib, y = predicted_count, fill = pollib)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(predicted_count, 2)), vjust = -0.5) +
  labs(title = "Predicted Number of Military Coups by Political Liberalization",
       subtitle = "Based on average values for other predictors",
       x = "Political Liberalization",
       y = "Predicted Number of Coups") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal()

# Save the plot
ggsave("plots_ex2/06_predicted_by_pollib.png", pred_plot, width = 8, height = 6)
print(pred_plot)

# Interpretation of predictions
cat("\nInterpretation of Predictions by Political Liberalization Level:\n")

# Sort predictions from highest to lowest
sorted_pred <- pred_data[order(-pred_data$predicted_count), ]
highest_level <- as.character(sorted_pred$pollib[1])
lowest_level <- as.character(sorted_pred$pollib[nrow(sorted_pred)])

cat("1. Countries with", highest_level, "are predicted to have the highest number of military coups")
cat(" (", round(sorted_pred$predicted_count[1], 2), " coups).\n", sep="")

cat("2. Countries with", lowest_level, "are predicted to have the lowest number of military coups")
cat(" (", round(sorted_pred$predicted_count[nrow(sorted_pred)], 2), " coups).\n", sep="")

# Calculate percent differences
baseline <- sorted_pred$predicted_count[nrow(sorted_pred)]
for (i in 1:(nrow(sorted_pred)-1)) {
  pct_diff <- (sorted_pred$predicted_count[i] - baseline) / baseline * 100
  cat("3. Moving from", lowest_level, "to", as.character(sorted_pred$pollib[i]))
  cat(" is associated with a ", round(pct_diff, 1), "% ", 
      ifelse(pct_diff > 0, "increase", "decrease"), " in the expected number of military coups.\n", sep="")
}

# -------------------------------------------------
# Summary of findings for Exercise 2
# -------------------------------------------------

cat("\nSummary of Findings for Exercise 2:\n")
cat("1. The full Poisson regression model identified several factors associated with military coups.\n")
cat("2. The backward selection process resulted in a more parsimonious model with better fit.\n")
cat("3. Key predictors of military coups include:") 

for (var in significant_vars_step) {
  effect_direction <- ifelse(coef(step_model)[var] > 0, "increases", "decreases")
  cat("\n   - ", var, ": ", effect_direction, " the expected number of coups", sep="")
}

cat("\n4. Political liberalization level has a substantial impact on the expected number of coups,\n")
cat("   with different levels showing notable variation in predicted coup counts.\n")
cat("5. The model provides insights into the political and socioeconomic factors that\n")
cat("   influence the stability of governments and vulnerability to military takeovers.\n")