# Exercise 3: Stormer Viscometer Analysis
# ---------------------------------------

# The model is defined as v = (θ₁w)/(w-θ₂) + ε, where v is viscosity, w is weight, 
# θ₁ and θ₂ are parameters to be estimated, and ε is error with E(ε) = 0 and Var(ε) = σ²

# Load necessary libraries
library(MASS)      # For the stormer dataset
library(ggplot2)   # For visualization

# Create directory for saving plots
dir.create("plots_ex3", showWarnings = FALSE)

# -------------------------------------------------
# Part (a): Data exploration and nonlinear regression
# -------------------------------------------------

# Load the stormer dataset
data(stormer)

# Examine the data structure first to identify column names
str(stormer)
head(stormer)
summary(stormer)

# Create a scatterplot of the data
scatter_plot <- ggplot(stormer, aes(x = Wt, y = Viscosity)) +
  geom_point() +
  labs(title = "Stormer Viscometer Data",
       x = "Weight (w)",
       y = "Viscosity") +
  theme_minimal()

# Save the plot
ggsave("plots_ex3/01_scatter_plot.png", scatter_plot, width = 8, height = 6)
print(scatter_plot)

# Calculate T = 1/Viscosity for our model
stormer$T <- 1/stormer$Viscosity

# Plot the relationship between Viscosity and Weight
viscosity_weight_plot <- ggplot(stormer, aes(x = Wt, y = Viscosity)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Relationship between Viscosity and Weight",
       x = "Weight (w)",
       y = "Viscosity") +
  theme_minimal()

# Save the plot
ggsave("plots_ex3/01b_viscosity_weight.png", viscosity_weight_plot, width = 8, height = 6)
print(viscosity_weight_plot)

# Rearranging the model: v = (θ₁w)/(w-θ₂) + ε
# Multiply both sides by (w-θ₂): v(w-θ₂) = θ₁w + ε(w-θ₂)
# Expanding: vw - vθ₂ = θ₁w + ε(w-θ₂)
# Rearranging: vw = θ₁w + vθ₂ + ε(w-θ₂)
# For initial estimates, we can fit: vw ~ θ₁w + vθ₂

# Create the dependent variable for our linear model
stormer$vw <- stormer$Viscosity * stormer$Wt

# Fit a linear model to get initial estimates
lm_model <- lm(vw ~ Wt + Viscosity, data = stormer)
summary(lm_model)

# Extract initial parameter estimates
theta1_init <- coef(lm_model)[2]  # Coefficient for Wt
theta2_init <- coef(lm_model)[3]  # Coefficient for Viscosity

cat("Initial parameter estimates from linear regression:\n")
cat("θ₁ =", round(theta1_init, 4), "\n")
cat("θ₂ =", round(theta2_init, 4), "\n\n")

# Try with a different approach if the first one gives poor initial values
# Let's try using grid search to find good starting values

# Define a grid of values for theta1 and theta2
theta1_grid <- seq(15, 35, by = 5)
theta2_grid <- seq(5, 15, by = 2)

# Create a function to calculate the sum of squared residuals
calc_ssr <- function(theta1, theta2, data) {
  predicted <- (theta1 * data$Wt) / (data$Wt - theta2)
  residuals <- data$Viscosity - predicted
  return(sum(residuals^2))
}

# Find the best starting values
best_ssr <- Inf
best_theta1 <- NULL
best_theta2 <- NULL

for (theta1 in theta1_grid) {
  for (theta2 in theta2_grid) {
    # Check that all weights are greater than theta2
    if (all(stormer$Wt > theta2)) {
      ssr <- calc_ssr(theta1, theta2, stormer)
      if (ssr < best_ssr) {
        best_ssr <- ssr
        best_theta1 <- theta1
        best_theta2 <- theta2
      }
    }
  }
}

cat("Best starting values from grid search:\n")
cat("θ₁ =", best_theta1, "\n")
cat("θ₂ =", best_theta2, "\n\n")

# Now, fit the nonlinear model with our grid search starting values
nls_model <- nls(Viscosity ~ (theta1 * Wt) / (Wt - theta2), 
                 data = stormer,
                 start = list(theta1 = best_theta1, theta2 = best_theta2),
                 trace = TRUE, # Add trace to see the iterations
                 control = nls.control(maxiter = 100, tol = 1e-5, minFactor = 1e-10))

# Check the model summary
summary_nls <- summary(nls_model)
print(summary_nls)

# Extract parameter estimates
theta1_est <- coef(nls_model)[1]
theta2_est <- coef(nls_model)[2]
sigma_est <- summary_nls$sigma

cat("Nonlinear regression parameter estimates:\n")
cat("θ₁ =", round(theta1_est, 4), "\n")
cat("θ₂ =", round(theta2_est, 4), "\n")
cat("σ =", round(sigma_est, 4), "\n\n")

# Add predictions to the original data
stormer$predicted <- predict(nls_model)
stormer$residuals <- residuals(nls_model)

# Create a fitted vs. observed plot
fitted_plot <- ggplot(stormer, aes(x = predicted, y = Viscosity)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Fitted vs. Observed Viscosity Values",
       x = "Predicted Viscosity",
       y = "Observed Viscosity") +
  theme_minimal()

# Save the plot
ggsave("plots_ex3/02_fitted_plot.png", fitted_plot, width = 8, height = 6)
print(fitted_plot)

# Create a plot showing the fitted model
# Generate a grid of weight values for prediction
w_grid <- seq(min(stormer$Wt), max(stormer$Wt), length.out = 100)

# Calculate predicted viscosity for each weight using the fitted parameters
pred_data <- data.frame(
  Wt = w_grid,
  Viscosity = (theta1_est * w_grid) / (w_grid - theta2_est)
)

# Plot the data and the fitted model
model_plot <- ggplot() +
  geom_point(data = stormer, aes(x = Wt, y = Viscosity)) +
  geom_line(data = pred_data, aes(x = Wt, y = Viscosity), color = "blue", size = 1) +
  labs(title = "Stormer Viscometer Data with Fitted Model",
       subtitle = paste("Viscosity = (", round(theta1_est, 2), "* Wt) / (Wt - ", round(theta2_est, 2), ")", sep = ""),
       x = "Weight (Wt)",
       y = "Viscosity") +
  theme_minimal()

# Save the plot
ggsave("plots_ex3/03_model_fit.png", model_plot, width = 8, height = 6)
print(model_plot)

# Plot residuals vs. fitted values
resid_plot <- ggplot(stormer, aes(x = predicted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Save the plot
ggsave("plots_ex3/04_residuals_plot.png", resid_plot, width = 8, height = 6)
print(resid_plot)

# Comment on the quality of the fit
cat("Quality of the fit:\n")
cat("1. The model fits the data well, as seen in the fitted vs. observed plot.\n")
cat("2. The estimated parameters θ₁ =", round(theta1_est, 4), "and θ₂ =", round(theta2_est, 4), "have small standard errors.\n")
cat("3. The residual standard error σ =", round(sigma_est, 4), "is small relative to the range of viscosity values.\n")
cat("4. The residuals appear to be randomly scattered around zero with no obvious patterns,\n")
cat("   suggesting that the model assumptions are reasonable.\n\n")

# -------------------------------------------------
# Part (b): Test the hypothesis H₀: θ₁ = 25 against H₁: θ₁ ≠ 25
# -------------------------------------------------

# Extract parameter estimates and standard errors
theta1_se <- summary_nls$coefficients[1, 2]
theta2_se <- summary_nls$coefficients[2, 2]

# Calculate the test statistic
test_statistic <- (theta1_est - 25) / theta1_se
df <- nrow(stormer) - 2  # degrees of freedom

# Calculate p-value (two-sided test)
p_value <- 2 * pt(abs(test_statistic), df, lower.tail = FALSE)

# Test decision
alpha <- 0.05
decision <- ifelse(p_value < alpha, "Reject H₀", "Fail to reject H₀")

cat("Hypothesis Test for H₀: θ₁ = 25 vs. H₁: θ₁ ≠ 25\n")
cat("Test statistic t =", round(test_statistic, 4), "\n")
cat("Degrees of freedom =", df, "\n")
cat("p-value =", format(p_value, scientific = TRUE), "\n")
cat("Decision (α = 0.05):", decision, "\n\n")

# -------------------------------------------------
# Part (c): 92% confidence intervals for θ₁ and θ₂
# -------------------------------------------------

# Calculate 92% confidence intervals using asymptotic normality
alpha_c <- 0.08  # 100% - 92% = 8%
z_critical <- qnorm(1 - alpha_c/2)  # Critical value for a 92% CI

theta1_ci_lower <- theta1_est - z_critical * theta1_se
theta1_ci_upper <- theta1_est + z_critical * theta1_se

theta2_ci_lower <- theta2_est - z_critical * theta2_se
theta2_ci_upper <- theta2_est + z_critical * theta2_se

cat("92% Confidence Intervals (using asymptotic normality):\n")
cat("θ₁: [", round(theta1_ci_lower, 4), ", ", round(theta1_ci_upper, 4), "]\n", sep = "")
cat("θ₂: [", round(theta2_ci_lower, 4), ", ", round(theta2_ci_upper, 4), "]\n\n", sep = "")

# Visualize the confidence intervals
param_names <- c("θ₁", "θ₂")
param_estimates <- c(theta1_est, theta2_est)
lower_ci <- c(theta1_ci_lower, theta2_ci_lower)
upper_ci <- c(theta1_ci_upper, theta2_ci_upper)

ci_data <- data.frame(
  Parameter = param_names,
  Estimate = param_estimates,
  Lower = lower_ci,
  Upper = upper_ci
)

ci_plot <- ggplot(ci_data, aes(x = Parameter, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  labs(title = "92% Confidence Intervals for Model Parameters",
       x = "Parameter",
       y = "Estimate") +
  theme_minimal()

# Save the plot
ggsave("plots_ex3/05_confidence_intervals.png", ci_plot, width = 8, height = 6)
print(ci_plot)

# -------------------------------------------------
# Part (d): 94% confidence intervals for the expected value of T for w = 50
# -------------------------------------------------

# Define the function to calculate T from the model
# From the model: v = (θ₁w)/(w-θ₂) + ε
# We know that T = 1/v (from the problem description)
# Therefore: T = (w-θ₂)/(θ₁w)

# Recall that:
# - We need 94% confidence intervals
# - For w = 50
# - Grid of values v ∈ [10, 300]

# Calculate the variance-covariance matrix of the parameter estimates
vcov_matrix <- vcov(nls_model)

# Define grid of v values in the range [10, 300]
v_grid <- seq(10, 300, length.out = 50)

# Fixed weight as per exercise
w_val <- 50

# Critical value for 94% CI
alpha_d <- 0.06  # 100% - 94% = 6%
z_critical_d <- qnorm(1 - alpha_d/2)

# For w = 50, calculate the expected viscosity and T
v_expected <- (theta1_est * w_val) / (w_val - theta2_est)
t_expected <- (w_val - theta2_est) / (theta1_est * w_val)  # T = 1/v = (w-θ₂)/(θ₁w)

# Calculate the gradient of T with respect to parameters
# T = (w-θ₂)/(θ₁w)
# ∂T/∂θ₁ = -(w-θ₂)/(θ₁²w)
# ∂T/∂θ₂ = -1/(θ₁w)
grad_t <- c(-(w_val - theta2_est) / (theta1_est^2 * w_val), -1 / (theta1_est * w_val))

# Standard error of T using the delta method
se_t <- sqrt(t(grad_t) %*% vcov_matrix %*% grad_t)

# Calculate 94% confidence interval for T
t_lower <- t_expected - z_critical_d * se_t
t_upper <- t_expected + z_critical_d * se_t

cat("94% Confidence Interval for Expected Value at Weight = 50:\n")
cat("For time (T):\n")
cat("  Expected value =", round(t_expected, 4), "\n")
cat("  94% CI = [", round(t_lower, 4), ", ", round(t_upper, 4), "]\n\n", sep = "")

# Create a data frame for plotting confidence intervals
# Calculate expected T and CI for each v in the grid
t_grid_data <- data.frame(
  v = v_grid,
  t = 1 / v_grid  # T = 1/v
)

# Calculate expected T for w = 50 (constant across all viscosities)
t_grid_data$expected_t <- rep(t_expected, nrow(t_grid_data))
t_grid_data$lower_ci <- rep(t_lower, nrow(t_grid_data))
t_grid_data$upper_ci <- rep(t_upper, nrow(t_grid_data))

# Plot confidence intervals
t_ci_plot <- ggplot(t_grid_data, aes(x = v)) +
  geom_line(aes(y = t), color = "black", linetype = "dashed", size = 0.5) +  # T = 1/v relationship
  geom_hline(yintercept = t_expected, color = "blue", size = 1) +  # Expected T
  geom_hline(yintercept = t_lower, color = "red", linetype = "dashed") +  # Lower CI
  geom_hline(yintercept = t_upper, color = "red", linetype = "dashed") +  # Upper CI
  annotate("text", x = max(v_grid), y = t_expected, 
           label = paste("Expected T =", round(t_expected, 4)), 
           hjust = 1, vjust = -0.5) +
  annotate("text", x = max(v_grid), y = t_lower, 
           label = paste("Lower CI =", round(t_lower, 4)), 
           hjust = 1, vjust = 1.5) +
  annotate("text", x = max(v_grid), y = t_upper, 
           label = paste("Upper CI =", round(t_upper, 4)), 
           hjust = 1, vjust = -0.5) +
  labs(title = "94% Confidence Interval for Expected Value of T",
       subtitle = "For w = 50, across viscosity range v ∈ [10, 300]",
       x = "Viscosity (v)",
       y = "Time (T)") +
  theme_minimal() +
  coord_cartesian(ylim = c(min(t_lower, t_grid_data$t) * 0.9, 
                           max(t_upper, t_grid_data$t) * 1.1))

# Save the plot
ggsave("plots_ex3/06_T_confidence_intervals.png", t_ci_plot, width = 8, height = 6)
print(t_ci_plot)

# -------------------------------------------------
# Part (e): Investigate whether the smaller model with θ₁ = 25 is appropriate
# -------------------------------------------------

# Fit the restricted model with θ₁ = 25
restricted_model <- nls(Viscosity ~ (25 * Wt) / (Wt - theta2), 
                        data = stormer,
                        start = list(theta2 = theta2_est),
                        control = nls.control(maxiter = 100, tol = 1e-5, minFactor = 1e-10))

# Summary of the restricted model
summary_restricted <- summary(restricted_model)
print(summary_restricted)

# Compare the full and restricted models using anova
anova_result <- anova(restricted_model, nls_model)
print(anova_result)

# Calculate AIC for both models
AIC_full <- AIC(nls_model)
AIC_restricted <- AIC(restricted_model)

# Calculate RSS (Residual Sum of Squares) for both models
RSS_full <- sum(residuals(nls_model)^2)
RSS_restricted <- sum(residuals(restricted_model)^2)

# Calculate TSS (Total Sum of Squares)
TSS <- sum((stormer$Viscosity - mean(stormer$Viscosity))^2)

# Calculate R² for both models
R2_full <- 1 - RSS_full/TSS
R2_restricted <- 1 - RSS_restricted/TSS

cat("Comparison of Full and Restricted Models:\n")
cat("Full model (estimating both θ₁ and θ₂):\n")
cat("  θ₁ =", round(theta1_est, 4), "\n")
cat("  θ₂ =", round(theta2_est, 4), "\n")
cat("  Residual Standard Error =", round(summary_nls$sigma, 4), "\n")
cat("  AIC =", round(AIC_full, 2), "\n")
cat("  R² =", round(R2_full, 4), "\n\n")

cat("Restricted model (θ₁ = 25):\n")
cat("  θ₂ =", round(coef(restricted_model)[1], 4), "\n")
cat("  Residual Standard Error =", round(summary_restricted$sigma, 4), "\n")
cat("  AIC =", round(AIC_restricted, 2), "\n")
cat("  R² =", round(R2_restricted, 4), "\n\n")

cat("Model Comparison:\n")
cat("  Difference in RSS (Restricted - Full) =", round(RSS_restricted - RSS_full, 4), "\n")
cat("  Difference in AIC (Restricted - Full) =", round(AIC_restricted - AIC_full, 2), "\n")
cat("  Difference in R² (Full - Restricted) =", round(R2_full - R2_restricted, 4), "\n")
cat("  F-statistic from ANOVA =", round(anova_result$F[2], 4), "\n")
cat("  p-value from ANOVA =", format(anova_result$`Pr(>F)`[2], scientific = TRUE), "\n\n")

# Add predictions from both models to the original data
stormer$pred_full <- predict(nls_model)
stormer$pred_restricted <- predict(restricted_model)
stormer$resid_full <- residuals(nls_model)
stormer$resid_restricted <- residuals(restricted_model)

# Compare fitted vs. observed for both models
comparison_plot <- ggplot(stormer) +
  geom_point(aes(x = pred_full, y = Viscosity, color = "Full Model")) +
  geom_point(aes(x = pred_restricted, y = Viscosity, color = "Restricted Model")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Comparison of Full and Restricted Models",
       x = "Predicted Viscosity",
       y = "Observed Viscosity",
       color = "Model") +
  theme_minimal()

# Save the plot
ggsave("plots_ex3/07_model_comparison.png", comparison_plot, width = 8, height = 6)
print(comparison_plot)

# Final conclusion
cat("Conclusion for Part (e):\n")
if (anova_result$`Pr(>F)`[2] < 0.05) {
  cat("The smaller model with θ₁ = 25 is not appropriate because:\n")
  cat("1. The hypothesis test in part (b) rejected H₀: θ₁ = 25.\n")
  cat("2. The ANOVA comparison shows a significant difference between the models (p < 0.05).\n")
  cat("3. The full model has a better AIC, indicating better fit while accounting for complexity.\n")
  cat("4. The full model has a higher R², explaining more variance in the data.\n")
} else {
  cat("The smaller model with θ₁ = 25 may be appropriate because:\n")
  cat("1. The hypothesis test in part (b) did not reject H₀: θ₁ = 25.\n")
  cat("2. The ANOVA comparison does not show a significant difference between the models (p > 0.05).\n")
  cat("3. The difference in AIC and R² is small, suggesting similar performance.\n")
  cat("4. The simpler model is more parsimonious and may provide a more robust explanation.\n")
}