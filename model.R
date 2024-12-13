library(ggplot2)
library(readr)
library(splines)
library(mgcv)

df <- data3 %>%
  group_by(pptid) %>%
  filter(event == max(event)) %>%
  ungroup()



# parametric logistic regression model
model <- glm(DISPO ~ Age, data = burn_data, family = binomial)
age_seq <- seq(min(burn_data$Age, na.rm = TRUE), max(burn_data$Age, na.rm = TRUE), length.out = 100)
pred_data <- data.frame(Age = age_seq)
pred_data$predicted_prob <- predict(model, newdata = pred_data, type = "response")

# optimal number of knots based on AIC
aic_b_spline <- function(m, data) {
  model <- glm(DISPO ~ bs(Age, df = m), data = data, family = binomial)
  
  return(AIC(model))
}
m_values <- 3:10  # Range of df values to test
aic_values <- sapply(m_values, aic_b_spline, data = burn_data)
optimal_m <- m_values[which.min(aic_values)]

# B-spline model
# knots on the quantiles of age
optimal_knots <- quantile(burn_data$Age, probs = seq(0, 1, length.out = optimal_m))
bs_model <- glm(DISPO ~ bs(Age, knots = optimal_knots), 
                data = burn_data, family = binomial)

pred_data$predicted_bs <- predict(bs_model, newdata = pred_data, type = "response")

# long data
long_pred_data <- rbind(
  data.frame(Age = pred_data$Age, Probability = pred_data$predicted_prob, Model = "Parametric Model"),
  data.frame(Age = pred_data$Age, Probability = pred_data$predicted_bs, Model = "B-Spline Model")
)

ggplot(burn_data, aes(x = Age, y = DISPO)) +
  geom_jitter(width = 0.5, height = 0.05, alpha = 0.5) +  
  geom_line(data = long_pred_data, aes(x = Age, y = Probability, color = Model), size = 1) +
  labs(title = "Comparison of Parametric and B-Spline Models",
       x = "Age",
       y = "Probability of Survival (DISPO)",
       color = "Model") + 
  theme_minimal()





# Logistic regression model (parametric)
model <- glm(vital ~ desat_slope, data = df, family = binomial)

# Generate a sequence of desat_slope values for prediction
desat_slope_seq <- seq(min(df$desat_slope, na.rm = TRUE), max(df$desat_slope, na.rm = TRUE), length.out = 1000)

# Create a new data frame for predictions
pred_data <- data.frame(desat_slope = desat_slope_seq)
pred_data$predicted_prob <- predict(model, newdata = pred_data, type = "response")

# Optimal number of knots based on AIC for B-spline model
aic_b_spline <- function(m, data) {
  model <- glm(vital ~ bs(desat_slope, df = 3,knots =m), data = data, family = binomial)
  return(AIC(model))
}

m_values <- 3:10  # Range of df values to test
aic_values <- sapply(m_values, aic_b_spline, data = df)
optimal_m <- m_values[which.min(aic_values)]

# B-spline model with optimal knots
optimal_knots <- quantile(df$desat_slope, probs = seq(0, 1, length.out = optimal_m))
bs_model <- glm(vital ~ bs(desat_slope, knots = optimal_knots, df=3), 
                data = df, family = binomial)

# Predictions from B-spline model
pred_data$predicted_bs <- predict(bs_model, newdata = pred_data, type = "response")

# Combine predictions for plotting
long_pred_data <- rbind(
  data.frame(desat_slope = pred_data$desat_slope, Probability = pred_data$predicted_prob, Model = "Parametric Model"),
  data.frame(desat_slope = pred_data$desat_slope, Probability = pred_data$predicted_bs, Model = "B-Spline Model")
)

# Plotting the results
ggplot(df, aes(x = desat_slope, y = vital)) +
  geom_jitter(width = 0.5, height = 0.05, alpha = 0.5) +  
  geom_line(data = long_pred_data, aes(x = desat_slope, y = Probability, color = Model), size = 1) +
  labs(title = "Comparison of Parametric and B-Spline Models",
       x = "Desaturation Slope (desat_slope)",
       y = "Vital Status (Outcome)",
       color = "Model") + 
  theme_minimal()













































# Logistic regression model (parametric)
model <- glm(vital ~ desat_slope, data = df, family = binomial)

# Generate a sequence of desat_slope values for prediction
desat_slope_seq <- seq(min(df$desat_slope, na.rm = TRUE), max(df$desat_slope, na.rm = TRUE), length.out = 1000)

# Create a new data frame for predictions
pred_data <- data.frame(desat_slope = desat_slope_seq)

# Predict probabilities and standard errors
pred_data$predicted_prob <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)$fit
pred_data$se_logit <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)$se.fit

# Confidence interval for logistic regression
pred_data$lower_ci <- pred_data$predicted_prob - 1.96 * pred_data$se_logit
pred_data$upper_ci <- pred_data$predicted_prob + 1.96 * pred_data$se_logit

# B-spline model
optimal_m <- 3  
optimal_knots <- quantile(df$desat_slope, probs = seq(0, 1, length.out = optimal_m))
bs_model <- glm(vital ~ bs(desat_slope, knots = optimal_knots, df=3), 
                data = df, family = binomial)

# Predict probabilities and standard errors for B-spline model
pred_data$predicted_bs <- predict(bs_model, newdata = pred_data, type = "response", se.fit = TRUE)$fit
pred_data$se_bs <- predict(bs_model, newdata = pred_data, type = "response", se.fit = TRUE)$se.fit

# Confidence interval for B-spline model
pred_data$lower_bs_ci <- pred_data$predicted_bs - 1.96 * pred_data$se_bs
pred_data$upper_bs_ci <- pred_data$predicted_bs + 1.96 * pred_data$se_bs

# Combine predictions for plotting
long_pred_data <- rbind(
  data.frame(desat_slope = pred_data$desat_slope, Probability = pred_data$predicted_prob, Model = "Parametric Model", 
             Lower_CI = pred_data$lower_ci, Upper_CI = pred_data$upper_ci),
  data.frame(desat_slope = pred_data$desat_slope, Probability = pred_data$predicted_bs, Model = "B-Spline Model",
             Lower_CI = pred_data$lower_bs_ci, Upper_CI = pred_data$upper_bs_ci)
)

# Plotting the results with confidence bands
ggplot() +
  geom_jitter(data = df, aes(x = desat_slope, y = vital), width = 0.5, height = 0.05, alpha = 0.5) +  
  geom_line(data = long_pred_data, aes(x = desat_slope, y = Probability, color = Model), size = 1) +
  geom_ribbon(data = long_pred_data, aes(x = desat_slope, ymin = Lower_CI, ymax = Upper_CI, fill = Model), alpha = 0.2) +
  labs(title = "Comparison of Parametric and B-Spline Models with Confidence Bands",
       x = "Last Recorded Desaturation Slope",
       y = "Estimated Probability (Vital Status)",
       color = "Model", fill = "Model") + 
  theme_minimal()





















































library(nlme)
library(mgcv)
set.seed(1)


  
  # smoothing for beta(z)
  smooth_basis <- predict(gam(y ~ s(z), data = data, method = "REML"), type = "terms")[, "s(z)"]
  
  # lme
  fit <- lme(
    fixed = y ~ x + smooth_basis,  # Fixed effects: x and the smooth term
    random = ~ 1 | z,              # Random effects for smooth functions of z
    data = data,
    method = "REML"
  )
  
  # Coefficients
  fitted_smooth <- predict(fit, level = 0)
  
  fitted_beta0 <- fitted_smooth[data$x == 0]
  fitted_beta1 <- fitted_smooth[data$x == 1] - fitted_beta0
  
  
  
  # RMSE
  results$beta0_rmse[sim] <- sqrt(mean((beta0[data$x == 0] - fitted_beta0)^2))
  results$beta1_rmse[sim] <- sqrt(mean((beta1[data$x == 1] - fitted_beta1)^2))
  
  
  fitted_y <- fitted_smooth + fitted_beta1 * data$x  # Rebuild fitted y from fixed and random effects
  results$y_rmse[sim] <- sqrt(mean((y - fitted_y)^2))  # RMSE between observed y and fitted y

  
  
  

summary(results)

fitted_smooth <- predict(fit, level = 0)

data$fitted_y <- fitted_smooth

ggplot(data, aes(x = z)) +
  geom_point(aes(y = y), color = "blue", alpha = 0.5, size = 1.5) +
  geom_line(aes(y = fitted_y), color = "red", linewidth = 1) +
  labs(
    title = "Comparison of True and Fitted y",
    x = "z",
    y = "y",
    subtitle = "Blue: True y | Red: Fitted Curve"
  ) +
  theme_minimal()


















library(lme4)



# Fit a mixed-effects logistic regression model
mixed_model <- glmer(vital ~ desat_slope + (1 | event), 
                     data = data3, 
                     family = binomial)

# Summary of the model
summary(mixed_model)

# Extract fixed effects (desaturation slope)
fixed_effects <- fixef(mixed_model)

# Random effects for events
random_effects <- ranef(mixed_model)

# Evaluate model fit using AIC/BIC
aic <- AIC(mixed_model)
bic <- BIC(mixed_model)

# Predict probabilities for vital status
data3$predicted_prob <- predict(mixed_model, type = "response")

# Plotting predicted probabilities
library(ggplot2)

ggplot(data3, aes(x = desaturation_slope, y = predicted_prob)) +
  geom_point(aes(color = factor(vital_status)), alpha = 0.6) +
  geom_smooth(method = "loess", color = "blue", se = TRUE) +
  labs(
    title = "Predicted Probabilities of Vital Status",
    x = "Desaturation Slope",
    y = "Predicted Probability of Vital Status",
    color = "Vital Status"
  ) +
  theme_minimal()






























# Load necessary libraries
library(KernSmooth)
library(ggplot2)

# Kernel smoothing function with Gaussian kernel
kernel_smooth <- function(x, y, z, bandwidth) {
  weights <- dnorm((z - x) / bandwidth)  # Gaussian kernel weights
  beta <- sum(weights * y) / sum(weights)
  return(beta)
}

# Select bandwidth using DPIK (Gaussian kernel)
bandwidth <- dpik(data3$desat_slope, kernel = "normal")  # "normal" specifies Gaussian kernel

# Estimate varying coefficients beta0(z) and beta1(z)
data3$fitted_beta0 <- sapply(
  data3$desat_slope,
  kernel_smooth,
  y = ifelse(data3$vital_status == 1, 1, 0),  # Response for vital_status = 1 (alive)
  z = data3$desat_slope,
  bandwidth = bandwidth
)

data3$fitted_beta1 <- sapply(
  data3$desat_slope,
  kernel_smooth,
  y = ifelse(data3$vital_status == 0, 1, 0),  # Response for vital_status = 0 (dead)
  z = data3$desat_slope,
  bandwidth = bandwidth
)

# Bootstrapping for confidence intervals
bootstrap_ci <- function(data, num_bootstrap = 1000) {
  n <- nrow(data)
  boot_results <- replicate(num_bootstrap, {
    boot_sample <- data[sample(1:n, replace = TRUE), ]
    beta0 <- sapply(
      boot_sample$desaturation_slope,
      kernel_smooth,
      y = ifelse(boot_sample$vital_status == 0, 1, 0),
      z = boot_sample$desaturation_slope,
      bandwidth = bandwidth
    )
    beta1 <- sapply(
      boot_sample$desaturation_slope,
      kernel_smooth,
      y = ifelse(boot_sample$vital_status == 1, 1, 0),
      z = boot_sample$desaturation_slope,
      bandwidth = bandwidth
    )
    list(beta0 = beta0, beta1 = beta1)
  })
  
  ci_beta0 <- apply(sapply(boot_results, `[[`, "beta0"), 1, quantile, probs = c(0.025, 0.975))
  ci_beta1 <- apply(sapply(boot_results, `[[`, "beta1"), 1, quantile, probs = c(0.025, 0.975))
  return(list(ci_beta0 = ci_beta0, ci_beta1 = ci_beta1))
}

# Perform bootstrap and obtain confidence intervals
ci_results <- bootstrap_ci(data3)

data3$ci_beta0_lower <- ci_results$ci_beta0[1, ]
data3$ci_beta0_upper <- ci_results$ci_beta0[2, ]
data3$ci_beta1_lower <- ci_results$ci_beta1[1, ]
data3$ci_beta1_upper <- ci_results$ci_beta1[2, ]

# Visualization
ggplot(data3, aes(x = desaturation_slope)) +
  geom_line(aes(y = fitted_beta0), color = "red", linewidth = 1, label = "Beta0") +
  geom_line(aes(y = fitted_beta1), color = "blue", linewidth = 1, label = "Beta1") +
  geom_ribbon(aes(ymin = ci_beta0_lower, ymax = ci_beta0_upper), fill = "red", alpha = 0.2) +
  geom_ribbon(aes(ymin = ci_beta1_lower, ymax = ci_beta1_upper), fill = "blue", alpha = 0.2) +
  labs(
    title = "Varying Coefficient Model with Gaussian Kernel",
    x = "Desaturation Slope",
    y = "Coefficients",
    subtitle = "Confidence Bands for Beta0 and Beta1"
  ) +
  theme_minimal()































library(KernSmooth)
library(ggplot2)
library(dplyr)

# Kernel Smoothing Function for Varying Coefficient Estimation
kernel_smooth_varying_coefficient <- function(data, bandwidth = NULL) {
  # If bandwidth not provided, use DPIK
  if (is.null(bandwidth)) {
    bandwidth <- dpik(data$event, kernel = "normal")
  }
  
  # Sort data by events
  data <- data[order(data$event), ]
  
  # Estimate varying coefficient
  estimate_coefficient <- function(z) {
    # Compute kernel weights
    kernel_weights <- dnorm((data$event - z) / bandwidth)
    
    # Weighted logistic regression-like estimation
    # Numerator: weighted sum of (vital_status * desat_slope)
    numerator <- sum(kernel_weights * data$vital * data$desat_slope)
    
    # Denominator: weighted sum of squared desat_slope
    denominator <- sum(kernel_weights * data$desat_slope^2)
    
    # Avoid division by zero
    if (denominator == 0) return(0)
    
    return(numerator / denominator)
  }
  
  # Compute standard error
  compute_standard_error <- function(z, beta_estimate) {
    kernel_weights <- dnorm((data$event - z) / bandwidth)  # Corrected from data$events
    
    # Compute residuals
    residuals <- data$vital - (beta_estimate * data$desat_slope)
    
    # Weighted variance
    weighted_var <- sum(kernel_weights * residuals^2) / sum(kernel_weights)
    
    # Standard error
    return(sqrt(weighted_var / sum(kernel_weights)))
  }
  
  # Compute estimates across unique event counts
  unique_events <- sort(unique(data$event))
  
  results <- data.frame(
    event = unique_events,
    coefficient = sapply(unique_events, estimate_coefficient)
  )
  
  # Add confidence intervals
  results$std_error <- sapply(unique_events, function(z) {
    compute_standard_error(z, results$coefficient[results$event == z])  # Changed from results$events
  })
  
  results$ci_lower <- results$coefficient - 1.96 * results$std_error
  results$ci_upper <- results$coefficient + 1.96 * results$std_error
  
  return(results)
}

# Main Analysis Function
perform_varying_coefficient_analysis <- function(data) {
  # Perform kernel smoothing
  varying_coef_results <- kernel_smooth_varying_coefficient(data)
  
  return(varying_coef_results)
}

# Perform analysis
result <- perform_varying_coefficient_analysis(data3)

# Create a plot that shows the original data points and the varying coefficient
vital_event_plot <- ggplot() +
  # Scatter plot of original points with jitter
 
  
  # Line for coefficient estimates
  geom_line(data = result, 
            aes(x = event, y = coefficient), 
            color = "red", 
            linewidth = 1) +
  
  # Confidence interval ribbon
  geom_ribbon(data = result, 
              aes(x = event, ymin = ci_lower, ymax = ci_upper), 
              fill = "red", 
              alpha = 0.2) +
  

  
  # Formatting
  labs(
    title = "Vital Status vs Prior Desaturation Events",
    x = "Number of Prior Desaturation Events", 
    y = "Varying Coefficient Estimate of Desaturation Slope",
    subtitle = "With 95% Confidence Interval"
  ) +
  
  theme_minimal()

# Display the plot
print(vital_event_plot)

# Print the results
print(result)
