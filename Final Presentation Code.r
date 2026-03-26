# Load required libraries
library(ggplot2)
library(GGally)
library(car)
library(gridExtra)

# Load dataset
setwd("C:\\Users\\debor\\OneDrive - York University\\York Courses\\PSYC 6132")
library(readxl)
df <- read_excel("Final Presentation Dataset.xlsx")
presentation_data <- na.omit(df)

# ============================================================
# MULTIPLE REGRESSION #1
# Outcome: Composite score for Time for Scene A
# Predictors: Sex, AGE, DHI, RPQ_3, RPQ_13
# ============================================================

# Fit the model
model <- lm(`1TimingComp` ~ AGE + Sex_Recode_F1M0 + DHI + RPQ_3 + RPQ_13, data = presentation_data)
summary(model)

# ============================================================
# ASSUMPTION 1: LINEARITY
# Method: Partial regression (added-variable) plots
# Look for: A roughly linear trend in each panel.
#           A curved/nonlinear pattern flags a problem.
# ============================================================
par(mfrow = c(1, 1))
avPlots(model,
        main = "Added-Variable Plots (Linearity Check)",
        col  = "steelblue", col.lines = "red")


# ============================================================
# ASSUMPTION 2: NORMALITY OF RESIDUALS
# Method: Q-Q plot of studentised residuals
# Look for: Points hugging the diagonal line.
#           Systematic S-curves or heavy tails flag non-normality.
# ============================================================
stud_res <- rstudent(model)

ggplot(data.frame(stud_res), aes(sample = stud_res)) +
  stat_qq(color = "steelblue", size = 2) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title    = "Normal Q-Q Plot of Studentised Residuals (Normality Check)",
       x = "Theoretical Quantiles",
       y = "Studentised Residuals") +
  theme_minimal(base_size = 13)


# ============================================================
# ASSUMPTION 3: HOMOSCEDASTICITY (constant variance)
# Method: Residuals vs Fitted plot
# Look for: A horizontal band of roughly equal spread.
#           A funnel shape (narrowing or widening) flags heteroscedasticity.
# ============================================================
fitted_vals <- fitted(model)
std_res     <- rstandard(model)

ggplot(data.frame(fitted_vals, std_res),
       aes(x = fitted_vals, y = std_res)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 0,  color = "red",  linewidth = 1) +
  geom_hline(yintercept =  2, color = "gray50", linetype = "dashed") +
  geom_hline(yintercept = -2, color = "gray50", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linewidth = 0.8) +
  labs(title = "Residuals vs Fitted (Homoscedasticity Check)",
       x = "Fitted Values",
       y = "Standardised Residuals") +
  theme_minimal(base_size = 13)


# ============================================================
# ASSUMPTION 4: INDEPENDENCE OF RESIDUALS
# Method: Residuals vs observation order (index plot)
# Look for: No systematic trend, wave, or drift across the sequence.
#           A pattern suggests autocorrelation or data-collection effects.
# ============================================================
n <- length(std_res)

ggplot(data.frame(index = 1:n, std_res),
       aes(x = index, y = std_res)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_line(color  = "steelblue", alpha = 0.3) +
  geom_hline(yintercept = 0,  color = "red",    linewidth = 1) +
  geom_hline(yintercept =  2, color = "gray50", linetype = "dashed") +
  geom_hline(yintercept = -2, color = "gray50", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linewidth = 0.8) +
  labs(title = "Residuals vs Observation Order (Independence Check)",
       x = "Observation Index",
       y = "Standardised Residuals") +
  theme_minimal(base_size = 13)


# ============================================================
# ASSUMPTION 5: MULTICOLLINEARITY
# Method: Correlation matrix heat map of continuous predictors
#         + VIF bar chart from the fitted model
# Look for: Correlations approaching ±0.80+ between predictors (heat map).
#           VIF > 5 (moderate concern) or > 10 (serious concern) in bar chart.
#           Sex is excluded from the correlation plot as it is binary.
# ============================================================

# 5a. Correlation heat map (continuous predictors only)
cont_preds <- presentation_data[, c("AGE", "DHI", "RPQ_3", "RPQ_13")]
cor_matrix  <- cor(cont_preds, use = "complete.obs")

cor_presentation_data <- as.data.frame(as.table(cor_matrix))
names(cor_presentation_data) <- c("Var1", "Var2", "Correlation")

ggplot(cor_presentation_data, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)),
            size = 5, fontface = "bold") +
  scale_fill_gradient2(low  = "#2166AC", mid = "white", high = "#D6604D",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Predictor Correlation Heat Map (Multicollinearity Check)") +
  theme_minimal(base_size = 13) +
  theme(axis.title = element_blank())


# 5b. VIF bar chart
vif_vals <- vif(model)
vif_presentation_data   <- data.frame(
  Predictor = names(vif_vals),
  VIF       = as.numeric(vif_vals)
)

ggplot(vif_presentation_data, aes(x = reorder(Predictor, VIF), y = VIF, fill = VIF)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 5,  color = "orange", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 10, color = "red",    linetype = "dashed", linewidth = 1) +
  annotate("text", x = 0.6, y = 5.3,  label = "VIF = 5",  color = "orange", hjust = 0) +
  annotate("text", x = 0.6, y = 10.3, label = "VIF = 10", color = "red",    hjust = 0) +
  scale_fill_gradient(low = "steelblue", high = "tomato") +
  coord_flip() +
  labs(title = "VIF Bar Chart (Multicollinearity Check)",
       x = "Predictor", y = "Variance Inflation Factor (VIF)") +
  theme_minimal(base_size = 13)


# ============================================================
# ASSUMPTION 6: FIXED / MEASURED-WITHOUT-ERROR PREDICTORS
# Method: Histogram for each continuous predictor
#         Grouped bar chart for the binary predictor (sex)
# Look for: Reasonable, plausible distributions with no extreme
#           implausible values — which would suggest measurement error.
#           Histograms flag outliers and data-entry anomalies.
# ============================================================

# Continuous predictor histograms
p_AGE <- ggplot(presentation_data, aes(x = AGE)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "AGE", x = "AGE", y = "Count") +
  theme_minimal(base_size = 12)

p_dhi <- ggplot(presentation_data, aes(x = DHI)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "DHI", x = "DHI Score", y = "Count") +
  theme_minimal(base_size = 12)

p_r3 <- ggplot(presentation_data, aes(x = RPQ_3)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "RPQ_3", x = "RPQ_3 Score", y = "Count") +
  theme_minimal(base_size = 12)

p_r13 <- ggplot(presentation_data, aes(x = RPQ_13)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "RPQ_13", x = "RPQ_13 Score", y = "Count") +
  theme_minimal(base_size = 12)

# Binary predictor bar chart (sex)
p_sex <- ggplot(presentation_data, aes(x = factor(Sex_Recode_F1M0, labels = c("Male (0)", "Female (1)")))) +
  geom_bar(fill = c("steelblue", "tomato"), color = "white") +
  labs(title = "Sex", x = "Sex", y = "Count") +
  theme_minimal(base_size = 12)

grid.arrange(p_AGE, p_dhi, p_r3, p_r13, p_sex,
             ncol   = 3,
             top    = "Predictor Distributions (Fixed Predictors Check)")

# ============================================================
# MULTIPLE REGRESSION #2
# Outcome: Composite score for Time for Scene B
# Predictors: Sex, AGE, DHI, RPQ_3, RPQ_13
# ============================================================

# Fit the model
model2 <- lm(`8TimingComp` ~ AGE + Sex_Recode_F1M0 + DHI + RPQ_3 + RPQ_13, data = presentation_data)
summary(model2)

# ============================================================
# ASSUMPTION 1: LINEARITY
# Method: Partial regression (added-variable) plots
# Look for: A roughly linear trend in each panel.
#           A curved/nonlinear pattern flags a problem.
# ============================================================
par(mfrow = c(1, 1))
avPlots(model_2,
        main = "Added-Variable Plots (Linearity Check)",
        col  = "steelblue", col.lines = "red")


# ============================================================
# ASSUMPTION 2: NORMALITY OF RESIDUALS
# Method: Q-Q plot of studentised residuals
# Look for: Points hugging the diagonal line.
#           Systematic S-curves or heavy tails flag non-normality.
# ============================================================
stud_res_2 <- rstudent(model_2)

ggplot(data.frame(stud_res_2), aes(sample = stud_res_2)) +
  stat_qq(color = "steelblue", size = 2) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title    = "Normal Q-Q Plot of Studentised Residuals (Normality Check)",
       x = "Theoretical Quantiles",
       y = "Studentised Residuals") +
  theme_minimal(base_size = 13)


# ============================================================
# ASSUMPTION 3: HOMOSCEDASTICITY (constant variance)
# Method: Residuals vs Fitted plot
# Look for: A horizontal band of roughly equal spread.
#           A funnel shape (narrowing or widening) flags heteroscedasticity.
# ============================================================
fitted_vals_2 <- fitted(model_2)
std_res_2     <- rstandard(model_2)

ggplot(data.frame(fitted_vals_2, std_res_2),
       aes(x = fitted_vals_2, y = std_res_2)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 0,  color = "red",  linewidth = 1) +
  geom_hline(yintercept =  2, color = "gray50", linetype = "dashed") +
  geom_hline(yintercept = -2, color = "gray50", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linewidth = 0.8) +
  labs(title = "Residuals vs Fitted (Homoscedasticity Check)",
       x = "Fitted Values",
       y = "Standardised Residuals") +
  theme_minimal(base_size = 13)


# ============================================================
# ASSUMPTION 4: INDEPENDENCE OF RESIDUALS
# Method: Residuals vs observation order (index plot)
# Look for: No systematic trend, wave, or drift across the sequence.
#           A pattern suggests autocorrelation or data-collection effects.
# ============================================================
n_2 <- length(std_res_2)

ggplot(data.frame(index = 1:n, std_res),
       aes(x = index, y = std_res)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_line(color  = "steelblue", alpha = 0.3) +
  geom_hline(yintercept = 0,  color = "red",    linewidth = 1) +
  geom_hline(yintercept =  2, color = "gray50", linetype = "dashed") +
  geom_hline(yintercept = -2, color = "gray50", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linewidth = 0.8) +
  labs(title = "Residuals vs Observation Order (Independence Check)",
       x = "Observation Index",
       y = "Standardised Residuals") +
  theme_minimal(base_size = 13)


# ============================================================
# ASSUMPTION 5: MULTICOLLINEARITY
# Method: Correlation matrix heat map of continuous predictors
#         + VIF bar chart from the fitted model
# Look for: Correlations approaching ±0.80+ between predictors (heat map).
#           VIF > 5 (moderate concern) or > 10 (serious concern) in bar chart.
#           Sex is excluded from the correlation plot as it is binary.
# ============================================================

# 5a. Correlation heat map (continuous predictors only)
cont_preds_2 <- presentation_data[, c("AGE", "DHI", "RPQ_3", "RPQ_13")]
cor_matrix_2  <- cor(cont_preds, use = "complete.obs")

cor_presentation_data_2 <- as.data.frame(as.table(cor_matrix))
names(cor_presentation_data_2) <- c("Var1", "Var2", "Correlation")

ggplot(cor_presentation_data_2, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)),
            size = 5, fontface = "bold") +
  scale_fill_gradient2(low  = "#2166AC", mid = "white", high = "#D6604D",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Predictor Correlation Heat Map (Multicollinearity Check)") +
  theme_minimal(base_size = 13) +
  theme(axis.title = element_blank())


# 5b. VIF bar chart
vif_vals_2 <- vif(model_2)
vif_presentation_data_2   <- data.frame(
  Predictor = names(vif_vals_2),
  VIF       = as.numeric(vif_vals_2)
)

ggplot(vif_presentation_data, aes(x = reorder(Predictor, VIF), y = VIF, fill = VIF)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 5,  color = "orange", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 10, color = "red",    linetype = "dashed", linewidth = 1) +
  annotate("text", x = 0.6, y = 5.3,  label = "VIF = 5",  color = "orange", hjust = 0) +
  annotate("text", x = 0.6, y = 10.3, label = "VIF = 10", color = "red",    hjust = 0) +
  scale_fill_gradient(low = "steelblue", high = "tomato") +
  coord_flip() +
  labs(title = "VIF Bar Chart (Multicollinearity Check)",
       x = "Predictor", y = "Variance Inflation Factor (VIF)") +
  theme_minimal(base_size = 13)


# ============================================================
# ASSUMPTION 6: FIXED / MEASURED-WITHOUT-ERROR PREDICTORS
# Method: Histogram for each continuous predictor
#         Grouped bar chart for the binary predictor (sex)
# Look for: Reasonable, plausible distributions with no extreme
#           implausible values — which would suggest measurement error.
#           Histograms flag outliers and data-entry anomalies.
# ============================================================

# Continuous predictor histograms
p_AGE <- ggplot(presentation_data, aes(x = AGE)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "AGE", x = "AGE", y = "Count") +
  theme_minimal(base_size = 12)

p_dhi <- ggplot(presentation_data, aes(x = DHI)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "DHI", x = "DHI Score", y = "Count") +
  theme_minimal(base_size = 12)

p_r3 <- ggplot(presentation_data, aes(x = RPQ_3)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "RPQ_3", x = "RPQ_3 Score", y = "Count") +
  theme_minimal(base_size = 12)

p_r13 <- ggplot(presentation_data, aes(x = RPQ_13)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "RPQ_13", x = "RPQ_13 Score", y = "Count") +
  theme_minimal(base_size = 12)

# Binary predictor bar chart (sex)
p_sex <- ggplot(presentation_data, aes(x = factor(Sex_Recode_F1M0, labels = c("Male (0)", "Female (1)")))) +
  geom_bar(fill = c("steelblue", "tomato"), color = "white") +
  labs(title = "Sex", x = "Sex", y = "Count") +
  theme_minimal(base_size = 12)

grid.arrange(p_AGE, p_dhi, p_r3, p_r13, p_sex,
             ncol   = 3,
             top    = "Predictor Distributions (Fixed Predictors Check)")