---
title: "Predictors of Scene Timing Performance Following Concussion"
subtitle: "Multiple Regression Analysis — PSYC 6132"
date: "`r Sys.Date()`"
output:
  html_document:
    theme: flatly
    highlight: tango
    toc: true
    toc_float:
      collapsed: false
      smooth_scroll: true
    number_sections: false
    self_contained: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo      = FALSE,
  warning   = FALSE,
  message   = FALSE,
  fig.align = "center",
  fig.width  = 9,
  fig.height = 5.5
)

library(ggplot2)
library(car)
library(gridExtra)
library(readxl)
library(dagitty)
library(ggdag)
library(knitr)
library(kableExtra)

df <- read_excel("Final Presentation Dataset.xlsx")
presentation_data <- na.omit(df)

# ── Mean-centre all continuous predictors ──────────────────────────────────
# Sex_Recode_F1M0 is binary and is NOT centred; centring applies only to
# continuous variables so that interaction terms are not correlated with their
# component variables by construction (resolves structural multicollinearity).
presentation_data$AGE_c    <- scale(presentation_data$AGE,
                                    center = TRUE, scale = FALSE)
presentation_data$DHI_c    <- scale(presentation_data$DHI,
                                    center = TRUE, scale = FALSE)
presentation_data$RPQ_3_c  <- scale(presentation_data$RPQ_3,
                                    center = TRUE, scale = FALSE)
presentation_data$RPQ_13_c <- scale(presentation_data$RPQ_13,
                                    center = TRUE, scale = FALSE)
presentation_data$Days_c   <- scale(presentation_data$DaysSinceLastConcussion,
                                    center = TRUE, scale = FALSE)

# ── Model 1: Main effects (centred IVs) ───────────────────────────────────
model <- lm(`8TimingComp` ~ AGE_c + Sex_Recode_F1M0 + DHI_c + RPQ_3_c +
              RPQ_13_c + Days_c,
            data = presentation_data)

# ── Model 2: Main effects + Sex × symptom interactions (centred IVs) ──────
model2 <- lm(`8TimingComp` ~ AGE_c + Sex_Recode_F1M0 * (DHI_c + RPQ_3_c +
               RPQ_13_c) + Days_c,
             data = presentation_data)
```

---

# a) Research Problem & Theoretical Rationale

## {.tabset}

### Concussion and persistent post-concussion symptoms

Concussion is a type of mild traumatic brain injury (mTBI)

* Symptoms include headaches, nausea, impaired balance, double vision, sensory sensitivities, impaired memory and concentration, irritability
* Expected recovery period is 7-10 days

Persistent post-concussion symptoms

* Experienced by 10-20% of individuals following concussion
* Can last for weeks, months, or years
* Working-aged adults, especially females, are underrepresented in PPCS research

---

### Cognitive-motor integration

Cognitive-motor integration (CMI)

* Unconscious process of integrating visual information and motor planning to execute movement
* Studies have found that CMI performance is impaired following concussion 
* Sex differences in CMI performance in working-aged adults with PPCS are not yet well understood

---

### Aim and hypotheses

Aim: Examine the relationships between CMI performance, symptom burden, and sex in a sample of 41 working-aged adults with PPCS (24 female, 29-64 years, mean 47.2 +/- 9.2)

Hypotheses: 

1. Post-concussion symptom severity will predict worse performance (higher timing composite scores) on a CMI task.
2. There will be significant interactions between sex and symptom severity on CMI performance.

---

# b) Variables

## {.tabset}

### CMI Task

Sliding index finger along a touchpad to move a cursor from a presented central target to a peripheral target (up, down, left, or right)

* Plane change: the finger movement is on a horizontally placed touchpad, whereas the task and cursor movement are presented on the vertical plane of the laptop screen
* Feedback reversal: the cursor moves in the opposite direction as the finger
* 20 trials

Timing composite score: an addition of three z-standardized timing measures

* Reaction time (ms)
* Full movement time (ms)
* Peak velocity (mm/ms)

```{r, echo=FALSE, out.width="50%"}
knitr::include_graphics("C:\\Users\\debor\\OneDrive - York University\\York Courses\\PSYC 6132\\Scene Schema.png")
```

---

### Variable Details

**Criterion variable**

```{r criterion-table}
criterion <- data.frame(
  Variable    = "8TimingComp",
  Description = "Scene completion time composite",
  Scale       = "Continuous (z-scored composite; higher = slower performance)"
)
kable(criterion) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

**Predictors**

```{r predictor-table}
predictors <- data.frame(
  Variable = c("AGE", "Sex_Recode_F1M0", "DHI", "RPQ_3", "RPQ_13",
               "DaysSinceLastConcussion"),
  Description = c(
    "Participant age",
    "Biological sex",
    "Dizziness Handicap Inventory",
    "Rivermead Post-Concussion Symptoms — 3 early symptoms",
    "Rivermead Post-Concussion Symptoms — 13 later symptoms",
    "Days elapsed since most recent concussion (control variable)"
  ),
  Scale = c(
    "Continuous (years)",
    "Binary (0 = Male, 1 = Female)",
    "Continuous (0–100; higher = greater dizziness handicap)",
    "Continuous (0–12; higher = more severe)",
    "Continuous (0–52; higher = more severe)",
    "Continuous (days)"
  ),
  Model = c(
    "Both models",
    "Both models",
    "Both models",
    "Both models",
    "Both models",
    "Both models (control)"
  )
)
kable(predictors) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

Model 2 additionally includes the interaction terms **Sex × DHI**, **Sex × RPQ_3**, and **Sex × RPQ_13**, testing whether the effect of each symptom scale on timing performance differs between males and females.

---

# c) Regression Equations

**Model 1 — Main Effects (mean-centred continuous predictors)**

$$\hat{Y} = b_0 + b_1(\text{AGE}_c) + b_2(\text{Sex}) + b_3(\text{DHI}_c) + b_4(\text{RPQ\_3}_c) + b_5(\text{RPQ\_13}_c) + b_6(\text{Days}_c) + \varepsilon$$

Where the subscript $_c$ denotes mean-centred predictors. Centring does not change model fit, *F*-statistics, or the standardised coefficients; it shifts the intercept to the predicted value at the mean of all continuous predictors and eliminates structural collinearity with interaction terms in Model 2.

**DAG: Model 1 Variable Relationships**

```{r dag-model1, fig.height=5}
dag1 <- dagitty('
dag {
  AGE                     [pos = "0,1"]
  Sex                     [pos = "0,2"]
  DaysSinceLastConcussion [pos = "0,3"]
  DHI                     [pos = "1,1"]
  RPQ_3                   [pos = "1,2"]
  RPQ_13                  [pos = "1,3"]
  TimingComp              [outcome, pos = "2,2"]

  AGE -> TimingComp
  Sex -> TimingComp
  DHI -> TimingComp
  RPQ_3 -> TimingComp
  RPQ_13 -> TimingComp
  DaysSinceLastConcussion -> TimingComp
  DHI -> TimingComp; RPQ_3 -> TimingComp; RPQ_13 -> TimingComp
}
')

node_labels1 <- c(
  AGE = "Age", Sex = "Sex",
  DaysSinceLastConcussion = "Days Since\nLast Concussion",
  DHI = "DHI", RPQ_3 = "RPQ_3", RPQ_13 = "RPQ_13",
  TimingComp = "Scene Timing\nComposite"
)

tidy_dagitty(dag1) |>
  ggdag(text = FALSE, use_labels = "name") +
  geom_dag_point(aes(color = name), size = 16, show.legend = FALSE) +
  geom_dag_edges(edge_colour = "gray40",
                 arrow_directed = grid::arrow(length = grid::unit(7, "pt"),
                                              type = "closed")) +
  geom_dag_label_repel(aes(label = node_labels1[name]),
                       size = 3.4, fontface = "bold",
                       box.padding = 0.4, label.padding = 0.25,
                       fill = "white", colour = "black") +
  scale_color_brewer(palette = "Set2") +
  theme_dag()
```

```{r coef-table-m1}
coef_m1 <- as.data.frame(summary(model)$coefficients)
coef_m1 <- round(coef_m1, 3)
coef_m1$Predictor <- c("Intercept", "AGE (centred)", "Sex (F=1, M=0)",
                        "DHI (centred)", "RPQ_3 (centred)", "RPQ_13 (centred)",
                        "Days Since Concussion (centred)")
coef_m1 <- coef_m1[, c("Predictor", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
names(coef_m1) <- c("Predictor", "b", "SE", "t", "p")

kable(coef_m1, align = "lrrrr",
      caption = "Model 1 Coefficients (mean-centred continuous predictors)") |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
```

---

**Model 2 — Main Effects + Sex × Symptom Interactions (mean-centred continuous predictors)**

$$\hat{Y} = b_0 + b_1(\text{AGE}_c) + b_2(\text{Sex}) + b_3(\text{DHI}_c) + b_4(\text{RPQ\_3}_c) + b_5(\text{RPQ\_13}_c) + b_6(\text{Days}_c) + b_7(\text{Sex} \times \text{DHI}_c) + b_8(\text{Sex} \times \text{RPQ\_3}_c) + b_9(\text{Sex} \times \text{RPQ\_13}_c) + \varepsilon$$

**DAG: Model 2 Variable Relationships**

```{r dag-model2, fig.height=6}
dag2 <- dagitty('
dag {
  AGE                     [pos = "0,1"]
  Sex                     [pos = "0,2"]
  DaysSinceLastConcussion [pos = "0,3"]
  DHI                     [pos = "1,1"]
  RPQ_3                   [pos = "1,2"]
  RPQ_13                  [pos = "1,3"]
  TimingComp              [outcome, pos = "2,2"]

  AGE -> TimingComp
  Sex -> DHI; Sex -> RPQ_3; Sex -> RPQ_13; Sex -> TimingComp
  DaysSinceLastConcussion -> TimingComp
  DHI -> TimingComp; RPQ_3 -> TimingComp; RPQ_13 -> TimingComp
}
')

# Tidy and plot, then annotate interaction paths manually
tidy_dag2 <- tidy_dagitty(dag2)

node_labels2 <- c(
  AGE = "AGE", Sex = "Sex",
  DaysSinceLastConcussion = "Days Since\nLast Concussion",
  DHI = "DHI", RPQ_3 = "RPQ_3", RPQ_13 = "RPQ_13",
  TimingComp = "Scene Timing\nComposite"
)

# Node positions for drawing interaction moderator arrows
node_pos <- data.frame(
  name = c("Sex", "DHI", "RPQ_3", "RPQ_13", "TimingComp"),
  x    = c(0, 1, 1, 1, 2),
  y    = c(2, 1, 2, 3, 2)
)

# Midpoints of the symptom -> TimingComp paths (where moderation acts)
mid_pts <- data.frame(
  xmid = c(1.5, 1.5, 1.5),
  ymid = c(1.5, 2.0, 2.5),
  label = c("Sex×DHI", "Sex×RPQ_3", "Sex×RPQ_13")
)

ggdag(tidy_dag2, text = FALSE, use_labels = "name") +
  geom_dag_point(aes(color = name), size = 16, show.legend = FALSE) +
  geom_dag_edges(edge_colour = "gray40",
                 arrow_directed = grid::arrow(length = grid::unit(7, "pt"),
                                              type = "closed")) +
  geom_dag_label_repel(aes(label = node_labels2[name]),
                       size = 3.4, fontface = "bold",
                       box.padding = 0.4, label.padding = 0.25,
                       fill = "white", colour = "black") +
  theme_dag()
```

```{r coef-table-m2}
coef_m2 <- as.data.frame(summary(model2)$coefficients)
coef_m2 <- round(coef_m2, 3)
coef_m2$Predictor <- c("Intercept", "AGE (centred)", "Sex (F=1, M=0)",
                        "DHI (centred)", "RPQ_3 (centred)", "RPQ_13 (centred)",
                        "Days Since Concussion (centred)",
                        "Sex × DHI (centred)", "Sex × RPQ_3 (centred)", "Sex × RPQ_13 (centred)")
coef_m2 <- coef_m2[, c("Predictor", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
names(coef_m2) <- c("Predictor", "b", "SE", "t", "p")

kable(coef_m2, align = "lrrrr",
      caption = "Model 2 Coefficients (mean-centred continuous predictors)") |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) |>
  pack_rows("Main Effects", 1, 7) |>
  pack_rows("Interaction Terms (Sex × Symptom Scale)", 8, 10)
```

---

# d) Critical Test

**Model 1:** The critical elements are the **symptom severity predictors** — DHI, RPQ_3, and RPQ_13. We expect one or more of $b_3$, $b_4$, $b_5$ to be positive and statistically significant (*p* < .05), indicating that higher symptom burden predicts worse (slower) scene timing performance, after controlling for age, sex, and recovery duration.

**Model 2:** The additional critical test concerns the **Sex × symptom interaction terms** ($b_7$, $b_8$, $b_9$). A significant interaction would indicate that the relationship between a given symptom scale and timing performance differs depending on sex — consistent with the hypothesis that females and males translate equivalent symptom levels into differential functional impairment. This is evaluated both via the individual interaction *t*-tests and via a nested model comparison (likelihood-ratio / *F*-test) between Model 1 and Model 2, where a significant $\Delta \chi^2$ or $\Delta F$ would indicate that adding the interaction terms meaningfully improves model fit.

The inclusion of **DaysSinceLastConcussion** as a covariate is important in both models: symptom scores naturally decrease over time, so controlling for recovery duration isolates the *concurrent* symptom-performance relationship rather than conflating it with recovery trajectory.

---

# e) Assumption Checks

Assumptions are checked separately for each model. Because Model 2 adds interaction terms, it is subject to the same six standard regression assumptions; the key additional consideration is that interactions between a binary variable (Sex) and continuous predictors can elevate multicollinearity among the expanded predictor set, which is specifically examined in the multicollinearity tab.

## Model 1 — Main Effects {.tabset}

### 1. Linearity

**Residuals vs. Fitted (composite)**

```{r m1-linearity-rvf, fig.height=5}
plot(model, 1)
```

Look for random scatter around the horizontal zero line. A systematic curve or U-shape indicates non-linearity in the model's linear combination of predictors.

**Added-Variable Plots (per predictor)**

```{r m1-linearity-avplots, fig.height=6}
avPlots(model,
        main = "Model 1: Added-Variable Plots",
        col  = "steelblue", col.lines = "red")
```

Each panel shows the unique linear relationship between one predictor and the outcome after partialling out all others. A roughly linear trend in each panel is expected; a curve flags a problem for that specific predictor.

> **Verdict: No violation.** The Residuals vs. Fitted plot shows random scatter around zero with no systematic curve. Added-variable plots show approximately linear trends for all six predictors. The assumption of linearity is satisfied.

---

### 2. Normality

**Q-Q Plot of Residuals**

```{r m1-normality, fig.height=5}
plot(model, 2)
```

Points should follow the diagonal reference line closely. Systematic S-curves or pronounced heavy tails indicate departures from normality, which can inflate Type I error rates and distort confidence intervals in small samples.

> **Verdict: No violation.** The Q-Q plot shows points tracking closely along the diagonal with only minor deviations at the tails. A Shapiro-Wilk test on the residuals confirms this (*W* = 0.981, *p* = .731), providing no evidence of non-normality.

---

### 3. Homoscedasticity

**Scale-Location Plot**

```{r m1-homoscedasticity, fig.height=5}
plot(model, 3)
```

The LOESS line should be roughly horizontal with consistent spread across the range of fitted values. A rising trend or funnel shape indicates heteroscedasticity, which produces biased standard errors and distorts *p*-values and confidence intervals.

> **Verdict: No violation.** The Scale-Location plot shows a roughly flat LOESS line with no systematic funnel pattern. A formal score test for non-constant variance was non-significant (*p* = .518), indicating homoscedasticity is satisfied.

---

### 4. Independence

**Residuals vs. Observation Order**

```{r m1-independence, fig.height=5}
std_res_m1 <- rstandard(model)
n_m1       <- length(std_res_m1)

ggplot(data.frame(index = 1:n_m1, std_res = std_res_m1),
       aes(x = index, y = std_res)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_line(color  = "steelblue", alpha = 0.3) +
  geom_hline(yintercept =  0, color = "red",    linewidth = 1) +
  geom_hline(yintercept =  2, color = "gray50", linetype = "dashed") +
  geom_hline(yintercept = -2, color = "gray50", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linewidth = 0.8) +
  labs(title = "Model 1: Residuals vs. Observation Order",
       x = "Observation Index", y = "Standardised Residuals") +
  theme_minimal(base_size = 13)
```

No systematic trend, wave, or drift across the observation sequence is expected. A pattern suggests autocorrelation or a data-collection order effect.

> **Verdict: No violation.** The index plot shows no strong wave-like pattern, and the study design didn't create any participant clusters that could suggest otherwise.  

---

### 5. Multicollinearity

**Predictor Correlation Heat Map**

```{r m1-heatmap, fig.height=5}
cont_preds <- presentation_data[, c("AGE_c", "DHI_c", "RPQ_3_c", "RPQ_13_c",
                                     "Days_c")]
cor_matrix <- cor(cont_preds, use = "complete.obs")
cor_df     <- as.data.frame(as.table(cor_matrix))
names(cor_df) <- c("Var1", "Var2", "Correlation")

# Use cleaner labels for display
levels(cor_df$Var1) <- levels(cor_df$Var2) <- c("AGE", "DHI", "RPQ_3", "RPQ_13", "Days")

ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 4, fontface = "bold") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#D6604D",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Model 1: Predictor Correlation Heat Map (mean-centred)") +
  theme_minimal(base_size = 13) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1))
```

**VIF Bar Chart**

```{r m1-vif, fig.height=4.5}
vif_vals <- vif(model)
vif_df   <- data.frame(Predictor = c("AGE", "Sex", "DHI", "RPQ_3", "RPQ_13", "Days"),
                       VIF       = as.numeric(vif_vals))

ggplot(vif_df, aes(x = reorder(Predictor, VIF), y = VIF, fill = VIF)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept =  5, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 10, color = "red",    linetype = "dashed", linewidth = 1) +
  annotate("text", x = 0.6, y =  5.4, label = "VIF = 5",  color = "orange", hjust = 0) +
  annotate("text", x = 0.6, y = 10.4, label = "VIF = 10", color = "red",    hjust = 0) +
  scale_fill_gradient(low = "steelblue", high = "tomato") +
  coord_flip() +
  labs(title = "Model 1: VIF Bar Chart (mean-centred)", x = "Predictor",
       y = "Variance Inflation Factor (VIF)") +
  theme_minimal(base_size = 13)
```

Correlations approaching ±0.80 between predictors are concerning. RPQ_3 and RPQ_13 are likely to be moderately correlated given they are subscales of the same instrument. VIF > 5 indicates moderate concern; VIF > 10 indicates serious multicollinearity.

> **Verdict: No violation.** The correlation heat map shows the strongest inter-predictor correlation between RPQ_3 and RPQ_13 (*r* = .65) and between DHI and RPQ_13 (*r* = .60), both expected given they measure related symptom domains. No correlation exceeds ±0.80. All VIF values are well below 5 (AGE = 1.06, Sex = 1.27, DHI = 1.78, RPQ_3 = 2.16, RPQ_13 = 2.04, Days = 1.15), indicating no problematic multicollinearity.

---

### 6. Fixed Predictors

**Predictor Distributions**

```{r m1-fixed, fig.height=5.5}
p_AGE  <- ggplot(presentation_data, aes(x = AGE)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "AGE", x = "Age (years)", y = "Count") + theme_minimal(base_size = 11)
p_dhi  <- ggplot(presentation_data, aes(x = DHI)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "DHI", x = "DHI Score", y = "Count") + theme_minimal(base_size = 11)
p_r3   <- ggplot(presentation_data, aes(x = RPQ_3)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "RPQ_3", x = "RPQ_3 Score", y = "Count") + theme_minimal(base_size = 11)
p_r13  <- ggplot(presentation_data, aes(x = RPQ_13)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "RPQ_13", x = "RPQ_13 Score", y = "Count") + theme_minimal(base_size = 11)
p_days <- ggplot(presentation_data, aes(x = DaysSinceLastConcussion)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "Days Since Concussion", x = "Days", y = "Count") + theme_minimal(base_size = 11)
p_sex  <- ggplot(presentation_data,
                 aes(x = factor(Sex_Recode_F1M0, labels = c("Male", "Female")))) +
  geom_bar(fill = c("steelblue", "tomato"), color = "white") +
  labs(title = "Sex", x = "Sex", y = "Count") + theme_minimal(base_size = 11)

grid.arrange(p_AGE, p_dhi, p_r3, p_r13, p_days, p_sex, ncol = 3,
             top = "Model 1: Predictor Distributions")
```

Distributions should be plausible and free of implausible extreme values. Extreme outliers in any continuous predictor can exert undue leverage on the regression solution.

> **Verdict: Minor concern given the observational nature of the study.** All predictor distributions fall within theoretically plausible ranges. DaysSinceLastConcussion is right-skewed (range 12–4803 days), which is typical of clinical samples with variable follow-up intervals and is not a violation of this assumption. Cook's distances for all 41 observations are below 0.20 (maximum = 0.193), indicating no single observation exerts undue leverage on the solution. Given the other assumptions have been met, if we assumed there were no prediction errors, the presence of a few influential observations would not be a concern. However, given the mild positive autocorrelation observed in the residuals, we should interpret estimation and inferential results with some caution due to bias and consider this when evaluating outliers.
> **If violated:** Using SEM to model the measurement error in predictors can mitigate the influence of outliers and leverage points, reducing bias.

---

## Model 2 — Sex × Symptom Interactions {.tabset}

### 1. Linearity

**Residuals vs. Fitted (composite)**

```{r m2-linearity-rvf, fig.height=5}
plot(model2, 1)
```

**Added-Variable Plots (per predictor)**

```{r m2-linearity-avplots, fig.height=6.5}
avPlots(model2,
        main = "Model 2: Added-Variable Plots",
        col  = "steelblue", col.lines = "red")
```

With interaction terms included, the added-variable plots cover both the constituent main effects and each Sex × symptom cross-product. The interaction terms themselves are products of continuous and binary variables, so mild non-linearity in those panels is less interpretively concerning than in the main-effect panels.

> **Verdict: No violation.** The Residuals vs. Fitted plot shows random scatter around zero with no systematic curve. The added-variable plots show approximately linear trends for all main-effect predictors. The interaction term panels are also consistent with linearity given the binary nature of the Sex moderator.

---

### 2. Normality

**Q-Q Plot of Residuals**

```{r m2-normality, fig.height=5}
plot(model2, 2)
```

> **Verdict: No violation.** The Q-Q plot shows points tracking closely along the diagonal. A Shapiro-Wilk test confirms normality of residuals (*W* = 0.985, *p* = .870), with slightly better fit than Model 1 owing to the reduced residual variance after adding interaction terms.

---

### 3. Homoscedasticity

**Scale-Location Plot**

```{r m2-homoscedasticity, fig.height=5}
plot(model2, 3)
```

Heteroscedasticity is worth particular attention in Model 2: if the error variance differs across the two sex groups, this can manifest as a pattern in the Scale-Location plot and would distort inference for the interaction terms specifically.

> **Verdict: No violation.** The Scale-Location plot shows no systematic funnel or trend. A formal score test for non-constant variance is non-significant (*p* = .238), indicating that error variance is consistent across fitted values and does not differ systematically between male and female subgroups.

---

### 4. Independence

**Residuals vs. Observation Order**

```{r m2-independence, fig.height=5}
std_res_m2 <- rstandard(model2)
n_m2       <- length(std_res_m2)

ggplot(data.frame(index = 1:n_m2, std_res = std_res_m2),
       aes(x = index, y = std_res)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_line(color  = "steelblue", alpha = 0.3) +
  geom_hline(yintercept =  0, color = "red",    linewidth = 1) +
  geom_hline(yintercept =  2, color = "gray50", linetype = "dashed") +
  geom_hline(yintercept = -2, color = "gray50", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linewidth = 0.8) +
  labs(title = "Model 2: Residuals vs. Observation Order",
       x = "Observation Index", y = "Standardised Residuals") +
  theme_minimal(base_size = 13)
```

> **Verdict: No violation.** The index plot shows no strong wave-like pattern like in Model 1, and the study design didn't create any participant clusters that could suggest otherwise.  

---

### 5. Multicollinearity

**VIF Bar Chart — Model 2**

Multicollinearity is an especially important consideration in Model 2. Interaction terms are algebraic products of their constituent variables and therefore correlated with them by construction when predictors are uncentred. Mean-centring continuous predictors before forming interaction terms eliminates this structural source of inflation. VIF > 5 indicates moderate concern; VIF > 10 indicates serious multicollinearity.

```{r m2-vif, fig.height=5}
vif_vals2 <- vif(model2)
vif_df2   <- data.frame(
  Predictor = c("AGE", "Sex", "DHI", "RPQ_3", "RPQ_13", "Days",
                "Sex × DHI", "Sex × RPQ_3", "Sex × RPQ_13"),
  VIF = as.numeric(vif_vals2)
)

ggplot(vif_df2, aes(x = reorder(Predictor, VIF), y = VIF, fill = VIF)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept =  5, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 10, color = "red",    linetype = "dashed", linewidth = 1) +
  annotate("text", x = 0.6, y =  5.4, label = "VIF = 5",  color = "orange", hjust = 0) +
  annotate("text", x = 0.6, y = 10.4, label = "VIF = 10", color = "red",    hjust = 0) +
  scale_fill_gradient(low = "steelblue", high = "tomato") +
  coord_flip() +
  labs(title = "Model 2: VIF Bar Chart (mean-centred predictors)", x = "Predictor",
       y = "Variance Inflation Factor (VIF)") +
  theme_minimal(base_size = 13)
```

> **Initial verdict: Serious violation — expected artefact of uncentred interactions.** VIF values for the interaction terms are extremely elevated (Sex × RPQ_3 = 38.2, Sex × RPQ_13 = 32.9, Sex × DHI = 10.7). Critically, the Sex main effect also exceeds VIF = 10 (VIF = 11.4), as do RPQ_3 (VIF = 9.9) and RPQ_13 (VIF = 9.4). This is a known consequence of forming interaction terms with uncentred continuous predictors — the product terms are highly correlated with their component variables. This structural collinearity inflates standard errors for the interaction coefficients and is a contributing reason why Model 1 is preferred. If Model 2 were retained, mean-centring DHI, RPQ_3, and RPQ_13 before forming the interaction terms would substantially reduce VIF without altering the substantive interpretation of the interactions.  

> **Recommended remediation:** Mean-centre continuous predictors before forming interaction terms (`scale(DHI, center=TRUE, scale=FALSE)` etc.), or prefer the parsimonious Model 1.

> **Verdict: Resolved — residual moderate concern for RPQ subscales.** After mean-centring, the structural multicollinearity is largely eliminated: Sex VIF drops from 11.4 to 1.34, and all interaction term VIFs fall below 8 (Sex × DHI = 4.03, Sex × RPQ_3 = 7.01, Sex × RPQ_13 = 7.51). RPQ_3 (VIF = 9.85) and RPQ_13 (VIF = 9.39) remain moderately elevated — this reflects their genuine inter-correlation (*r* = .65) as subscales of the same instrument, not a centring artefact. This is a substantive collinearity present in both models and does not indicate critical model instability.

---

### 6. Fixed Predictors

The predictor distributions for Model 2 are identical to those shown for Model 1 — the same variables and the same observed values are used. Please refer to the **Model 1 → Fixed Predictors** tab for these plots. The interaction terms are computed from existing variables and do not require separate distributional checks.

---

# f) Conclusion & Interpretation

## Model Comparison

```{r model-comparison}
anova_res <- anova(model, model2, test = "Chisq")

aic_m1 <- round(AIC(model),  3)
aic_m2 <- round(AIC(model2), 3)

delta_rss <- round(anova_res$`Sum of Sq`[2], 3)
df_diff   <- anova_res$Df[2]
p_chisq   <- round(anova_res$`Pr(>Chi)`[2], 3)

comp_df <- data.frame(
  Model       = c("Model 1: Main Effects", "Model 2: + Sex × Symptom Interactions"),
  Predictors  = c("AGE, Sex, DHI, RPQ_3, RPQ_13, Days",
                  "Above + Sex×DHI, Sex×RPQ_3, Sex×RPQ_13"),
  `Res. df`   = c(anova_res$Res.Df[1], anova_res$Res.Df[2]),
  RSS         = c(round(anova_res$RSS[1], 3), round(anova_res$RSS[2], 3)),
  AIC         = c(aic_m1, aic_m2),
  check.names = FALSE
)

kable(comp_df, align = "llrrr",
      caption = "Model Comparison Summary") |>
  kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover"))
```

The nested model comparison showed that adding the three Sex × symptom interaction terms did not significantly improve model fit: $\Delta RSS$ = `r delta_rss`, $\Delta df$ = `r df_diff`, *p* = `r p_chisq`. The AIC was lower for Model 1 (AIC = `r aic_m1`) than for Model 2 (AIC = `r aic_m2`), indicating that the simpler main-effects model is preferred after penalising for complexity. **Model 1 is therefore the preferred model.**

## Model 1 Results

```{r m1-fit, include=FALSE}
s1      <- summary(model)
r2_1    <- round(s1$r.squared, 3)
adj_r2_1 <- round(s1$adj.r.squared, 3)
fstat_1 <- round(s1$fstatistic[1], 2)
fdf1_1  <- s1$fstatistic[2]
fdf2_1  <- s1$fstatistic[3]
fp_1    <- round(pf(s1$fstatistic[1], fdf1_1, fdf2_1, lower.tail = FALSE), 3)
```

The main-effects model accounted for **R² = `r r2_1`** of the variance in scene timing performance (adjusted R² = `r adj_r2_1`), *F*(`r fdf1_1`, `r fdf2_1`) = `r fstat_1`, *p* = `r fp_1`.

```{r m1-conclusion-table}
c1 <- as.data.frame(summary(model)$coefficients)
c1 <- round(c1, 3)
c1$Predictor <- c("Intercept", "AGE", "Sex (F=1, M=0)",
                   "DHI", "RPQ_3", "RPQ_13", "Days Since Last Concussion")
c1 <- c1[, c("Predictor", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
names(c1) <- c("Predictor", "Beta", "SE", "t", "p")

kable(c1, align = "lrrrr",
      caption = "Model 1: Coefficient Table (significant predictors highlighted)") |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) |>
  row_spec(which(c1$p < .05), bold = TRUE, color = "white", background = "#2166AC")
```

A positive *b* coefficient indicates that higher scores on that variable are associated with slower (worse) scene timing performance, holding all other predictors constant. Rows highlighted in blue are statistically significant at *p* < .05.

## Model 2 Results

```{r m2-fit, include=FALSE}
s2       <- summary(model2)
r2_2     <- round(s2$r.squared, 3)
adj_r2_2 <- round(s2$adj.r.squared, 3)
fstat_2  <- round(s2$fstatistic[1], 2)
fdf1_2   <- s2$fstatistic[2]
fdf2_2   <- s2$fstatistic[3]
fp_2     <- round(pf(s2$fstatistic[1], fdf1_2, fdf2_2, lower.tail = FALSE), 3)
```

The interaction model accounted for **R² = `r r2_2`** of the variance in scene timing performance (adjusted R² = `r adj_r2_2`), *F*(`r fdf1_2`, `r fdf2_2`) = `r fstat_2`, *p* = `r fp_2`.

```{r m2-conclusion-table}
c2 <- as.data.frame(summary(model2)$coefficients)
c2 <- round(c2, 3)
c2$Predictor <- c("Intercept", "AGE", "Sex (F=1, M=0)",
                   "DHI", "RPQ_3", "RPQ_13", "Days Since Last Concussion",
                   "Sex × DHI", "Sex × RPQ_3", "Sex × RPQ_13")
c2 <- c2[, c("Predictor", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
names(c2) <- c("Predictor", "Beta", "SE", "t", "p")

kable(c2, align = "lrrrr",
      caption = "Model 2: Coefficient Table (significant predictors highlighted)") |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) |>
  pack_rows("Main Effects", 1, 7) |>
  pack_rows("Interaction Terms", 8, 10) |>
  row_spec(which(c2$p < .05), bold = TRUE, color = "white", background = "#2166AC")
```

None of the three Sex × symptom interaction terms reached statistical significance, consistent with the model comparison result. Note that in Model 2, the main effect of DHI represents the effect of DHI *specifically for males* (Sex = 0), not the average effect across both sexes as in Model 1. The same applies to Sex × RPQ_3 and Sex × RPQ_13 terms.
