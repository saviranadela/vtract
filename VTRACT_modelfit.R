library(tidyr)
library(dplyr)
library(lme4)
library(ggplot2)
library(broom)


data <- read.csv("dummy_vsite.csv", sep = ",")


data$Gender <- ifelse(data$Gender == "Male", 1, 0)
data$USMLE_Step_1 <- ifelse(data$USMLE_Step_1 == "Pass", 1, 0)



# -------------------- TABLE 1 --------------------

compute_stats <- function(data, variable) {
  
  # t-test by gender
  t_test <- t.test(data[[variable]] ~ data$Gender)
  
  stats <- data %>%
    group_by(Gender) %>%
    summarise(
      Mean = mean(get(variable), na.rm = TRUE),
      SD = sd(get(variable), na.rm = TRUE),
      n = sum(!is.na(get(variable))),
      .groups = "drop"
    )
  
  stats <- stats %>%
    mutate(
      Lower_CI = t_test$conf.int[1],
      Upper_CI = t_test$conf.int[2],
      p_value = t_test$p.value
    ) %>%
    mutate(Variable = variable)
  
  return(stats)
}

variables <- c("USMLE_Step_1", "USMLE_Step_2", "USMLE_Step_3",
               "VSITE_Score_1", "VSITE_Score_2", "VSITE_Score_3",
               "VSITE_Score_4", "VSITE_Score_5")

results <- lapply(variables, function(var) compute_stats(data, var))

summary_table <- do.call(rbind, results)

summary_table <- summary_table %>%
  select(Variable, Gender, Mean, SD, Lower_CI, Upper_CI, p_value) %>%
  arrange(Variable, Gender)

print(summary_table)

# -------------------- TABLE 2 --------------------

data_long <- data %>%
  pivot_longer(
    cols = starts_with("VSITE"),
    names_to = "PGY",
    names_prefix = "VSITE_Score_",
    values_to = "VSITE_Score"
  ) %>%
  mutate(PGY = as.numeric(PGY))

data_long <- data_long %>% filter(!is.na(VSITE_Score))

data_long

summary_table <- data_long %>%
  group_by(PGY) %>%
  summarise(
    Mean = mean(VSITE_Score, na.rm = TRUE),
    SD = sd(VSITE_Score, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

anova_result <- aov(VSITE_Score ~ factor(PGY), data = data_long)
anova_summary <- summary(anova_result)

f_statistic <- anova_summary[[1]]$`F value`[1]
p_value <- anova_summary[[1]]$`Pr(>F)`[1]


print(summary_table)


# -------------------- Growth Chart --------------------

growth_model <- lmer(
  VSITE_Score ~ PGY + USMLE_Step_1 + USMLE_Step_2 + USMLE_Step_3 + Gender + (1 | Resident_ID),
  data = data_long
)

summary(growth_model)

data$USMLE_Step_2 <- as.numeric(data$USMLE_Step_2)
data$USMLE_Step_3 <- as.numeric(data$USMLE_Step_3)

percentiles <- data.frame(
  Percentile = c(0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95),
  USMLE_Step_1 = quantile(data$USMLE_Step_1, probs = c(0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95), na.rm = TRUE),
  USMLE_Step_2 = quantile(data$USMLE_Step_2, probs = c(0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95), na.rm = TRUE),
  USMLE_Step_3 = quantile(data$USMLE_Step_3, probs = c(0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95), na.rm = TRUE),
  Gender = quantile(data$Gender, probs = c(0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95), na.rm = TRUE)
)

prediction_data <- expand.grid(
  PGY = 1:5,
  Percentile = c(0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95)
)

prediction_data <- prediction_data %>%
  left_join(percentiles, by = "Percentile")

prediction_data$Predicted_Score <- predict(growth_model, newdata = prediction_data, re.form = NA)

ggplot(prediction_data, aes(x = PGY, y = Predicted_Score, color = as.factor(Percentile), group = Percentile)) +
  geom_line(size = 1) +
  labs(
    title = "Population-Level Growth Chart with Percentiles",
    x = "PGY Year",
    y = "Predicted VSITE Score",
    color = "Percentile"
  ) +
  theme_minimal()


# -------------------- Individual At-risk --------------------

saveRDS(growth_model, "growth_model.rds")


# -------------------- Passing Probability --------------------

library(caret)
library(e1071)

dp <- data %>%
  filter(!is.na(data$VQE_Pass))


dp$VQE_Pass <- as.factor(dp$VQE_Pass)

set.seed(123)
train_index <- createDataPartition(dp$VQE_Pass, p = 0.8, list = FALSE)
train_data <- dp[train_index, ]
test_data <- dp[-train_index, ]

logit_model <- glm(
  VQE_Pass ~ USMLE_Step_1 + USMLE_Step_2 + USMLE_Step_3 + Gender + 
    VSITE_Score_1 + VSITE_Score_2 + VSITE_Score_3 + 
    VSITE_Score_4 + VSITE_Score_5,
  data = train_data,
  family = binomial
)

summary(logit_model)

test_data$Predicted_Prob <- predict(logit_model, newdata = test_data, type = "response")
test_data$Predicted_Class <- ifelse(test_data$Predicted_Prob > 0.5, 1, 0)

test_data

confusion_matrix <- confusionMatrix(as.factor(test_data$Predicted_Class), test_data$VQE_Pass)
print(confusion_matrix)

saveRDS(logit_model, "logit_model.rds")
