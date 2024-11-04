library(tidyverse)
library(dplyr)
library(tibble)
library(plm)
library(ggplot2)
library(broom)
library(effects)

data <- read_csv("clean_wide_data.csv")

colnames(data)

ID_pers <- data |> pull(ID_pers)
Age_Group <- data |> pull(Age_Group)
Generation <- data |> pull(Generation)
Y_work_hours <- data |> pull(Y_work_hours)
Life_sat <- data |> pull(Life_sat)
Year <- data |> pull(Year)
Health_sat <- data |> pull(Health_sat)



data <- data %>%
  mutate(
    Birth_year = Year - Age,  # Calculates the year of birth
    Week_work_hours = Y_work_hours / 52,
    Generation = case_when(
      Birth_year >= 1901 & Birth_year <= 1927 ~ 6,  # Greatest Generation
      Birth_year >= 1928 & Birth_year <= 1945 ~ 5,  # Silent Generation
      Birth_year >= 1946 & Birth_year <= 1964 ~ 4,  # Baby Boomers
      Birth_year >= 1965 & Birth_year <= 1980 ~ 3,  # Generation X
      Birth_year >= 1981 & Birth_year <= 1996 ~ 2,  # Millennials
      Birth_year >= 1997 & Birth_year <= 2012 ~ 1,  # Generation Z
    )
  )



model_fe <- plm(Life_sat ~ Y_work_hours + Generation + Health_sat,
                data = data,
                model = "within",
                index = c("ID_pers", "Year"))

summary(model_fe)
print(coef(model_fe))

#model removes Generation because it's Time-invariant Variable and hence can't be measured using fixed effect model as it doesn't vary
#thus we need to use random effect model instead of fixed effect regressions 

model_re <- plm(Life_sat ~ Generation + Week_work_hours + Health_sat,
                data = data,
                model = "random",
                index = c("ID_pers", "Year"))

summary(model_re)
print(coef(model_re))



#  descriptive statistics for life satisfaction by generation
descriptive_stats <- data %>%
  group_by(Generation) %>%
  summarise(
    mean_life_sat = mean(Life_sat, na.rm = TRUE),
    sd_life_sat = sd(Life_sat, na.rm = TRUE),
    count = n()
  )

print(descriptive_stats)


# Two-way ANOVA model
anova_model <- aov(Life_sat ~ Generation * Year, data = data)

summary(anova_model)



data$Generation <- as.factor(data$Generation)
data$Year <- as.factor(data$Year)

tukey_test <- TukeyHSD(anova_model, "Generation")

print(tukey_test)

# Visualisazation of Tukey test with error bars at sig 0.05

tukey_df <- tidy(tukey_test)
tukey_df$significant <- tukey_df$adj.p.value < 0.05

ggplot(tukey_df, aes(x = contrast, y = estimate, color = significant)) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Tukey HSD Test Results for Generations",
       x = "Generation Comparison",
       y = "Difference in Mean Life Satisfaction") +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                     labels = c("Not Significant", "Significant")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = guide_legend(title = "Significance (p < 0.0.5)"))


#Life_sat based on the year and generation
descriptive_stats1 <- data %>%
  group_by(Year, Generation) %>%
  summarise(
    mean_life_sat = mean(Life_sat, na.rm = TRUE),
    sd_life_sat = sd(Life_sat, na.rm = TRUE),
    count = n()
  )

print(descriptive_stats1)
