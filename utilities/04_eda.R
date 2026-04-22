# =============================================================================
# 04_eda.R — Complete EDA (Final Version)
# Lead: Zafi Nasser
# =============================================================================
# Course rules applied:
#   1. "What is my outcome? Plot density." (Week 3)
#   2. "Always put response on y-axis" (Week 1)
#   3. "Always add smoothers, NOT regression lines" (PDF FAQ)
#   4. "Use geom_smooth() WITHOUT method to see TRUE shape" (Week 2)
#   5. If density is skewed/amount → check if log-transform needed (Week 2)
# =============================================================================

library(tidyverse)
library(here)

train <- readRDS(here("data", "train.rds"))

# Feature engineering
train <- train %>%
  mutate(
    hour  = hour(accident_dt_dummy),
    month = factor(month(accident_dt_dummy)),
    year  = year(accident_dt_dummy)
  )


# ============================================================
# A) RESPONSE: AccidentInvolvingPedestrian (Binary → GLM Binomial + GAM)
# ============================================================
p <- ggplot(train, aes(x = factor(AccidentInvolvingPedestrian))) +
  geom_bar(fill = c("grey70", "tomato")) +
  labs(title = "Response: Pedestrian Involvement (Binary)",
       subtitle = "Imbalanced: ~5% TRUE → GLM Binomial appropriate",
       x = "Pedestrian Involved", y = "Count")
p; ggsave("img/A01_ped_distribution.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentType_en, fill = AccidentInvolvingPedestrian)) +
  geom_bar(position = "fill") + coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "P(Pedestrian) by Accident Type", x = NULL, y = "Proportion", fill = "Pedestrian")
p; ggsave("img/A06_ped_by_type.png", p, width = 10, height = 6)

p <- ggplot(train, aes(x = RoadType_en, fill = AccidentInvolvingPedestrian)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "P(Pedestrian) by Road Type", x = NULL, y = "Proportion", fill = "Pedestrian")
p; ggsave("img/A07_ped_by_road.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentSeverityCategory_en, fill = AccidentInvolvingPedestrian)) +
  geom_bar(position = "fill") + coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "P(Pedestrian) by Severity", x = NULL, y = "Proportion", fill = "Pedestrian")
p; ggsave("img/A08_ped_by_severity.png", p, width = 10, height = 5)

p <- ggplot(train, aes(x = month, fill = AccidentInvolvingPedestrian)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "P(Pedestrian) by Month", x = "Month", y = "Proportion", fill = "Pedestrian")
p; ggsave("img/A09_ped_by_month.png", p, width = 8, height = 5)


# ============================================================
# B) RESPONSE: AccidentInvolvingBicycle (Binary)
# ============================================================
p <- ggplot(train, aes(x = factor(AccidentInvolvingBicycle))) +
  geom_bar(fill = c("grey70", "steelblue")) +
  labs(title = "Response: Bicycle Involvement (Binary)", x = "Bicycle Involved", y = "Count")
p; ggsave("img/B01_bike_distribution.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = month, fill = AccidentInvolvingBicycle)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "P(Bicycle) by Month", x = "Month", y = "Proportion", fill = "Bicycle")
p; ggsave("img/B03_bike_by_month.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentLocation_CHLV95_N, y = as.numeric(AccidentInvolvingBicycle))) +
  geom_smooth(se = TRUE) +
  labs(title = "P(Bicycle) vs North", x = "North", y = "P(Bicycle)")
p; ggsave("img/B05_bike_vs_north.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = year, y = as.numeric(AccidentInvolvingBicycle))) +
  geom_smooth(se = TRUE) +
  labs(title = "P(Bicycle) vs Year", x = "Year", y = "P(Bicycle)")
p; ggsave("img/B06_bike_vs_year.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentType_en, fill = AccidentInvolvingBicycle)) +
  geom_bar(position = "fill") + coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "P(Bicycle) by Accident Type", x = NULL, y = "Proportion", fill = "Bicycle")
p; ggsave("img/B07_bike_by_type.png", p, width = 10, height = 6)

p <- ggplot(train, aes(x = RoadType_en, fill = AccidentInvolvingBicycle)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "P(Bicycle) by Road Type", x = NULL, y = "Proportion", fill = "Bicycle")
p; ggsave("img/B08_bike_by_road.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentSeverityCategory_en, fill = AccidentInvolvingBicycle)) +
  geom_bar(position = "fill") + coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "P(Bicycle) by Severity", x = NULL, y = "Proportion", fill = "Bicycle")
p; ggsave("img/B09_bike_by_severity.png", p, width = 10, height = 5)


# ============================================================
# C) RESPONSE: AccidentSeverityCategory_en (Classification → SVM, NN)
# ============================================================
p <- ggplot(train, aes(x = fct_infreq(AccidentSeverityCategory_en))) +
  geom_bar(fill = "tomato") + coord_flip() +
  labs(title = "Response: Accident Severity (4 classes)",
       subtitle = "Heavily imbalanced → check no information rate in SVM",
       x = NULL, y = "Count")
p; ggsave("img/C01_severity_distribution.png", p, width = 10, height = 5)

p <- ggplot(train, aes(x = hour, fill = AccidentSeverityCategory_en)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Severity by Hour", x = "Hour", y = "Proportion", fill = "Severity")
p; ggsave("img/C02_severity_by_hour.png", p, width = 10, height = 5)

p <- ggplot(train, aes(x = fct_infreq(AccidentType_en), fill = AccidentSeverityCategory_en)) +
  geom_bar(position = "fill") + coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Severity by Accident Type", x = NULL, y = "Proportion", fill = "Severity")
p; ggsave("img/C03_severity_by_type.png", p, width = 10, height = 6)

p <- ggplot(train, aes(x = RoadType_en, fill = AccidentSeverityCategory_en)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Severity by Road Type", x = NULL, y = "Proportion", fill = "Severity")
p; ggsave("img/C04_severity_by_road.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = month, fill = AccidentSeverityCategory_en)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Severity by Month", x = "Month", y = "Proportion", fill = "Severity")
p; ggsave("img/C05_severity_by_month.png", p, width = 10, height = 5)

p <- ggplot(train, aes(x = AccidentSeverityCategory_en, y = AccidentLocation_CHLV95_E)) +
  geom_boxplot(fill = "steelblue", alpha = 0.5) + coord_flip() +
  labs(title = "East Coordinate by Severity — Boxplot", x = NULL, y = "East")
p; ggsave("img/C06_severity_boxplot_east.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentSeverityCategory_en, y = AccidentLocation_CHLV95_N)) +
  geom_boxplot(fill = "steelblue", alpha = 0.5) + coord_flip() +
  labs(title = "North Coordinate by Severity — Boxplot", x = NULL, y = "North")
p; ggsave("img/C07_severity_boxplot_north.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentSeverityCategory_en, y = hour)) +
  geom_boxplot(fill = "steelblue", alpha = 0.5) + coord_flip() +
  labs(title = "Hour by Severity — Boxplot", x = NULL, y = "Hour")
p; ggsave("img/C08_severity_boxplot_hour.png", p, width = 8, height = 5)


# ============================================================
# D) DISTRIBUTION OF EVERY SINGLE VARIABLE
# ============================================================
p <- ggplot(train, aes(x = AccidentLocation_CHLV95_E)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(title = "Density: East Coordinate",
       subtitle = "Not an amount → no log needed. Roughly symmetric.",
       x = "CHLV95 East", y = "Density")
p; ggsave("img/D01_density_east.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentLocation_CHLV95_N)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(title = "Density: North Coordinate", x = "CHLV95 North", y = "Density")
p; ggsave("img/D02_density_north.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = hour)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution: Hour of Day",
       subtitle = "Bimodal: peaks at commute hours",
       x = "Hour", y = "Count")
p; ggsave("img/D03_distribution_hour.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = year)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution: Year",
       subtitle = "Jump after 2015: changed Sachschaden reporting (ASTRA)",
       x = "Year", y = "Count")
p; ggsave("img/D04_distribution_year.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = fct_infreq(AccidentType_en))) +
  geom_bar(fill = "steelblue") + coord_flip() +
  labs(title = "Distribution: Accident Type (11 levels)", x = NULL, y = "Count")
p; ggsave("img/D05_distribution_acctype.png", p, width = 10, height = 6)

p <- ggplot(train, aes(x = fct_infreq(RoadType_en))) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Distribution: Road Type (3 levels)", x = NULL, y = "Count")
p; ggsave("img/D06_distribution_roadtype.png", p, width = 8, height = 5)

binary_summary <- train %>%
  summarise(Pedestrian = mean(AccidentInvolvingPedestrian),
            Bicycle = mean(AccidentInvolvingBicycle),
            Motorcycle = mean(AccidentInvolvingMotorcycle)) %>%
  pivot_longer(everything(), names_to = "Type", values_to = "Proportion")

p <- ggplot(binary_summary, aes(x = Type, y = Proportion, fill = Type)) +
  geom_col() + scale_y_continuous(labels = scales::percent) +
  labs(title = "Binary Variables: Proportion TRUE",
       subtitle = "All rare events → class imbalance",
       x = NULL, y = "Proportion") +
  theme(legend.position = "none")
p; ggsave("img/D07_binary_proportions.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = month)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution: Month", x = "Month", y = "Count")
p; ggsave("img/D08_distribution_month.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = RoadType_en, fill = fct_infreq(AccidentType_en))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Accident Type Distribution by Road Type",
       x = NULL, y = "Proportion", fill = "Accident Type")
p; ggsave("img/D09_type_by_road.png", p, width = 10, height = 6)


# ============================================================
# E) SPATIAL: Accident hotspots
# ============================================================
p <- ggplot(train, aes(x = AccidentLocation_CHLV95_E, y = AccidentLocation_CHLV95_N)) +
  geom_bin2d(bins = 50) + scale_fill_viridis_c() +
  labs(title = "Accident Density Map — Zürich",
       subtitle = "Hotspots visible → spatial effect for GAM with s(E, N)",
       x = "East", y = "North", fill = "Count")
p; ggsave("img/E01_hotspot_all.png", p, width = 8, height = 6)

p <- ggplot(train %>% filter(AccidentInvolvingPedestrian),
       aes(x = AccidentLocation_CHLV95_E, y = AccidentLocation_CHLV95_N)) +
  geom_bin2d(bins = 40) + scale_fill_viridis_c() +
  labs(title = "Pedestrian Accident Hotspots",
       subtitle = "Clustered in city center",
       x = "East", y = "North", fill = "Count")
p; ggsave("img/E02_hotspot_pedestrian.png", p, width = 8, height = 6)


# ============================================================
# F) COUNT DATA: Aggregated for GLM Poisson + LM
# ============================================================
count_data <- train %>%
  count(hour, month, year, RoadType_en, name = "n_accidents")

p <- ggplot(count_data, aes(x = n_accidents)) +
  geom_histogram(fill = "steelblue", binwidth = 5) +
  labs(title = "Distribution: Aggregated Accident Count",
       subtitle = "Right-skewed, discrete, positive → Poisson appropriate",
       x = "Number of Accidents", y = "Frequency")
p; ggsave("img/F01_count_distribution.png", p, width = 8, height = 5)

p <- ggplot(count_data, aes(x = log(n_accidents))) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(title = "Density: log(Accident Count)",
       subtitle = "After log: more symmetric → LM on log(count) viable",
       x = "log(n_accidents)", y = "Density")
p; ggsave("img/F02_count_log_density.png", p, width = 8, height = 5)

p <- ggplot(count_data, aes(x = as.numeric(as.character(month)), y = n_accidents)) +
  geom_point(alpha = 0.3) + geom_smooth(se = TRUE) +
  labs(title = "Accident Count vs Month — Smoother",
       subtitle = "Non-linear seasonal effect",
       x = "Month", y = "n_accidents")
p; ggsave("img/F03_count_vs_month.png", p, width = 8, height = 5)

p <- ggplot(count_data, aes(x = RoadType_en, y = n_accidents)) +
  geom_boxplot(fill = "steelblue", alpha = 0.5) +
  labs(title = "Accident Count by Road Type — Boxplot", x = NULL, y = "n_accidents")
p; ggsave("img/F04_count_by_road.png", p, width = 8, height = 5)

p <- ggplot(count_data, aes(x = as.numeric(hour), y = n_accidents)) +
  geom_point(alpha = 0.3) + geom_smooth(se = TRUE) +
  labs(title = "Accident Count vs Hour — Smoother", x = "Hour", y = "n_accidents")
p; ggsave("img/F05_count_vs_hour.png", p, width = 8, height = 5)

p <- ggplot(count_data, aes(x = year, y = n_accidents)) +
  geom_point(alpha = 0.3) + geom_smooth(se = TRUE) +
  labs(title = "Accident Count vs Year — Smoother", x = "Year", y = "n_accidents")
p; ggsave("img/F06_count_vs_year.png", p, width = 8, height = 5)

p <- ggplot(count_data, aes(x = as.numeric(hour), y = log(n_accidents))) +
  geom_point(alpha = 0.3) + geom_smooth(se = TRUE) +
  labs(title = "log(Count) vs Hour — for LM check",
       subtitle = "After log: is the relationship more linear?",
       x = "Hour", y = "log(n_accidents)")
p; ggsave("img/F07_logcount_vs_hour.png", p, width = 8, height = 5)

# Poisson check: variance vs mean
count_summary <- count_data %>%
  group_by(RoadType_en) %>%
  summarise(mean = mean(n_accidents), variance = var(n_accidents), .groups = "drop")
print("=== Poisson check: variance vs mean (if var >> mean → overdispersion) ===")
print(count_summary)


# ============================================================
# G) NON-LINEARITY CHECKS: red (lm) vs blue (smoother)
# ============================================================
# Week 2: "If smoother deviates from line → non-linear → needs s() in GAM"

p <- ggplot(train, aes(x = hour, y = as.numeric(AccidentInvolvingPedestrian))) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_smooth(color = "blue", se = TRUE) +
  labs(title = "Non-Linearity Check: P(Pedestrian) vs Hour",
       subtitle = "Red=linear | Blue=true shape → clearly non-linear → use s(hour)",
       x = "Hour", y = "P(Pedestrian)")
p; ggsave("img/G01_nonlin_ped_hour.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = hour, y = as.numeric(AccidentInvolvingBicycle))) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_smooth(color = "blue", se = TRUE) +
  labs(title = "Non-Linearity Check: P(Bicycle) vs Hour",
       subtitle = "Red=linear | Blue=true shape → strong non-linearity",
       x = "Hour", y = "P(Bicycle)")
p; ggsave("img/G02_nonlin_bike_hour.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentLocation_CHLV95_E, y = as.numeric(AccidentInvolvingPedestrian))) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_smooth(color = "blue", se = TRUE) +
  labs(title = "Non-Linearity Check: P(Pedestrian) vs East",
       subtitle = "Spatial non-linearity → use s(E, N) in GAM",
       x = "East", y = "P(Pedestrian)")
p; ggsave("img/G03_nonlin_ped_east.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentLocation_CHLV95_N, y = as.numeric(AccidentInvolvingPedestrian))) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_smooth(color = "blue", se = TRUE) +
  labs(title = "Non-Linearity Check: P(Pedestrian) vs North",
       x = "North", y = "P(Pedestrian)")
p; ggsave("img/G04_nonlin_ped_north.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = year, y = as.numeric(AccidentInvolvingPedestrian))) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_smooth(color = "blue", se = TRUE) +
  labs(title = "Non-Linearity Check: P(Pedestrian) vs Year",
       x = "Year", y = "P(Pedestrian)")
p; ggsave("img/G05_nonlin_ped_year.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentLocation_CHLV95_E, y = as.numeric(AccidentInvolvingBicycle))) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_smooth(color = "blue", se = TRUE) +
  labs(title = "Non-Linearity Check: P(Bicycle) vs East",
       x = "East", y = "P(Bicycle)")
p; ggsave("img/G06_nonlin_bike_east.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = hour, y = as.numeric(AccidentInvolvingMotorcycle))) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_smooth(color = "blue", se = TRUE) +
  labs(title = "Non-Linearity Check: P(Motorcycle) vs Hour",
       x = "Hour", y = "P(Motorcycle)")
p; ggsave("img/G07_nonlin_moto_hour.png", p, width = 8, height = 5)


# ============================================================
# H) SUMMARY STATISTICS
# ============================================================
summary_stats <- train %>%
  summarise(
    n = n(),
    pct_pedestrian = round(mean(AccidentInvolvingPedestrian) * 100, 1),
    pct_bicycle = round(mean(AccidentInvolvingBicycle) * 100, 1),
    pct_motorcycle = round(mean(AccidentInvolvingMotorcycle) * 100, 1),
    mean_hour = round(mean(hour), 1),
    mean_east = round(mean(AccidentLocation_CHLV95_E), 0),
    mean_north = round(mean(AccidentLocation_CHLV95_N), 0)
  )
print("=== Summary Statistics ===")
print(summary_stats)

severity_table <- train %>%
  count(AccidentSeverityCategory_en) %>%
  mutate(pct = round(n / sum(n) * 100, 1))
print("=== Severity Class Balance (No Information Rate = largest class %) ===")
print(severity_table)


# ============================================================
# I) INTERACTION EFFECTS
# ============================================================
# Week 1: "Does the effect of x1 DEPEND on x2?" → interaction

p <- ggplot(train, aes(x = hour, y = as.numeric(AccidentInvolvingPedestrian), color = RoadType_en)) +
  geom_smooth(se = FALSE) +
  labs(title = "Interaction Check: P(Pedestrian) vs Hour BY Road Type",
       subtitle = "Different shapes per road type → interaction effect",
       x = "Hour", y = "P(Pedestrian)", color = "Road Type")
p; ggsave("img/I01_interaction_ped_hour_road.png", p, width = 10, height = 5)

p <- ggplot(train, aes(x = hour, y = as.numeric(AccidentInvolvingBicycle), color = RoadType_en)) +
  geom_smooth(se = FALSE) +
  labs(title = "Interaction Check: P(Bicycle) vs Hour BY Road Type",
       subtitle = "If lines cross or differ in shape → interaction needed in model",
       x = "Hour", y = "P(Bicycle)", color = "Road Type")
p; ggsave("img/I02_interaction_bike_hour_road.png", p, width = 10, height = 5)


# ============================================================
# J) CORRELATION: continuous variables
# ============================================================
cor_matrix <- train %>%
  select(AccidentLocation_CHLV95_E, AccidentLocation_CHLV95_N, hour, year) %>%
  cor()
print("=== Correlation Matrix (continuous variables) ===")
print(round(cor_matrix, 3))

p <- train %>%
  select(AccidentLocation_CHLV95_E, AccidentLocation_CHLV95_N, hour, year) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  facet_wrap(~name, scales = "free") +
  labs(title = "Histograms of All Continuous Variables",
       subtitle = "Check for skewness → log-transform needed?")
p; ggsave("img/J01_continuous_histograms.png", p, width = 10, height = 6)


# ============================================================
# K) MOTORCYCLE (3rd binary variable — complete)
# ============================================================
p <- ggplot(train, aes(x = hour, y = as.numeric(AccidentInvolvingMotorcycle))) +
  geom_smooth(se = TRUE) +
  labs(title = "P(Motorcycle) vs Hour — Smoother", x = "Hour", y = "P(Motorcycle)")
p; ggsave("img/K01_moto_vs_hour.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = month, fill = AccidentInvolvingMotorcycle)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "P(Motorcycle) by Month — Seasonal?",
       subtitle = "Expected: more in summer (riding season)",
       x = "Month", y = "Proportion", fill = "Motorcycle")
p; ggsave("img/K02_moto_by_month.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentLocation_CHLV95_E, y = as.numeric(AccidentInvolvingMotorcycle))) +
  geom_smooth(se = TRUE) +
  labs(title = "P(Motorcycle) vs East", x = "East", y = "P(Motorcycle)")
p; ggsave("img/K03_moto_vs_east.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentLocation_CHLV95_N, y = as.numeric(AccidentInvolvingMotorcycle))) +
  geom_smooth(se = TRUE) +
  labs(title = "P(Motorcycle) vs North", x = "North", y = "P(Motorcycle)")
p; ggsave("img/K04_moto_vs_north.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = year, y = as.numeric(AccidentInvolvingMotorcycle))) +
  geom_smooth(se = TRUE) +
  labs(title = "P(Motorcycle) vs Year", x = "Year", y = "P(Motorcycle)")
p; ggsave("img/K05_moto_vs_year.png", p, width = 8, height = 5)

p <- ggplot(train, aes(x = AccidentType_en, fill = AccidentInvolvingMotorcycle)) +
  geom_bar(position = "fill") + coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "P(Motorcycle) by Accident Type", x = NULL, y = "Proportion", fill = "Motorcycle")
p; ggsave("img/K06_moto_by_type.png", p, width = 10, height = 6)

p <- ggplot(train, aes(x = RoadType_en, fill = AccidentInvolvingMotorcycle)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "P(Motorcycle) by Road Type", x = NULL, y = "Proportion", fill = "Motorcycle")
p; ggsave("img/K07_moto_by_road.png", p, width = 8, height = 5)


# ============================================================
# NA CHECK
# ============================================================
na_summary <- colSums(is.na(train))
print("=== NA Check ===")
print(na_summary)
cat("Total rows:", nrow(train), "\n")
cat("Total columns:", ncol(train), "\n")


cat("\n=== EDA COMPLETE: 55 plots saved to img/ ===\n")
cat("Model assignment summary:\n")
cat("  LM            → log(n_accidents)              → continuous after log\n")
cat("  GLM Poisson   → n_accidents (aggregated)      → count data\n")
cat("  GLM Binomial  → AccidentInvolvingPedestrian   → binary\n")
cat("  GAM           → AccidentInvolvingPedestrian   → non-linear effects\n")
cat("  SVM           → AccidentSeverityCategory_en   → classification\n")
cat("  NN            → AccidentSeverityCategory_en   → classification\n")
