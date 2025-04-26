# One-Way ANOVA
# Load necessary packages
library(dplyr)
library(ggplot2)

# (1) Titanic Data: Age distribution across passenger classes

# Load Titanic data
titanic_data <- read.csv("/home/ibab/Downloads/titanic.csv", stringsAsFactors = TRUE)

# Plot histograms of age by class
par(mfrow = c(3, 1))  # Set up a 3x1 plot grid

hist(titanic_data$age[titanic_data$passenger_class == '1st'], breaks = 30,
     main = "1st Class", xlab = "Age", col = "pink")

hist(titanic_data$age[titanic_data$passenger_class == '2nd'], breaks = 30,
     main = "2nd Class", xlab = "Age", col = "lightblue")

hist(titanic_data$age[titanic_data$passenger_class == '3rd'], breaks = 30,
     main = "3rd Class", xlab = "Age", col = "lavender")

# Summary statistics by passenger class
titanic_summary <- titanic_data %>%
  group_by(passenger_class) %>%
  summarise(mean_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE))
print(titanic_summary)

# Fit ANOVA model and print ANOVA table
anova_titanic <- lm(age ~ passenger_class, data = titanic_data)
print(anova(anova_titanic))

# Post-hoc test: Tukey HSD
print(TukeyHSD(aov(anova_titanic)))

# Non-parametric alternative: Kruskal-Wallis test
print(kruskal.test(age ~ passenger_class, data = titanic_data))


# (2) Cuckoo Egg Size Problem

# Load cuckoo data
cuckoo_data <- read.csv("/home/ibab/Downloads/cuckooeggs.csv", stringsAsFactors = TRUE)

# Plot histogram by host species
ggplot(cuckoo_data, aes(x = egg_length)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  facet_wrap(~host_species)

# Summary statistics by host species
cuckoo_summary <- cuckoo_data %>%
  group_by(host_species) %>%
  summarise(mean_length = mean(egg_length, na.rm = TRUE),
            sd_length = sd(egg_length, na.rm = TRUE))
print(cuckoo_summary)

# ANOVA test for difference in egg lengths by host species
anova_cuckoo <- aov(egg_length ~ host_species, data = cuckoo_data)
print(summary(anova_cuckoo))

# Post-hoc test: Tukey HSD
print(TukeyHSD(anova_cuckoo))


# (3) Malaria vs. Maize Problem

# Load malaria data
malaria_data <- read.csv("/home/ibab/Downloads/malaria vs maize.csv", stringsAsFactors = TRUE)

# Histogram of malaria incidence by maize yield category
ggplot(malaria_data, aes(x = incidence_rate_per_ten_thousand)) +
  geom_histogram(bins = 20, fill = "pink", color = "black") +
  facet_wrap(~maize_yield)

# Standard deviation of incidence rate by maize yield
malaria_sd <- malaria_data %>%
  group_by(maize_yield) %>%
  summarise(sd_incidence = sd(incidence_rate_per_ten_thousand, na.rm = TRUE))
print(malaria_sd)

# Log-transform incidence rate
malaria_data$log_incidence <- log(malaria_data$incidence_rate_per_ten_thousand)

# Plot log-transformed histograms
ggplot(malaria_data, aes(x = log_incidence)) +
  geom_histogram(bins = 20, fill = "salmon", color = "black") +
  facet_wrap(~maize_yield)

# Standard deviation after log-transform
malaria_log_sd <- malaria_data %>%
  group_by(maize_yield) %>%
  summarise(sd_log = sd(log_incidence, na.rm = TRUE))
print(malaria_log_sd)

# ANOVA on log-transformed incidence
anova_malaria <- aov(log_incidence ~ maize_yield, data = malaria_data)
print(summary(anova_malaria))


# (4) Circadian Rhythms in Diseased Fruit Flies

# Load circadian health data
circadian_data <- read.csv("/home/ibab/Downloads/circadian mutant health.csv", stringsAsFactors = TRUE)

# Plot histogram of lifespan (days to death) by genotype
ggplot(circadian_data, aes(x = days_to_death)) +
  geom_histogram(bins = 20, fill = "plum", color = "black") +
  facet_wrap(~genotype)

# Kruskal-Wallis test for difference in lifespan
print(kruskal.test(days_to_death ~ genotype, data = circadian_data))
