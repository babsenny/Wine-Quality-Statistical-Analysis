#install packages
install.packages("datarium")
install.packages("readxl")   
install.packages("tidyverse")
install.packages("corrplot")
install.packages("rcompanion")
install.packages("psych")   
install.packages("GGally")  
install.packages("ggpubr")  
install.packages("tidyr")  
install.packages("car")
install.packages("randomForest")


#load libraries
library(GGally)
library(psych)
library(datarium)
library(tidyverse)
library(corrplot)
library(rcompanion)
library(readxl)
library(ggpubr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(car)
library(randomForest)


#load the datasets
red   <- read_excel("winequality-red.xlsx")     
white <- read_excel("Winequality-white.xlsx")   

#viewing the dataset
head(red)
head(white)

# Adding the type variable
red$type <- "red"
white$type <- "white"

# Combine the dataset
wine <- rbind(red, white)

# Convert type to factor
wine$type <- as.factor(wine$type)

# Dimensions
dim(wine)

# Structure 
str(wine)

# Summary of all variables
summary(wine)

# First few rows
head(wine)

# Count the number of red vs white wines
table(wine$type)

#ordering the quality column
wine$quality_factor <- ordered(wine$quality)


# 2 Initial Overview: Structure, Missing Values, Summary, and duplicates

#checking for missing values
colSums(is.na(wine))
#there are no nulls in the dataset

#checking for duplicates
sum(duplicated(wine))
#there are 1177 duplicates present which would be removed

# dropping the dupliccates
wine <- wine[!duplicated(wine), ]


######## 3 ---- EXPLORATORY DATA ANALYSIS ----------------------

# checking the distribution of both red and white wine in the dataset
table(wine$type) 

#descriptive statistics
describeBy(wine[,1:12], group = wine$type)

#summary statistics for red wine
wine %>%
  filter(type == 'red') %>%
  summary()

# summary statistics for white wine
wine %>%
  filter(type == 'white') %>%
  summary()


# distribution of our red and white wine by count (bar chart)
ggplot(wine, aes(x = type, fill = type)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Count of Red vs White Wines", x = "Type", y = "Count")

#alcohol by wine type
ggplot(wine, aes(x = type, y = alcohol, fill = type)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Alcohol Content by Wine Type", x = "Wine Type", y = "Alcohol")

#summary by wine type
wine_long <- wine %>%
  # keep only type + numeric columns
  select(type, where(is.numeric)) %>%
  pivot_longer(
    cols = -type,
    names_to = "variable",
    values_to = "value"
  )
ggplot(wine_long, aes(x = value, fill = type)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions of Numeric Variables by Wine Type")


#Density plots of quality
ggplot(wine, aes(x = quality, fill = type)) +
  geom_density(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Quality Score Distribution: Red vs White Wine")

# Boxplot quality by type
ggplot(wine, aes(x = type, y = quality, fill = type)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Quality Comparison by Wine Type", x = "Wine Type", y = "Quality")


#numeric distribution of the numeric columns using a density plot

#selecting only the numeric columns
numeric_vars <- names(wine)[sapply(wine, is.numeric)]
numeric_vars <- setdiff(numeric_vars, "quality")

for (v in numeric_vars) {
  
  p <- ggplot(wine, aes(x = .data[[v]], colour = type, fill = type)) +
    geom_density(alpha = 0.4) +
    theme_minimal(base_size = 12) +
    labs(
      title = paste("Density of", v, "by Wine Type"),
      x = v,
      y = "Density"
    )
  
  print(p)
}


######## ------------------ Correlation Analysis --------------------------------- ####

wine_num <- wine %>% select(where(is.numeric))

#correlation matrix for all wines
GGally::ggcorr(
  wine_num,
  method = c("complete.obs", "spearman"),
  label = TRUE,
  label_round = 2,
  label_size = 3,
  hjust = 0.9
) +
  ggtitle("Correlation Matrix – Red & White Wines")

#Seperate correlation matrices for red vs white
wine_red  <- wine %>% filter(type == "red") %>% select(where(is.numeric))
wine_white <- wine %>% filter(type == "white") %>% select(where(is.numeric))

#correlation matrix for red wines
GGally::ggcorr(
  wine_red,
  method = c("complete.obs", "spearman"),
  label = TRUE,
  label_round = 2,
  label_size = 3,
  hjust = 0.9
) +
  ggtitle("Correlation Matrix – Red Wine")

#correlation matrix for white wines
GGally::ggcorr(
  wine_white,
  method = c("complete.obs", "spearman"),
  label = TRUE,
  label_round = 2,
  label_size = 3,
  hjust = 0.9
) +
  ggtitle("Correlation Matrix – White Wine")


#### Correlation Matrix using spearman

#Applying spearman correlation for all wines
round(cor(wine_num, method = "spearman"), digit=2)

#applying spearman correlation for red wines
round(cor(wine_red, method = "spearman"), digit=2)

#applying spearman correlation for White wines
round(cor(wine_white, method = "spearman"), digit=2)

#pairwise scatterplot matrix
ggpairs(wine[, c(numeric_vars, "type")])

### -------------- Normality tests ------------------------- ####

# Convert to long format
wine_long <- wine %>%
  pivot_longer(cols = all_of(numeric_vars),
               names_to = "variable",
               values_to = "value")

# Faceted Q-Q plot
ggplot(wine_long, aes(sample = value)) +
  stat_qq(geom = "point", size = 1, color = "blue") +
  stat_qq_line() +
  facet_wrap(~ variable, scales = "free")

# Usinf shapiro to check for normality

shapiro_results <- numeric_vars %>%
  tibble(variable = .) %>%
  rowwise() %>%
  mutate(
    values = list(na.omit(wine[[variable]])),
    values = list(if (length(values) > 5000) sample(values, 5000) else values),
    p_value = shapiro.test(values)$p.value,
    normality = if_else(p_value > 0.05, "Normal", "Non-normal")
  ) %>%
  select(variable, p_value, normality) %>%
  ungroup()

print(shapiro_results)


#### ---------------- Outlier Detection ---------------- ####


#boxplot to visualise all the variables

wine_long_plot <- wine %>%
  select(type, where(is.numeric)) %>%
  pivot_longer(
    cols = -type,
    names_to = "variable",
    values_to = "value"
  )

ggplot(wine_long_plot, aes(x = type, y = value, fill = type)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.4) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Wine Type",
    y = "Value",
    title = "Distribution of Chemical Properties by Wine Type"
  ) +
  theme(legend.position = "none")

#from the boxplot, we can see there are outliers present in the variables

#identify outliers using the IQR Method
iqr_outlier_summary <- wine %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR,
    outlier_pct = mean(value < lower | value > upper, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(outlier_pct))

iqr_outlier_summary

#putting the numeric columns in a bucket
predictors_raw <- names(wine)[
  sapply(wine, is.numeric) &
    names(wine) != "quality"
]
 
#Log transform with outlier rates greater than 3%
vars_to_log <- iqr_outlier_summary %>%
  filter(outlier_pct > 3) %>%
  pull(variable)

vars_to_log

#applying log transformation to these variables
wine <- wine %>%
  mutate(
    across(
      all_of(vars_to_log),
      log1p,
      .names = "{.col}_log"
    )
  )

#final variables for regression analysis
model_vars <- c(
  predictors_raw,
  paste0(vars_to_log, "_log")
)

model_vars

#adding the quality column
model_data <- wine %>%
  select(quality, all_of(model_vars))

model_data




# --------------- Building the regression Models -------------- #

#Building a Simple Regression Model
#Y = Quality
#X = Alcohol

model_1 <-lm(quality ~ alcohol, wine)
summary.lm(model_1)

#plotting the fitted regression line
plot(quality ~ alcohol, wine,
     col = "blue",
     main = "Regression: Alcohol Quality & Wine",
     xlab = "Alcohol",
     ylab = "Quality")


# Residual Independence Check
plot(model_1, which = 1)

# Check Normality of Residuals
plot(model_1, which = 2)

# Check for Homoscedasticity 
plot(model_1, which = 3)




#### ---- Building a multiple linear regression Model ---- #####

mlr1 <- lm(
  quality ~ alcohol + 
    `volatile acidity` + 
    sulphates,
  data = wine
)
summary(mlr1)
## Has an adjusted R-square of approximately 28percent

mlr2 <- lm(
  quality ~ alcohol + 
    `volatile acidity` + 
    density + 
    sulphates_log + 
    chlorides_log +
    `citric acid` + 
    `total sulfur dioxide` + 
    `fixed acidity`,
  data = wine
)
summary (mlr2)

## Has an adjusted R-square of approximately 29percent


mlr3 <- lm(
  quality ~ alcohol + 
    `volatile acidity` + 
    density + 
    sulphates + 
    chlorides +
    `citric acid` + 
    `total sulfur dioxide` + 
    `fixed acidity` +
    `free sulfur dioxide`,
  data = wine
)
summary (mlr3)

## Has an adjusted R-square of approximately 30 percent

mlr4 <- lm(
  quality ~ alcohol + 
    `volatile acidity` + 
    density + 
    sulphates + 
    chlorides_log +
    `citric acid` + 
    `total sulfur dioxide` + 
    `fixed acidity`,
  data = wine
)
summary (mlr4)

## Has an adjusted R-square of approximately 29 percent
####the best model is model three based on the adjusted rsquare


#---- MLR checking for assumptions ---- #

# Assumption one - Linearity (Y vs each X)
predictors_mlr <- c(
  "alcohol",
  "volatile acidity",
  "density",
  "sulphates",
  "chlorides",
  "citric acid",
  "total sulfur dioxide",
  "fixed acidity",
  "free sulfur dioxide"
)

# Linearity check grid
wine %>%
  pivot_longer(all_of(predictors_mlr), names_to="variable", values_to="x") %>%
  ggplot(aes(x, quality)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method="lm", color="red", se=FALSE) +
  facet_wrap(~ variable, scales="free") +
  theme_minimal() +
  labs(title="Linearity Check: Predictors vs Quality")

# Assumption two - Residual Independence (Residuals vs Fitted)
plot(mlr3, which = 1)  

# Assumption three: Normality of Residuals
plot(mlr3, which = 2)

residuals_mlr3 <- residuals(mlr3)

hist(
  residuals_mlr3,
  breaks = 30,
  main = "Histogram of Residuals",
  xlab = "Residuals"
)
# Assumption four: Homoscedasticity: Equal Residual Variance
plot(mlr3, which = 3)

# Assumption five: Multicollinearity (Variance Inflation Factor)
vif(mlr3)


# As a result of all the assumptions not being met, it might seem as tho simple and linear regression is not the best fit for out data. 
#Since quality is ordinal, I would now be trying ordinal regression


# ---------------- Ordinal Logistic regression -------------------- #
# to avoid errors due to column names
names(wine) <- make.names(names(wine))
library(MASS)

model_polr <- polr(as.factor(quality) ~ alcohol + volatile.acidity + 
                     sulphates + type,
                   data = wine,
                   method = "logistic")
summary(model_polr)

#Checking the proportional odds assumption
install.packages("brant")

library(brant)
brant(model_polr)


#---------------------------- Random Forest --------------------------------------- #

#selecting the predictors
library(dplyr)
rf_df <- wine %>%
  dplyr::select(
    quality,
    type,
    `fixed.acidity`, `volatile.acidity`, `citric.acid`,
    `residual.sugar`, chlorides,
    `free.sulfur.dioxide`, `total.sulfur.dioxide`,
    density, pH, sulphates, alcohol
  ) %>%
  dplyr::mutate(type = factor(type)) %>%
  na.omit()


#train, test, split
set.seed(123)
n <- nrow(rf_df)
train_id <- sample.int(n, size = floor(0.8 * n))

train <- rf_df[train_id, ]
test  <- rf_df[-train_id, ]

#standardize the numeric factors
num_cols <- names(train)[sapply(train, is.numeric)]
num_cols <- setdiff(num_cols, "quality")  # never scale outcome

# compute scaling params from TRAIN only
mu <- sapply(train[, num_cols], mean)
sdv <- sapply(train[, num_cols], sd)

scale_with <- function(df, cols, mu, sdv) {
  out <- df
  out[, cols] <- sweep(out[, cols, drop = FALSE], 2, mu, FUN = "-")
  out[, cols] <- sweep(out[, cols, drop = FALSE], 2, sdv, FUN = "/")
  out
}

train_sc <- scale_with(train, num_cols, mu, sdv)
test_sc  <- scale_with(test,  num_cols, mu, sdv)

#fit random forest
set.seed(123)
rf_fit <- randomForest(
  quality ~ .,
  data = train_sc,
  ntree = 500,
  importance = TRUE
)

rf_fit
#evaluate the results
pred <- predict(rf_fit, newdata = test_sc)

rmse <- sqrt(mean((test_sc$quality - pred)^2))
mae  <- mean(abs(test_sc$quality - pred))
r2   <- cor(test_sc$quality, pred)^2

c(RMSE = rmse, MAE = mae, R2 = r2)

#~----- hypothesis testing -----------------#


#Does wine type affect quality

#H₀: Wine quality distributions are the same for red and white wines

#H₁: Wine quality distributions differ between red and white wines

#using the wilcox test
wilcox.test(quality ~ type, data = wine)

#TEST 2: 

#H₀: Mean (alcohol) is the same for red and white wines

#H₁: Mean (alcohol) differs between red and white wines
#alcohol is not normally distributed

wine$log_alcohol <- log(wine$alcohol)

#visualising before and after transformations
par(mfrow = c(1, 2))

hist(wine$alcohol,
     main = "Alcohol (Original Scale)",
     xlab = "Alcohol",
     col = "lightblue",
     breaks = 30)

hist(wine$log_alcohol,
     main = "Alcohol (Log-Transformed)",
     xlab = "log(Alcohol)",
     col = "lightgreen",
     breaks = 30)

par(mfrow = c(1, 1))

# independent two sample test
t_test_log_alcohol <- t.test(
  log_alcohol ~ type,
  data = wine,
  var.equal = FALSE
)

t_test_log_alcohol



# test 4: Is wine quality category independent of wine type?

#using chi square test of independence
  wine$quality_cat <- cut(
  wine$quality,
  breaks = c(0, 5, 6, 10),
  labels = c("Low", "Medium", "High"),
  right = TRUE
)


quality_table <- table(wine$type, wine$quality_cat)
quality_table

chisq_quality <- chisq.test(quality_table)
chisq_quality

#checking the assumption
chisq_quality$expected







names(wine)



