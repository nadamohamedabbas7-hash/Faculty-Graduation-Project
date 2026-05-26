library(dplyr)
library(ggplot2)
# =====================================================
# Load Global Findex 2024 Microdata — Egypt
# =====================================================

data_2024 <- read.csv("Findex_Microdata_2025_Egypt, Arab Rep..csv")


# =====================================================
# Load Global Findex 2021 Microdata — Egypt
# =====================================================

data_2021 <- read.csv("micro_egy.csv")

# Drop the 5 level typo in education as it has three levels only 
data_2021 <- data_2021 %>%
  filter(educ %in% c(1,2,3))

# =====================================================
# Recode 2024 Binary Indicators (0/1 → No/Yes)
# =====================================================

binary_vars <- c("account", "account_fin", "account_mob",
                 "dig_account", "borrowed", "saved",
                 "merchantpay_dig", "anydigpayment")

data_2024[binary_vars] <- lapply(data_2024[binary_vars], function(x) {
  factor(x, levels = c(0,1), labels = c("No", "Yes"))
})


# =====================================================
# Recode 2024 Demographic Characteristics
# =====================================================

# Gender
data_2024$gender <- factor(data_2024$female,
                           levels = c(1,2),
                           labels = c("female", "Male"))

# Labor force participation
data_2024$emp_in <- factor(data_2024$emp_in,
                           levels = c(2,1),
                           labels = c("Out_of_workforce","In_workforce"))

# Area of residence
data_2024$urbanicity <- factor(data_2024$urbanicity,
                               levels = c(1,2),
                               labels = c("Rural", "Urban"))

# Educational attainment
data_2024$educ <- factor(data_2024$educ,
                         levels = c(1,2,3),
                         labels = c("Primary_or_less",
                                    "Secondary",
                                    "Tertiary_or_more"))

# Household income quintiles (ordered)
data_2024$inc_q <- factor(data_2024$inc_q,
                          levels = 1:5,
                          labels = c("Q1_Poorest","Q2","Q3","Q4","Q5_Richest"))

# =====================================================
# Recode 2024 Payment Variables
# =====================================================

payment_vars_2024 <- c("receive_wages", "receive_transfers",
                  "receive_pensions", "receive_agriculture")

data_2024[payment_vars_2024] <- lapply(data_2024[payment_vars_2024], function(x) {
  factor(x,
         levels = c(1,2,3,4,5),
         labels = c("Into_account",
                    "Cash_only",
                    "Other_method",
                    "Did_not_receive",
                    "Dont_know_or_refused"))
})

# Utility bill payment method
data_2024$pay_utilities <- factor(
  data_2024$pay_utilities,
  levels = c(1,2,3,4,5),
  labels = c("Into_account","Cash_only","Other_method",
             "Did_not_pay","Dont_know_or_refused"))

# Domestic remittance channels
data_2024$domestic_remittances <- factor(data_2024$domestic_remittances,
                                             levels = c(1,2,3,4),
                                             labels = c("Via_account",
                                                        "Other_method",
                                                        "No",
                                                        "Dont_know_or_refused"))

# Internet usage indicator
data_2024$internet_use <- factor(data_2024$internet_use,
                                     levels = c(0,1),
                                     labels = c("No", "Yes"))

# Example: create labelled factor variable
data_2024$mobileowner <- factor(
  data_2024$con1,
  levels = c(1, 2),
  labels = c(
    "Has mobile phone",
    "No mobile phone"
  )
)

# =====================================================
# Recode 2021 Binary Indicators
# =====================================================

binary_vars_2021 <- c("account","account_fin","account_mob",
                      "borrowed","saved",
                      "merchantpay_dig","anydigpayment")

data_2021[binary_vars_2021] <- lapply(data_2021[binary_vars_2021], function(x) {
  factor(x, levels = c(0,1), labels = c("No","Yes"))
})

# =====================================================
# Recode 2021 Demographic Characteristics
# =====================================================

data_2021$gender <- factor(data_2021$female,
                           levels = c(1,2),
                           labels = c("female","Male"))

data_2021$emp_in <- factor(data_2021$emp_in,
                           levels = c(2,1),
                           labels = c("Out_of_workforce","In_workforce"))

data_2021$urbanicity_f2f <- factor(data_2021$urbanicity_f2f,
                                   levels = c(1,2),
                                   labels = c("Rural","Urban"))

data_2021$educ <- factor(data_2021$educ,
                         levels = c(1,2,3),
                         labels = c("Primary_or_less",
                                    "Secondary",
                                    "Tertiary_or_more"))

data_2021$inc_q <- factor(data_2021$inc_q,
                          levels = 1:5,
                          labels = c("Q1_Poorest","Q2","Q3","Q4","Q5_Richest"))

# =====================================================
# Recode 2021 Payment Variables
# =====================================================

payment_vars_2021 <- c("receive_wages","receive_transfers",
                       "receive_pension","receive_agriculture")

data_2021[payment_vars_2021] <- lapply(data_2021[payment_vars_2021], function(x) {
  factor(x,
         levels = c(1,2,3,4,5),
         labels = c("Into_account","Cash_only","Other_method",
                    "Did_not_receive","Dont_know_or_refused"))
})

data_2021$pay_utilities <- factor(
  data_2021$pay_utilities,
  levels = c(1,2,3,4,5),
  labels = c("Into_account","Cash_only","Other_method",
             "Did_not_pay","Dont_know_or_refused"))

# Remittance receipt categories (more detailed in 2021)
data_2021$remittances <- factor(data_2021$remittances,
                                levels = c(1,2,3,4,5,6),
                                labels = c("Via_account",
                                           "MTO_service",
                                           "Cash_only",
                                           "Other_method",
                                           "No",
                                           "Dont_know_or_refused"))

# Mobile phone ownership
data_2021$mobileowner <- factor(data_2021$mobileowner,
                                levels = c(1,2,3,4),
                                labels = c("Yes","No","Dont_know","Refused"))

# Internet access availability
data_2021$internetaccess <- factor(data_2021$internetaccess,
                                   levels = c(1,2,3,4),
                                   labels = c("Yes","No","Dont_know","Refused"))


# =====================================================
# Generate Frequency Tables (Pre-Harmonization Check)
# =====================================================

# 2024 variables
vars_2024 <- c(binary_vars, "gender","emp_in","urbanicity",
               "educ","inc_q","internet_use",payment_vars_2024,
               "pay_utilities","domestic_remittances","mobileowner")

freq_tables_2024 <- lapply(vars_2024, \(v)
                           table(data_2024[[v]], useNA = "ifany")
)

# 2021 variables
vars_2021 <- c(binary_vars_2021, "gender","emp_in","urbanicity_f2f",
               "educ","inc_q", payment_vars_2021,
               "pay_utilities","remittances",
               "mobileowner","internetaccess")

freq_tables_2021 <- lapply(vars_2021, \(v)
                           table(data_2021[[v]], useNA = "ifany")
)

# view results
freq_tables_2024
freq_tables_2021

# =====================================================
# Harmonize Variables Across Survey Waves
# =====================================================

# Collapse remittance channels to match 2024 structure
data_2021$remittances <- dplyr::recode(
  data_2021$remittances,
  "Via_account" = "Via_account",
  "MTO_service" = "Other_method",
  "Cash_only" = "Other_method",
  "Other_method" = "Other_method",
  "No" = "No",
  "Dont_know_or_refused" = "Dont_know_or_refused"
)

# Recode internet responses: treat unknown/refused as No access
data_2021$internetaccess <- dplyr::recode(
  data_2021$internetaccess,
  "Yes"="Yes","No"="No","Dont_know"="No","Refused"="No")

# Rename variables to ensure consistent naming
colnames(data_2021)[colnames(data_2021) == "remittances"] <- "domestic_remittances"
colnames(data_2021)[colnames(data_2021) == "receive_pension"] <- "receive_pensions"
colnames(data_2021)[colnames(data_2021) == "urbanicity_f2f"] <- "urbanicity"
colnames(data_2024)[colnames(data_2024) == "internet_use"] <- "internetaccess"

# Treat unknown remittance responses as No
data_2024$domestic_remittances <- dplyr::recode(
  data_2024$domestic_remittances,
  "Dont_know_or_refused" = "No"
)

data_2021$domestic_remittances <- dplyr::recode(
  data_2021$domestic_remittances,
  "Dont_know_or_refused" = "No"
)

# Harmonize payment non-response categories
payment_vars <- c("receive_wages","receive_transfers",
                  "receive_pensions","receive_agriculture")

payment_levels_receive <- c("Into_account", "Cash_only", "Did_not_receive")
payment_levels_pay <- c("Into_account", "Cash_only", "Did_not_pay")

# Recode using character labels to avoid factor-level mismatches -> NA
data_2021[payment_vars] <- lapply(data_2021[payment_vars], function(x){
  x_chr <- as.character(x)
  x_chr <- dplyr::recode(
    x_chr,
    "Dont_know_or_refused" = "Did_not_receive",
    "Other_method" = "Did_not_receive"
  )
  factor(x_chr, levels = payment_levels_receive, ordered = TRUE)
})

data_2024[payment_vars] <- lapply(data_2024[payment_vars], function(x){
  x_chr <- as.character(x)
  x_chr <- dplyr::recode(
    x_chr,
    "Dont_know_or_refused" = "Did_not_receive",
    "Other_method" = "Did_not_receive"
  )
  factor(x_chr, levels = payment_levels_receive, ordered = TRUE)
})

# Pay utilities: into_account/cash_only vs did_not_pay (merge other+dontknow into did_not_pay)
data_2024$pay_utilities <- {
  x_chr <- as.character(data_2024$pay_utilities)
  x_chr <- dplyr::recode(
    x_chr,
    "Dont_know_or_refused" = "Did_not_pay",
    "Other_method" = "Did_not_pay"
  )
  factor(x_chr, levels = payment_levels_pay, ordered = TRUE)
}

data_2021$pay_utilities <- {
  x_chr <- as.character(data_2021$pay_utilities)
  x_chr <- dplyr::recode(
    x_chr,
    "Dont_know_or_refused" = "Did_not_pay",
    "Other_method" = "Did_not_pay"
  )
  factor(x_chr, levels = payment_levels_pay, ordered = TRUE)
}

data_2021$mobileowner <- dplyr::recode(data_2021$mobileowner, "Dont_know"="No","Refused"="No")

# =====================================================
# 1. Creating financial resilience (fin_resilience)
# =====================================================

data_2021$fin_resilience <- ifelse( data_2021$fin24a %in% 2:3,
                                    1,
                                    0
)
data_2024$fin_resilience <- ifelse( data_2024$fin24a %in%  2:3,
                                    1,
                                    0
)
data_2021$fin_resilience <- factor(
  data_2021$fin_resilience,
  levels = c(0, 1),
  labels = c("not_resilient", "resilient")
)
data_2024$fin_resilience <- factor(
  data_2024$fin_resilience,
  levels = c(0, 1),
  labels = c("not_resilient", "resilient")
)
table(data_2021$fin_resilience)
prop.table(table(data_2021$fin_resilience))
table(data_2024$fin_resilience)
prop.table(table(data_2024$fin_resilience))
# =========================
# IN-STORE SHIFT
# =========================

# ---- 1. PRE-COVID (Cash vs Non-cash) ----
data_2021$pre_cash <- ifelse(data_2021$fin14_2 == 1, 1,
                             ifelse(data_2021$fin14_2 == 2, 0, NA))
table(data_2021$pre_cash)
# ---- 2. POST-COVID (Any digital payment) ----
data_2021$post_digital <- ifelse(
  data_2021$fin14_1 == 1 |
    data_2021$fin4a == 1 |
    data_2021$fin8a == 1,
  1, 0
)
table(data_2021$fin14_1)
table(data_2021$post_digital)
# ---- 3. SHIFT VARIABLE (ONLY VALID TRANSITIONS) ----
data_2021$payment_shift <- NA

data_2021$payment_shift[data_2021$pre_cash == 1 & data_2021$post_digital == 1] <- 1  # shifted to digital
data_2021$payment_shift[data_2021$pre_cash == 0 & !is.na(data_2021$pre_cash)] <- 2  # already digital

# ---- 4. REMOVE NO-CHANGE CATEGORY (cash stays undefined) ----
# (intentionally excluded because pre-COVID status is not identifiable for them)

# ---- 5. LABELS ----
data_2021$payment_shift <- factor(
  data_2021$payment_shift,
  levels = c(1, 2),
  labels = c("Shifted to digital",
             "Already digital")
)

# ---- 6. FREQUENCY TABLE ----
table(data_2021$payment_shift)

# ---- 7. PROPORTIONS (%) ----
prop.table(table(data_2021$payment_shift)) * 100

# ---- 8. SUMMARY TABLE ----
results <- as.data.frame(table(data_2021$payment_shift))
colnames(results) <- c("Category", "Frequency")
results$Percentage <- round(results$Frequency / sum(results$Frequency) * 100, 2)

results
# =========================
# ONLINE PAYMENT SHIFT (DELIVERY / E-COMMERCE)
# =========================

# ---- 1. DEFINE ELIGIBLE USERS (E-COMMERCE USERS) ----
online_users <- data_2021$fin14b == 1
table(data_2021$fin14b)
# ---- 2. PRE-COVID (Cash vs Online at delivery) ----
data_2021$pre_cod_cash <- NA
data_2021$pre_cod_cash[data_2021$fin14c_2 == 1] <- 1  # only cash
data_2021$pre_cod_cash[data_2021$fin14c_2 == 2] <- 0  # also paid online

# ---- 3. POST-COVID (Payment method at delivery) ----
data_2021$post_online <- NA
data_2021$post_online[data_2021$fin14c == 1 | data_2021$fin14c == 3] <- 1  # online or both
data_2021$post_online[data_2021$fin14c == 2] <- 0  # cash on delivery

# ---- 4. SHIFT VARIABLE ----
data_2021$online_shift <- NA

# shifted to online payment
data_2021$online_shift[online_users & data_2021$pre_cod_cash == 1 & data_2021$post_online == 1] <- 1

# stayed cash on delivery
data_2021$online_shift[online_users & data_2021$pre_cod_cash == 1 & data_2021$post_online == 0] <- 0

# already digital before COVID
data_2021$online_shift[online_users & data_2021$pre_cod_cash == 0] <- 2

# ---- 5. LABELS ----
data_2021$online_shift <- factor(
  data_2021$online_shift,
  levels = c(0, 1, 2),
  labels = c("Stayed cash on delivery",
             "Shifted to online payment",
             "Already digital")
)

# ---- 6. FREQUENCY TABLE ----
table(data_2021$online_shift)

# ---- 7. PROPORTIONS (%) ----
prop.table(table(data_2021$online_shift)) * 100

# ---- 8. SUMMARY TABLE ----
results_online <- as.data.frame(table(data_2021$online_shift))
colnames(results_online) <- c("Category", "Frequency")
results_online$Percentage <- round(results_online$Frequency / sum(results_online$Frequency) * 100, 2)

results_online
# =====================================================
# UNIVARIATE DESCRIPTIVES - 2021
# =====================================================

# 1. Select constructed variables (excluding raw financial variables)
data_2021_constructed <- data_2021 %>%
  dplyr::select(
    # Demographic variables
    gender,
    educ,
    inc_q,
    urbanicity,
    emp_in,
    
    # Core financial indicators (constructed)
    account,
    account_fin,
    account_mob,
    saved,
    borrowed,
    internetaccess,
    mobileowner,
    domestic_remittances,
    anydigpayment,
    merchantpay_dig,
    
    # Payment variables (harmonized levels)
    receive_wages,
    receive_transfers,
    receive_pensions,
    receive_agriculture,
    pay_utilities
  )

# 2. Generate frequency tables for all variables
freq_tables_2021_final <- lapply(
  data_2021_constructed,
  table
)

# 3. Display results
freq_tables_2021_final

# =====================================================
# UNIVARIATE DESCRIPTIVES - 2024
# =====================================================

# 1. Select constructed variables
data_2024_constructed <- data_2024 %>%
  dplyr::select(
    # Demographic variables
    gender,
    educ,
    inc_q,
    urbanicity,
    emp_in,
    
    # Core financial indicators (constructed)
    account,
    account_fin,
    account_mob,
    dig_account,
    saved,
    borrowed,
    internetaccess,
    domestic_remittances,
    anydigpayment,
    merchantpay_dig,
    mobileowner,
    
    # Payment variables (harmonized levels)
    receive_wages,
    receive_transfers,
    receive_pensions,
    receive_agriculture,
    pay_utilities
  )

# 2. Generate frequency tables for all variables
freq_tables_2024_final <- lapply(
  data_2024_constructed,
  table
)

# 3. Display results
freq_tables_2024_final

# =====================================================
# BIVARIATE DESCRIPTIVES - 2021
# =====================================================

# 1. Define variables to analyze
vars_to_analyze <- c(
  "gender",
  "educ",
  "inc_q",
  "urbanicity",
  "emp_in",
  "internetaccess",
  "fin_resilience",
  "saved",
  "borrowed"
)

# 2. Generate bivariate frequency tables (2021 vs account in 2021)
bivariate_tables_2021 <- lapply(
  data_2021[vars_to_analyze],
  function(x) table(x, data_2021$account)
)
pct_tables_2021 <- lapply(
  data_2021[vars_to_analyze],
  function(x) {
    round(prop.table(table(x, data_2021$account), 1) * 100, 1)
  }
)
# 3. Display results
bivariate_tables_2021
pct_tables_2021

# 4. Internet access rate by urban/rural (correct direction)
prop.table(table(data_2021$urbanicity, data_2021$internetaccess), 1) * 100

# =====================================================
# BIVARIATE DESCRIPTIVES - 2024
# =====================================================

# 1. Define variables to analyze
vars_to_table <- c(
  "gender",
  "educ",
  "inc_q",
  "urbanicity",
  "emp_in",
  "internetaccess",
  "fin_resilience",
  "saved",
  "borrowed"
)

# 2. Generate bivariate frequency tables
bivariate_tables_2024 <- lapply(
  data_2024[vars_to_table],
  function(x) table(x, data_2024$account)
)
pct_tables_2024 <- lapply(
  data_2024[vars_to_table],
  function(x) {
    round(prop.table(table(x, data_2024$account), 1) * 100, 1)
  }
)

# 3. Display results
bivariate_tables_2024
pct_tables_2024

# 4. Internet access rate by urban/rural (correct direction)
prop.table(table(data_2024$urbanicity, data_2024$internetaccess), 1) * 100

# =====================================================
# 1. BIND DATASETS (ADD YEAR)
# =====================================================

data_all <- bind_rows(
  data_2021 %>% mutate(year = 2021),
  data_2024 %>% mutate(year = 2024)
)

# Convert year to factor
data_all$year <- factor(data_all$year)


# =====================================================
# 2. BIVARIATE DESCRIPTIVE STATISTICS (ON BOUND DATA)
# =====================================================

# Variables to analyze
vars_to_table <- c(
  "gender",
  "educ",
  "inc_q",
  "urbanicity",
  "emp_in",
  "internetaccess"
)

# Bivariate frequency tables with account
bivariate_tables_all <- lapply(
  data_all[vars_to_table],
  function(x) table(x, data_all$account)
)

# Display frequency tables
bivariate_tables_all


# =====================================================
# 3. PREPARE DATA FOR REGRESSION (FROM BOUND DATA)
# =====================================================

# Create binary variables directly on data_all
data_all <- data_all %>%
  mutate(
    inc_q_num = ifelse(inc_q %in% c("Q4", "Q5_Richest"), 1, 0),
    inc_q_bin = factor(inc_q_num, levels = c(0, 1), labels = c("poor", "rich")),
    
    educ_num = ifelse(educ %in% c("Tertiary_or_more"), 1, 0),
    educ_bin = factor(educ_num, levels = c(0, 1), labels = c("Secondary_or_less", "Tertiary_or_more")),
    
    account_num = ifelse(account == "Yes", 1, 0)
  )
edu_plot <- data_all %>%
  group_by(educ) %>%
  summarise(
    prob = mean(account == "Yes"),
    n = n()
  )
ggplot(edu_plot, aes(x = educ, y = prob, group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  ylim(0, 1) +
  labs(
    title = "Probability of Account Ownership by Education",
    x = "Education Level",
    y = "Probability of Having an Account"
  ) +
  theme_minimal()

table(data_all$educ_bin)
# =====================================================
# DETERMINANTS OF FINANCIAL INCLUSION
# FULL ANALYSIS: BASE + INTERACTIONS + REVERSED MODEL
# =====================================================

library(survey)
library(pROC)
library(splines)
library(car)

# =====================================================
# COMMON SURVEY DESIGN
# =====================================================

design_all <- svydesign(
  ids = ~1,
  weights = ~wgt,
  data = data_all
)


# =====================================================
# =====================================================
# 1. BASE MODEL (NO INTERACTIONS)
# =====================================================
# =====================================================


# LINEARITY CHECK

boxTidwell(account_num ~ age, data = data_all)

# MODEL
model_base <- svyglm(
  account ~ inc_q_bin +
    year +
    age +
    internetaccess +
    gender +
    educ_bin +
    emp_in +
    urbanicity,
  design = design_all,
  family = quasibinomial()
)

summary(model_base)

#Mullticollinearity
vif(model_base)

# PREDICTIONS
data_all$prob_base <- predict(model_base, type = "response")
data_all$y_account <- data_all$account_num


# ROC + AUC
roc_base <- roc(
  response = data_all$y_account,
  predictor = data_all$prob_base,
  weights = data_all$wgt,
  direction = "<"
)

auc_base <- auc(roc_base)
print(auc_base)


# YOUden cutoff
cut_base <- coords(roc_base, "best", best.method = "youden")
th_base <- cut_base$threshold
th_base

# CLASSIFICATION
data_all$pred_base <- ifelse(data_all$prob_base >= th_base, 1, 0)


# ACCURACY
acc_base <- with(data_all,
                 sum(wgt * (pred_base == y_account), na.rm = TRUE) /
                   sum(wgt, na.rm = TRUE))
print(acc_base)


# SENS + SPEC
conf_base <- table(data_all$y_account, data_all$pred_base)

sens_base <- conf_base[2,2] / sum(conf_base[2, ])
spec_base <- conf_base[1,1] / sum(conf_base[1, ])

print(conf_base)
print(sens_base)
print(spec_base)


# ROC PLOT
plot(
  roc_base,
  col = "blue",
  main = paste0("Base Model ROC | AUC = ", round(auc_base, 3))
)

# Odds Ratios (Base Model)
or_base <- data.frame(
  Variable = names(coef(model_base)),
  OR = exp(coef(model_base)),
  CI_lower = exp(confint(model_base)[,1]),
  CI_upper = exp(confint(model_base)[,2]),
  p_value = summary(model_base)$coefficients[,4]
)

print(or_base)
library(car)

# =====================================================
# =====================================================
# 2. INTERACTION MODEL
# =====================================================
# =====================================================


model_int <- svyglm(
  account ~ inc_q_bin * year +
    gender * emp_in +
    age +
    internetaccess +
    educ_bin +
    urbanicity,
  design = design_all,
  family = quasibinomial()
)

summary(model_int)


# PREDICTIONS
data_all$prob_int <- predict(model_int, type = "response")


# ROC + AUC
roc_int <- roc(
  response = data_all$y_account,
  predictor = data_all$prob_int,
  weights = data_all$wgt,
  direction = "<"
)

auc_int <- auc(roc_int)
print(auc_int)


# YOUden cutoff
cut_int <- coords(roc_int, "best", best.method = "youden")
th_int <- cut_int$threshold
cut_int
th_int

# CLASSIFICATION
data_all$pred_int <- ifelse(data_all$prob_int >= th_int, 1, 0)


# ACCURACY
acc_int <- with(data_all,
                sum(wgt * (pred_int == y_account), na.rm = TRUE) /
                  sum(wgt, na.rm = TRUE))
print(acc_int)


# SENS + SPEC
conf_int <- table(data_all$y_account, data_all$pred_int)
sens_int <- conf_int[2,2] / sum(conf_int[2, ])
spec_int <- conf_int[1,1] / sum(conf_int[1, ])

print(conf_int)
print(sens_int)
print(spec_int)


# ROC PLOT
plot(
  roc_int,
  col = "red",
  main = paste0("Interaction Model ROC | AUC = ", round(auc_int, 3))
)

# Odds Ratios (Interaction Model)
or_int <- data.frame(
  Variable = names(coef(model_int)),
  OR = exp(coef(model_int)),
  CI_lower = exp(confint(model_int)[,1]),
  CI_upper = exp(confint(model_int)[,2]),
  p_value = summary(model_int)$coefficients[,4]
)

print(or_int)

# =====================================================
# =====================================================
# 3. REVERSED MODEL (INCOME AS OUTCOME)
# =====================================================
# =====================================================


# LINEARITY CHECK
boxTidwell(inc_q_num ~ age, data = data_all)

# MODEL
model_rev <- svyglm(
  inc_q_bin ~ account +
    year +
    ns(age, 3) +
    internetaccess +
    gender +
    educ +
    emp_in +
    urbanicity,
  design = design_all,
  family = quasibinomial()
)

summary(model_rev)

#Mullticollinearity
vif(model_rev)

# PREDICTIONS
data_all$prob_rev <- predict(model_rev, type = "response")
data_all$y_income <- data_all$inc_q_num


# ROC + AUC
roc_rev <- roc(
  response = data_all$y_income,
  predictor = data_all$prob_rev,
  weights = data_all$wgt,
  direction = "<"
)

auc_rev <- auc(roc_rev)
print(auc_rev)


# YOUden cutoff
cut_rev <- coords(roc_rev, "best", best.method = "youden")
th_rev <- cut_rev$threshold
th_rev

# CLASSIFICATION
data_all$pred_rev <- ifelse(data_all$prob_rev >= th_rev, 1, 0)


# ACCURACY
acc_rev <- with(data_all,
                sum(wgt * (pred_rev == y_income), na.rm = TRUE) /
                  sum(wgt, na.rm = TRUE))
print(acc_rev)


# SENS + SPEC
conf_rev <- table(data_all$y_income, data_all$pred_rev)
sens_rev <- conf_rev[2,2] / sum(conf_rev[2, ])
spec_rev <- conf_rev[1,1] / sum(conf_rev[1, ])
print(conf_rev)
print(sens_rev)
print(spec_rev)

# ROC PLOT
plot(
  roc_rev,
  col = "darkgreen",
  main = paste0("Reversed Model ROC | AUC = ", round(auc_rev, 3))
)

# Odds Ratios (Reversed Model)
or_rev <- data.frame(
  Variable = names(coef(model_rev)),
  OR = exp(coef(model_rev)),
  CI_lower = exp(confint(model_rev)[,1]),
  CI_upper = exp(confint(model_rev)[,2]),
  p_value = summary(model_rev)$coefficients[,4]
)

print(or_rev)

# =====================================================
# STRUCTURAL EQUATION MODEL (SEM)
# Financial Inclusion Pathways to Financial Resilience
# Using Numeric (0/1) Indicators Only
# =====================================================

library(lavaan)
library(dplyr)
# =====================================================
# VARIABLE RECODING (NUMERIC INDICATORS ONLY)
# =====================================================

data_all <- data_all %>%
  mutate(
    inc_q_num = ifelse(inc_q %in% c("Q4", "Q5_Richest"), 1, 0),
    educ_num = ifelse(educ %in% c("Tertiary_or_more"), 1, 0),
    account_num = ifelse(account == "Yes", 1, 0),
    saved_num = ifelse(saved == "Yes", 1, 0),
    internet_num = ifelse(internetaccess == "Yes", 1, 0),
    gender_num = ifelse(gender == "Male", 1, 0),
    urban_num = ifelse(urbanicity == "Urban", 1, 0),
    emp_num = ifelse(emp_in == "In_workforce", 1, 0),
    resilience_num = ifelse(fin_resilience == "resilient", 1, 0),
  )

# =====================================================
# STRUCTURAL EQUATION MODEL
# =====================================================

model_sem <- '
account_num ~ inc_q_num + age + internet_num + educ_num + year + gender_num + emp_num + urban_num
saved_num ~ b1*account_num + inc_q_num + internet_num + emp_num + educ_num
resilience_num ~ d1*saved_num + account_num + inc_q_num + internet_num + educ_num
indirect := b1*d1
'


# =====================================================
# MODEL ESTIMATION
# =====================================================

fit_sem <- sem(
  model_sem,
  data = data_all,
  estimator = "MLR"
)


# =====================================================
# RESULTS (WITH INTERCEPTS SHOWN)
# =====================================================

summary(
  fit_sem,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)

modindices(fit_sem)


###limitations and checking 
library(MASS)

model_ord <- polr(
  inc_q ~ account + age + internetaccess + gender + educ + emp_in + urbanicity,
  data = data_all,
  method = "logistic",
  Hess = TRUE
)
library(brant)

# =========================
# 1. UNBANKED SUBSETS
# =========================

data_2021_unbanked <- subset(data_2021, account_fin == "No")
data_2024_unbanked <- subset(data_2024, account_fin == "No")

# -------------------------
# 2021 BARRIERS
# -------------------------
data_2021_barriers <- data_2021_unbanked %>%
  mutate(
    year = 2021,
    distance = ifelse(fin11a == 1, 1, 0),
    cost     = ifelse(fin11b == 1, 1, 0),
    docs     = ifelse(fin11c == 1, 1, 0),
    trust    = ifelse(fin11d == 1, 1, 0),
    money   = ifelse(fin11f == 1, 1, 0),
    family    = ifelse(fin11g == 1, 1, 0)
  ) %>%
  dplyr::select(year, distance, cost, docs, money, family, trust)


# -------------------------
# 2024 BARRIERS (NO religion!)
# -------------------------
data_2024_barriers <- data_2024_unbanked %>%
  mutate(
    year = 2024,
    distance = ifelse(fin11a == 1, 1, 0),
    cost     = ifelse(fin11b == 1, 1, 0),
    docs     = ifelse(fin11c == 1, 1, 0),
    money    = ifelse(fin11d == 1, 1, 0),
    family   = ifelse(fin11e == 1, 1, 0),
    trust    = ifelse(fin11f == 1, 1, 0)
  ) %>%
  dplyr::select(year, distance, cost, docs, money, family, trust)


# -------------------------
# COMBINE DATASETS
# -------------------------
barriers_all <- bind_rows(data_2021_barriers, data_2024_barriers)
dim(barriers_all)

library(dplyr)

barriers_all %>%
  group_by(year) %>%
  summarise(
    distance = mean(distance, na.rm = TRUE) * 100,
    cost     = mean(cost, na.rm = TRUE) * 100,
    docs     = mean(docs, na.rm = TRUE) * 100,
    money    = mean(money, na.rm = TRUE) * 100,
    family   = mean(family, na.rm = TRUE) * 100,
    trust    = mean(trust, na.rm = TRUE) * 100
  )
# =========================
# 5. PLOT COMPARISON
# =========================
library(tidyverse)
ggplot(barriers_long, aes(x = reorder(Barrier, Share), y = Share, fill = factor(year))) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  labs(
    title = "Financial Exclusion Barriers in Egypt (2021 vs 2024)",
    x = NULL,
    y = "Share of Unbanked Adults",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )
library(ggplot2)

ggplot(data_all, aes(x = factor(year), y = age, fill = factor(year))) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Age Distribution Comparison (2021 vs 2024)",
    x = "Year",
    y = "Age",
    fill = "Year"
  ) +
  theme_minimal()

# =========================
# PACKAGES
# =========================
library(klaR)
library(dplyr)
library(cluster)
library(FactoMineR)
library(factoextra)

# =========================
# VARIABLES
# =========================
vars <- c(
  "account_fin",
  "anydigpayment",
  "internetaccess",
  "educ",
  "emp_in"
)
# =========================
# DATA SELECTION
# =========================
data_2021_sel <- data_2021 %>% dplyr::select(all_of(vars))
data_2024_sel <- data_2024 %>% dplyr::select(all_of(vars))

# =========================
# ELBOW FUNCTION (K-MODES)
# =========================
set.seed(123)

wss_2021 <- sapply(1:6, function(k){
  km <- kmodes(data_2021_sel, modes = k)
  sum(km$withindiff)
})

plot(1:6, wss_2021, type="b", pch=19,
     col="blue",
     xlab="Number of Clusters (k)",
     ylab="Within-cluster Dissimilarity",
     main="Elbow Plot - 2021")


wss_2024 <- sapply(1:6, function(k){
  km <- kmodes(data_2024_sel, modes = k)
  sum(km$withindiff)
})

plot(1:6, wss_2024, type="b", pch=19,
     col="darkgreen",
     xlab="Number of Clusters (k)",
     ylab="Within-cluster Dissimilarity",
     main="Elbow Plot - 2024")

# =========================
# DENDROGRAM (GOWER + HCLUST)
# =========================
dist_2021 <- daisy(data_2021_sel, metric="gower")
hc_2021 <- hclust(dist_2021, method="ward.D2")
plot(hc_2021, labels=FALSE, main="Dendrogram - 2021")
rect.hclust(hc_2021, k=3, border="red")

dist_2024 <- daisy(data_2024_sel, metric="gower")
hc_2024 <- hclust(dist_2024, method="ward.D2")
plot(hc_2024, labels=FALSE, main="Dendrogram - 2024")
rect.hclust(hc_2024, k=3, border="red")

# =========================
# K-MODES MODEL (FINAL)
# =========================
set.seed(123)

km21 <- kmodes(data_2021_sel, modes = 3)
km24 <- kmodes(data_2024_sel, modes = 3)

# =========================
# BASIC OUTPUTS
# =========================
# Cluster sizes
km21$size
km24$size

# Cluster assignments
table(km21$cluster)
table(km24$cluster)

# Within-cluster dissimilarity
sum(km21$withindiff)
sum(km24$withindiff)

# Cluster modes (MOST IMPORTANT INTERPRETATION PART)
km21$modes
km24$modes

# =====================================================
# 1. LOAD DATA ASSUMED ALREADY DONE
# data_2021
# data_2024
# km21, km24 (kmodes outputs)
# =====================================================

# =====================================================
# 2. ADD CLUSTERS TO DATA
# =====================================================

data_2021$cluster <- km21$cluster
data_2024$cluster <- km24$cluster

# =====================================================
# 3. CLUSTER LABELS (HARMONIZED INTERPRETATION)
# =====================================================

data_2021$cluster_name <- factor(data_2021$cluster,
                                 levels = c(1,2,3),
                                 labels = c(
                                   "Structurally_Excluded",
                                   "Digitally_Connected_Excluded",
                                   "Fully_Included"
                                 ))

data_2024$cluster_name <- factor(data_2024$cluster,
                                 levels = c(1,2,3),
                                 labels = c(
                                   "Fully_Included",
                                   "Structurally_Excluded",
                                   "Digitally_Connected_Excluded"
                                 ))

# =====================================================
# 4. CLUSTER PROFILING (2021)
# =====================================================

profile_2021 <- data_2021 %>%
  group_by(cluster_name) %>%
  summarise(
    year = 2021,
    N = n(),
    
    # Demographics
    female_rate = mean(gender == "female", na.rm = TRUE) * 100,
    urban_rate  = mean(urbanicity == "Urban", na.rm = TRUE) * 100,
    employed_rate = mean(emp_in == "In_workforce", na.rm = TRUE) * 100,
    
    # Digital inclusion
    internet_rate = mean(internetaccess == "Yes", na.rm = TRUE) * 100,
    account_rate  = mean(account_fin == "Yes", na.rm = TRUE) * 100,
    digital_rate  = mean(anydigpayment == "Yes", na.rm = TRUE) * 100,
    
    # Financial resilience
    fin_resilience_rate = mean(fin_resilience == "resilient", na.rm = TRUE) * 100,
    
    # Education
    primary_rate   = mean(educ == "Primary_or_less", na.rm = TRUE) * 100,
    secondary_rate = mean(educ == "Secondary", na.rm = TRUE) * 100,
    tertiary_rate  = mean(educ == "Tertiary_or_more", na.rm = TRUE) * 100,
    
    # Income
    rich_rate   = mean(inc_q == "Q5_Richest", na.rm = TRUE) * 100,
    middle_rate = mean(inc_q %in% c("Q2","Q3","Q4"), na.rm = TRUE) * 100,
    poor_rate   = mean(inc_q == "Q1_Poorest", na.rm = TRUE) * 100,
    
    .groups = "drop"
  )

# =====================================================
# 5. CLUSTER PROFILING (2024)
# =====================================================

profile_2024 <- data_2024 %>%
  group_by(cluster_name) %>%
  summarise(
    year = 2024,
    N = n(),
    
    # Demographics
    female_rate = mean(gender == "female", na.rm = TRUE) * 100,
    urban_rate  = mean(urbanicity == "Urban", na.rm = TRUE) * 100,
    employed_rate = mean(emp_in == "In_workforce", na.rm = TRUE) * 100,
    
    # Digital inclusion
    internet_rate = mean(internetaccess == "Yes", na.rm = TRUE) * 100,
    account_rate  = mean(account_fin == "Yes", na.rm = TRUE) * 100,
    digital_rate  = mean(anydigpayment == "Yes", na.rm = TRUE) * 100,
    
    # Financial resilience
    fin_resilience_rate = mean(fin_resilience == "resilient", na.rm = TRUE) * 100,
    
    # Education
    primary_rate   = mean(educ == "Primary_or_less", na.rm = TRUE) * 100,
    secondary_rate = mean(educ == "Secondary", na.rm = TRUE) * 100,
    tertiary_rate  = mean(educ == "Tertiary_or_more", na.rm = TRUE) * 100,
    
    # Income
    rich_rate   = mean(inc_q == "Q5_Richest", na.rm = TRUE) * 100,
    middle_rate = mean(inc_q %in% c("Q2","Q3","Q4"), na.rm = TRUE) * 100,
    poor_rate   = mean(inc_q == "Q1_Poorest", na.rm = TRUE) * 100,
    
    .groups = "drop"
  )

# =====================================================
# 6. FINAL TABLE (FOR PAPER)
# =====================================================

final_cluster_table <- bind_rows(profile_2021, profile_2024)

# show full table
print(final_cluster_table, width = Inf)

prop.table(table(data_2024$con1,data_2024$anydigpayment))
table(data_2024)
table(data_all$borrowed,data_all$account)
