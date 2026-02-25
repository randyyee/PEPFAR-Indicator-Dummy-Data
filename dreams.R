library(tidyverse)
library(lubridate)

set.seed(42)
n <- 200

# --- Reference Data ---
subdistricts <- c("Manzini Central", "Nhlangano", "Siteki", "Pigg's Peak", "Lobamba")
districts     <- c("Manzini", "Shiselweni", "Lubombo", "Hhohho", "Hhohho")
sub_dist_map  <- setNames(districts, subdistricts)

# --- Generate Base Patient Data ---
patient_uid    <- paste0("PT-", str_pad(1:n, 4, pad = "0"))
gender         <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.45, 0.55))
subdistrict    <- sample(subdistricts, n, replace = TRUE)
district       <- sub_dist_map[subdistrict]
in_school      <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.6, 0.4))

# DOB: ages 15-24 as of today
today          <- Sys.Date()
dob_clean      <- today - days(sample(15 * 365:(24 * 365), n, replace = TRUE))
enrollment_date <- dob_clean + days(sample(15 * 365:(24 * 365 - 1), n, replace = TRUE))
enrollment_date <- pmin(enrollment_date, today - days(30))  # keep most in the past

today         <- Sys.Date()
max_tests     <- 5
prob_positive <- 0.25

# Build one patient's test history
generate_tests <- function(patient_uid, enrollment_date) {
  
  n_tests    <- sample(1:max_tests, 1)
  test_dates <- sort(enrollment_date + days(cumsum(sample(30:120, n_tests, replace = TRUE))))
  test_dates <- test_dates[test_dates <= today]
  n_tests    <- length(test_dates)
  
  if (n_tests == 0) return(NULL)
  
  statuses <- character(n_tests)
  for (i in seq_len(n_tests)) {
    if (runif(1) < prob_positive) {
      statuses[i] <- "Positive"
      test_dates  <- test_dates[1:i]
      statuses    <- statuses[1:i]
      break
    } else {
      statuses[i] <- "Negative"
    }
  }
  
  tibble(patient_uid = patient_uid,
         test_date   = test_dates,
         hiv_status  = statuses)
}

# Use df (before DQ issues) as the base
patient_base <- df %>%
  distinct(patient_uid, .keep_all = TRUE) %>%
  mutate(enrollment_date = as.Date(enrollment_date)) %>%
  filter(!is.na(enrollment_date))

test_results <- map2(
  patient_base$patient_uid,
  patient_base$enrollment_date,
  generate_tests
) %>%
  compact() %>%
  bind_rows()

# --- Sense checks ---
cat("Test results rows:     ", nrow(test_results), "\n")
cat("Patients with tests:   ", n_distinct(test_results$patient_uid), "\n")
cat("HIV positive patients: ", sum(test_results$hiv_status == "Positive"), "\n")

test_results %>%
  group_by(patient_uid) %>%
  mutate(lag_positive = lag(cumany(hiv_status == "Positive"), default = FALSE)) %>%
  filter(lag_positive) %>%
  nrow() %>%
  { cat("Tests after a positive (should be 0):", ., "\n") }

df <- tibble(
  patient_uid,
  date_of_birth   = dob_clean,
  gender,
  subdistrict,
  district,
  in_school,
  enrollment_date,
  test_date,
  hiv_status
)

# ============================================================
# INJECT DATA QUALITY ISSUES
# ============================================================

# --- 1. Dates in the future ---
future_idx <- sample(setdiff(1:n, dmy_idx), 10)
df$test_date[future_idx]       <- as.character(today + days(sample(30:365, 10)))
df$enrollment_date[future_idx] <- as.character(today + days(sample(10:60,  10)))

# --- 2. Duplicate patients ---
dup_idx <- sample(1:n, 8)
df <- bind_rows(df, df[dup_idx, ])   # exact duplicates appended

# --- 3. Test / enrollment dates BEFORE date of birth ---
pre_dob_idx <- sample(1:n, 8)
df$enrollment_date[pre_dob_idx] <- as.character(
  as.Date(df$date_of_birth[pre_dob_idx]) - days(sample(100:1000, 8))
)
df$test_date[pre_dob_idx] <- as.character(
  as.Date(df$date_of_birth[pre_dob_idx]) - days(sample(10:90, 8))
)

# --- 4. Missingness ---
for (col in c("gender", "subdistrict", "district", "in_school", "hiv_status", "test_date")) {
  miss_idx <- sample(1:nrow(df), size = round(nrow(df) * 0.04))
  df[[col]][miss_idx] <- NA
}

# --- Shuffle rows ---
df <- df[sample(1:nrow(df)), ] %>% 
  mutate(across(c(enrollment_date, test_date), as.character))

# --- Split into two tables ---

# Table 1: HTS
test_results <- df %>%
  select(patient_uid, test_date, hiv_status)

# Table 2: Patient demographics
patient_demographics <- df %>%
  select(patient_uid, date_of_birth, gender, subdistrict, district, in_school, enrollment_date)

# --- Add duplicate patient errors to demographics table ---
set.seed(7)

dup_demo_idx  <- sample(1:nrow(patient_demographics), 12)
patient_demographics <- bind_rows(patient_demographics, patient_demographics[dup_demo_idx, ])

# Shuffle so duplicates aren't obviously at the bottom
patient_demographics <- patient_demographics[sample(1:nrow(patient_demographics)), ]
