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

#  --- Testing ---
max_tests      <- 5      # maximum number of tests a patient could have
prob_positive  <- 0.25   # probability of testing positive on any given test

# Start from the cleaned demographics so dates are consistent
test_results <- patient_demographics %>%
  distinct(patient_uid, .keep_all = TRUE) %>%        # one row per patient
  mutate(date_of_birth   = as.Date(date_of_birth),
         enrollment_date = as.Date(enrollment_date)) %>%
  filter(!is.na(enrollment_date)) %>%
  rowwise() %>%
  reframe({
    
    # Randomly assign how many tests this patient will have
    n_tests <- sample(1:max_tests, 1)
    
    # Generate test dates spaced at least 30 days apart from enrollment
    test_dates <- sort(enrollment_date + days(cumsum(sample(30:120, n_tests, replace = TRUE))))
    
    # Keep only test dates that are not in the future
    test_dates <- test_dates[test_dates <= today]
    n_tests    <- length(test_dates)
    
    # If no valid test dates remain, return empty
    if (n_tests == 0) return(tibble(patient_uid = character(), test_date = Date(), hiv_status = character()))
    
    # Assign HIV statuses:
    # Each test is Negative until one comes back Positive, after which testing stops
    statuses <- character(n_tests)
    for (i in seq_len(n_tests)) {
      if (runif(1) < prob_positive) {
        statuses[i] <- "Positive"
        test_dates  <- test_dates[1:i]   # drop any future tests after positive
        statuses    <- statuses[1:i]
        break
      } else {
        statuses[i] <- "Negative"
      }
    }
    
    tibble(
      patient_uid = patient_uid,
      test_date   = test_dates,
      hiv_status  = statuses
    )
  })

cat("Test results rows:     ", nrow(test_results), "\n")
cat("Patients with tests:   ", n_distinct(test_results$patient_uid), "\n")
cat("HIV positive patients: ", test_results %>% filter(hiv_status == "Positive") %>% n_distinct("patient_uid"), "\n")

# Quick sense check: no patient should have a test after a positive
test_results %>%
  group_by(patient_uid) %>%
  mutate(any_positive = cumany(hiv_status == "Positive"),
         lag_positive = lag(any_positive, default = FALSE)) %>%
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
