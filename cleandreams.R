library(tidyverse)
library(lubridate)

today <- Sys.Date()

# ============================================================
# STEP 1: DIAGNOSE DATA QUALITY ISSUES
# ============================================================

cat("\n===== DATA QUALITY REPORT =====\n")

# --- Demographics Table ---
cat("\n--- patient_demographics ---\n")
cat("Total rows:     ", nrow(patient_demographics), "\n")
cat("Distinct UIDs:  ", n_distinct(patient_demographics$patient_uid), "\n")

# Duplicates
demo_dups <- patient_demographics %>%
  group_by(patient_uid) %>%
  filter(n() > 1) %>%
  arrange(patient_uid)
cat("Duplicate rows: ", nrow(demo_dups), "\n")

# Missingness
cat("\nMissing values per column (demographics):\n")
patient_demographics %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  print()

# DOB format issues (d-m-y strings where day > 12, stored as character)
dob_format_issues <- patient_demographics %>%
  filter(str_detect(date_of_birth, "^\\d{2}/\\d{2}/\\d{4}")) %>%
  mutate(day_part = as.integer(str_extract(date_of_birth, "^\\d{2}"))) %>%
  filter(day_part > 12)
cat("\nDOB with likely d/m/Y format (day > 12):", nrow(dob_format_issues), "\n")

# Enrollment date before DOB
enroll_before_dob <- patient_demographics %>%
  filter(
    !str_detect(date_of_birth, "^\\d{2}/\\d{2}/\\d{4}"),  # exclude dmy strings for now
    !is.na(enrollment_date),
    as.Date(enrollment_date) < as.Date(date_of_birth)
  )
cat("Enrollment date before DOB:", nrow(enroll_before_dob), "\n")

# Future enrollment dates
future_enrollment <- patient_demographics %>%
  filter(!is.na(enrollment_date), as.Date(enrollment_date) > today)
cat("Future enrollment dates:   ", nrow(future_enrollment), "\n")

# --- Test Results Table ---
cat("\n--- test_results ---\n")
cat("Total rows:     ", nrow(test_results), "\n")
cat("Distinct UIDs:  ", n_distinct(test_results$patient_uid), "\n")

# Duplicates (same patient, same test_date)
test_dups <- test_results %>%
  group_by(patient_uid, test_date) %>%
  filter(n() > 1) %>%
  arrange(patient_uid, test_date)
cat("Same-day duplicate tests:  ", nrow(test_dups), "\n")

# Future test dates
future_tests <- test_results %>%
  filter(!is.na(test_date), as.Date(test_date) > today)
cat("Future test dates:         ", nrow(future_tests), "\n")

# Missingness
cat("\nMissing values per column (test results):\n")
test_results %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  print()


# ============================================================
# STEP 2: CLEAN DATA
# ============================================================

cat("\n===== CLEANING =====\n")

# --- Clean patient_demographics ---

demo_clean <- patient_demographics %>%
  
  # 1. Fix d/m/Y DOB format -> Y-m-d
  mutate(
    date_of_birth = case_when(
      str_detect(date_of_birth, "^\\d{2}/\\d{2}/\\d{4}") ~
        as.character(dmy(date_of_birth)),
      TRUE ~ date_of_birth
    )
  ) %>%
  
  # 2. Convert date columns to Date type
  mutate(
    date_of_birth   = as.Date(date_of_birth),
    enrollment_date = as.Date(enrollment_date)
  ) %>%
  
  # 3. Flag and remove future enrollment dates
  filter(is.na(enrollment_date) | enrollment_date <= today) %>%
  
  # 4. Flag and remove enrollment dates before DOB
  filter(is.na(enrollment_date) | is.na(date_of_birth) | enrollment_date >= date_of_birth) %>%
  
  # 5. Remove duplicate rows (keep first occurrence)
  distinct(patient_uid, .keep_all = TRUE)

cat("Rows removed from demographics during cleaning:",
    nrow(patient_demographics) - nrow(demo_clean), "\n")

# --- Clean test_results ---

test_clean <- test_results %>%
  
  # 1. Convert to Date type
  mutate(test_date = as.Date(test_date)) %>%
  
  # 2. Remove future test dates
  filter(is.na(test_date) | test_date <= today) %>%
  
  # 3. Remove same-day duplicate tests (keep first occurrence per patient per day)
  distinct(patient_uid, test_date, .keep_all = TRUE)

cat("Rows removed from test results during cleaning:",
    nrow(test_results) - nrow(test_clean), "\n")


# ============================================================
# STEP 3: POST-CLEANING DIAGNOSTICS
# ============================================================

cat("\n===== POST-CLEANING CHECK =====\n")

cat("Demographics rows remaining: ", nrow(demo_clean), "\n")
cat("Duplicate UIDs remaining:    ",
    nrow(demo_clean) - n_distinct(demo_clean$patient_uid), "\n")
cat("Future enrollment dates:     ",
    sum(demo_clean$enrollment_date > today, na.rm = TRUE), "\n")
cat("Enrollment before DOB:       ",
    sum(demo_clean$enrollment_date < demo_clean$date_of_birth, na.rm = TRUE), "\n")
cat("DOB format issues remaining: ",
    sum(str_detect(as.character(demo_clean$date_of_birth), "^\\d{2}/\\d{2}/\\d{4}"), na.rm = TRUE), "\n")

cat("\nTest results rows remaining: ", nrow(test_clean), "\n")
cat("Future test dates:           ",
    sum(test_clean$test_date > today, na.rm = TRUE), "\n")
cat("Same-day duplicate tests:    ",
    test_clean %>% group_by(patient_uid, test_date) %>% filter(n() > 1) %>% nrow(), "\n")

cat("\nMissing values (demographics):\n")
demo_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  print()

cat("\nMissing values (test results):\n")
test_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  print()


# ============================================================
# STEP 4: JOIN
# ============================================================

cat("\n===== JOINING =====\n")

final_df <- left_join(demo_clean, test_clean, by = "patient_uid")

cat("Rows in demo_clean:   ", nrow(demo_clean), "\n")
cat("Rows in test_clean:   ", nrow(test_clean), "\n")
cat("Rows after left join: ", nrow(final_df), "\n")
cat("Patients with no test result: ",
    sum(is.na(final_df$test_date)), "\n")

glimpse(final_df)
