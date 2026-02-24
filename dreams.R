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
test_date       <- enrollment_date + days(sample(1:180, n, replace = TRUE))
test_date       <- pmin(test_date, today)

hiv_status     <- sample(c("Positive", "Negative", "Unknown"), n, replace = TRUE, prob = c(0.25, 0.65, 0.10))

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

# --- 1. DOB in D-M-Y format (days > 12 to make it obvious) ---
# Pick rows where day > 12, then format as d/m/Y string
dmy_idx <- which(day(df$date_of_birth) > 12)[1:15]
df$date_of_birth <- as.character(df$date_of_birth)   # convert col to character first
df$date_of_birth[dmy_idx] <- format(
  as.Date(df$date_of_birth[dmy_idx]),
  "%d/%m/%Y"   # e.g. 23/08/2003 — unambiguous swap
)

# --- 2. Dates in the future ---
future_idx <- sample(setdiff(1:n, dmy_idx), 10)
df$test_date[future_idx]       <- as.character(today + days(sample(30:365, 10)))
df$enrollment_date[future_idx] <- as.character(today + days(sample(10:60,  10)))

# --- 3. Duplicate patients ---
dup_idx <- sample(1:n, 8)
df <- bind_rows(df, df[dup_idx, ])   # exact duplicates appended

# --- 4. Same-day repeated test scores (same patient, same test_date) ---
repeat_idx <- sample(1:n, 6)
repeat_rows <- df[repeat_idx, ] %>%
  mutate(hiv_status = sample(c("Positive", "Negative", "Unknown"), 6, replace = TRUE))
df <- bind_rows(df, repeat_rows)

# --- 5. Test / enrollment dates BEFORE date of birth ---
pre_dob_idx <- sample(1:n, 8)
df$enrollment_date[pre_dob_idx] <- as.character(
  as.Date(df$date_of_birth[pre_dob_idx]) - days(sample(100:1000, 8))
)
df$test_date[pre_dob_idx] <- as.character(
  as.Date(df$date_of_birth[pre_dob_idx]) - days(sample(10:90, 8))
)

# --- 6. Missingness ---
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
