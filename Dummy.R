library(tidyverse)
library(lubridate)
library(zoo)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HTS Creation ----
n <- 39159
ids <- seq(n)
# Use yield to distribute + and -: Male 4.2%; Female 3.1%
test_dates <- sample(seq(as.Date('2015/01/01'), as.Date('2022/01/01'), by="day"), n, replace = TRUE)
age <- floor(runif(n, min = 4, max = 75))
dob <- test_dates - age
sex <- rbinom(n = n, size = 1, prob = 0.7)
sex <- replace(sex, sex == 0, "F")
sex <- replace(sex, sex == 1, "M")
mods <- c("Emergency Ward", 
          "Index",
          "IndexMod",
          "Inpat",
          "Malnutrition",
          "MobileMod",
          "OtherMod",
          "OtherPITC",
          "Pediatric",
          "PMTCT ANC",
          "PMTCT PostANC",
          "Post ANC1",
          "SNS",
          "SNSMod",
          "STI Clinic",
          "TBClinic",
          "VCT",
          "VCTMod",
          "VMMC")

df_hts <- data.frame(ids, test_dates, dob, sex)

df_hts <- df_hts %>%
  mutate(test_result = case_when(sex=="M" ~rbinom(n(), size=1, prob = 0.042),
                                 sex=="F" ~rbinom(n(), size=1, prob = 0.031))) %>%
  mutate(test_result = ifelse(test_result == 1, "Positive", "Negative")) 

df_hts["modality"] <- mods[sample(1:length(mods), n, replace = T)]

write.csv(df_hts, "HTS.csv")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TX_NEW Creation ----
# Make sure POS IDs transfer to next step (not all depending on LINKAGE): Male 96%; Female 97%
df_txnew <- df_hts %>%
  filter(test_result == "Positive") %>%
  mutate(initiate = case_when(sex=="M" ~rbinom(n(), size = 1, prob = 0.96),
                              sex=="F" ~rbinom(n(), size = 1, prob = 0.97))) %>%
  filter(initiate == 1) %>%
  select(-initiate) %>%
  mutate(initiation_dates = rbinom(n(), size=1, prob = 0.85)) %>% # Assign a few to initiate later
  rowwise() %>%
  mutate(initiation_dates = as.Date(ifelse(initiation_dates == 1, test_dates, test_dates + sample(1:30, 1, replace=TRUE)))) %>%
  ungroup() %>%
  select(-modality, -test_result, -test_dates)

# Add Errors (n = 12)
# DOB > Initiation Date (n = 5)
txnew_errors <- sample(df_txnew$ids, 12)
datevalue_errors <- txnew_errors[1:5]

df_txnew <- df_txnew %>%
  rowwise() %>%
  mutate(dob = case_when(ids %in% datevalue_errors ~initiation_dates + sample(1:30, 1, replace=TRUE),
                         TRUE ~dob)) %>%
  ungroup()

# Errors in dob (n = 3)
datevalue_errors2 <- txnew_errors[6:8]
df_txnew <- df_txnew %>%
  mutate(dob = case_when(ids %in% datevalue_errors2 ~as.Date("1900-01-01"),
                         TRUE ~dob))

# Errors in initiation (n = 4)
datevalue_errors3 <- txnew_errors[9:12]
df_txnew <- df_txnew %>%
  mutate(initiation_dates = as.character(initiation_dates),
         initiation_dates = case_when(ids %in% datevalue_errors3 ~"9999",
                                      TRUE ~initiation_dates))

write.csv(df_txnew,"TX_NEW.csv", row.names = F)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TX_CURR Creation ----
followup_stagger <- sample(df_txnew$ids, floor(0.85*length(df_txnew$ids)))

df_txcurr <- df_txnew %>%
  filter(!ids %in% txnew_errors) %>%
  mutate(initiation_dates = as.Date(initiation_dates)) %>%
  rowwise() %>%
  mutate(visit0      = initiation_dates,
         follow_up0  = initiation_dates %m+% months(1),
         visit1      = follow_up0 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up1  = visit1  %m+% months(1),
         visit2      = follow_up1 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up2  = visit2  %m+% months(1),
         visit3      = follow_up2 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up3  = visit3  %m+% months(1),
         visit4      = follow_up3 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up4  = visit4  %m+% months(1),
         visit5      = follow_up4 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up5  = visit5  %m+% months(1),
         visit6      = follow_up5 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         
         follow_up6  = visit6 %m+% months(1),
         visit7      = follow_up6 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up7  = visit7  %m+% months(1),
         visit8      = follow_up7 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up8  = visit8  %m+% months(1),
         visit9      = follow_up8 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up9 = visit9  %m+% months(1),
         visit10     = follow_up9 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up10 = visit10  %m+% months(1),
         visit11     = follow_up10 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up11 = visit11  %m+% months(1),
         visit12     = follow_up11 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         
         follow_up12 = visit12 %m+% months(1),
         visit13     = follow_up12 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up13 = visit13  %m+% months(1),
         visit14     = follow_up13 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up14 = visit14  %m+% months(1),
         visit15     = follow_up14 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up15 = visit15  %m+% months(1),
         visit16     = follow_up15 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up16 = visit16  %m+% months(1),
         visit17     = follow_up16 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up17 = visit17  %m+% months(1),
         visit18     = follow_up17 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         
         follow_up18 = visit18 %m+% months(1),
         visit19     = follow_up18 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up19 = visit19  %m+% months(1),
         visit20     = follow_up19 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up20 = visit20  %m+% months(1),
         visit21     = follow_up20 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up21 = visit21  %m+% months(1),
         visit22     = follow_up21 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up22 = visit22  %m+% months(1),
         visit23     = follow_up22 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up23 = visit23  %m+% months(1),
         visit24     = follow_up23 + ifelse(!ids %in% followup_stagger, 0, ceiling(runif(1, min = -10, max = 100))),
         follow_up24 = visit24  %m+% months(1)) %>%
  ungroup()

# Deletions
# Initiation Only
txcurr_errors <- sample(df_txcurr$ids, 30)
datevalue_errors4 <- txcurr_errors[1:15]
# Middle deletions
## Taken care of by sampling
# Random deletions 
datevalue_errors5 <- txcurr_errors[16:30]

# Implement random deletions
df_txcurr2 <- df_txcurr %>% 
  pivot_longer(5:length(.), names_to = "attribute", values_to = "dates") %>% 
  mutate(obs       = str_extract(attribute, "[[:digit:]]+"),
         attribute = gsub('[[:digit:]]+', '', attribute)) %>% # Extract out obs #
  pivot_wider(names_from = attribute, values_from = dates) %>% # Columns for visit and scheduled follow-up
  rowwise() %>%
  mutate(counter = rbinom(n = 1, size = 1, prob = 0.7), # Add some randomness for deleting obs for later dates
         marked  = case_when(ids %in% datevalue_errors4 & obs != 1 ~"delete_intiation", # Delete all other obs besides initiation
                             ids %in% datevalue_errors5 & obs != 1 & counter == 1 ~"delete_random", # Delete random obs besides initiation
                             TRUE ~"keep")) %>%
  ungroup() %>%
  mutate(next_follow_up = visit %m+% months(1)) %>% # Add next follow-up for each visit to use for randomly deleted obs
  mutate(follow_up      = case_when(ids %in% datevalue_errors5 & obs != 1 & counter == 1 ~NA_Date_,
                               TRUE ~follow_up),
         next_follow_up = case_when(ids %in% datevalue_errors5 & obs != 1 & counter == 1 ~NA_Date_,
                               TRUE ~next_follow_up)) %>%
  group_by(ids, initiation_dates, dob, sex) %>%
  mutate(counter_lag = lag(counter), # Move counter up to check for subsequent deletions
         next_follow_up = na.locf0(next_follow_up, fromLast = F)) %>% # Fill down next_follow_up
  ungroup() %>%
  mutate(next_follow_up = case_when(ids %in% datevalue_errors5 & obs != 1 & marked == "keep" & counter_lag != 0 ~NA_Date_, # Remove those later follow-up dates for keep so that previous next_follow_up can replace
                                    TRUE ~next_follow_up)) %>%
  group_by(ids, initiation_dates, dob, sex) %>%
  mutate(next_follow_up = na.locf0(next_follow_up, fromLast = F)) %>% # Fill down next_follow_up
  ungroup() %>%
  mutate(follow_up = case_when(ids %in% datevalue_errors5 & obs != 1 & marked == "keep" & counter_lag != 0 ~next_follow_up,
                               TRUE ~follow_up)) %>%
  filter(marked == "keep") %>%
select(-c(obs, counter, counter_lag, marked, next_follow_up))

write.csv(df_txcurr2, "TXS.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# VL Creation ----
df_pvls <- df_txcurr2 %>%
  rowwise() %>%
  mutate(vltestdone = ifelse(initiation_dates == visit, 1, rbinom(1, size = 1, prob = 0.6))) %>% 
  mutate(vlsuppressed = ifelse(initiation_dates == visit, 0,
                               ifelse((!ids %in% followup_stagger) & vltestdone == 1, rbinom(1, size = 1, prob = 0.9),
                                      ifelse((ids %in% followup_stagger) & vltestdone == 1, rbinom(1, size = 1, prob = 0.5), NA)))) %>%
  ungroup()

write.csv(df_pvls, "PVLS.csv", na = "")
