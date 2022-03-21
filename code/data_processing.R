# Puttin' in the usual
library(jtools)
library(purrr)
library(tidyverse)
library(lubridate)

# Sourcing & standardizing
scorecard <- read.csv("../data/Most+Recent+Cohorts+(Scorecard+Elements).csv")
id <- read.csv("../data/id_name_link.csv")
names(scorecard) <- names(scorecard) %>%
  tolower()

# Creating our set
dirty_data <- list.files(path = "../data", 
                   pattern = "trends_up_to_", 
                   full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows

# Removing NA's in trend data and creating out GT 
dirty_data <- dirty_data %>%
  group_by(schname, keyword) %>%
  mutate(index_std = (index - mean(index,na.rm = TRUE))/sd(index, na.rm = TRUE))

# Deleting the dups
id <- id %>%
  group_by(schname) %>%
  mutate(N = n()) %>%
  filter(N == 1)

# Making earnings numerical for our use
scorecard$md_earn_wne_p10.reported.earnings = 
  as.numeric(as.character(scorecard$md_earn_wne_p10.reported.earnings))

# Joining together our sets
id_trends <- id %>% 
  left_join(dirty_data, by = "schname")
scorecard_trends <- id_trends %>% 
  left_join(scorecard, by = c("opeid" = "opeid", "unitid" = "unitid"))

# Honing in on bachelors degree recipients
bd_scorecard_trends <- scorecard_trends %>% 
  filter (preddeg == 3, na.rm = TRUE)

# Median for 'scorecard$md_earn_wne_p10.reported.earnings' is 41700. We use this
# as a binary cutoff
bd_scorecard_trends$medianEarn <- 
  ifelse(bd_scorecard_trends$md_earn_wne_p10.reported.earnings >= 
           41700, "1", "0")

# Selecting only the columns we want
bd_scorecard_trends <- bd_scorecard_trends %>% 
  select(unitid, opeid, schname, keyword, monthorweek, preddeg, 
         md_earn_wne_p10.reported.earnings, medianEarn, index_std)

# Switching to months and putting it together
bd_month <- bd_scorecard_trends %>%
  mutate(date = as.Date(str_sub(monthorweek, 1, 10))) %>%
  group_by(schname, keyword) %>%
  mutate(index_std = (index_std - mean(index_std, na.rm = TRUE))/
           sd(index_std, na.rm = TRUE)) %>%
  group_by(month = floor_date(date, "month"), opeid, 
           md_earn_wne_p10.reported.earnings, medianEarn) %>%
  summarize(index_std = mean(index_std, na.rm = TRUE))

# Getting it regression-ready.
clean_data <- drop_na(bd_month) %>%
  mutate(untreated = md_earn_wne_p10.reported.earnings >= 41700, treated = 
           month >= as.Date("2015-09-12"))

# Shipping & handling
write_csv(clean_data, '../processed_data/clean_data.csv')