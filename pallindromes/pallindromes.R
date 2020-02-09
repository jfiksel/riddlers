library(dplyr)
### All dates for months with 31 days
months_31_days <- expand.grid(month = c("01", "03", "05", "07", "08", "10", "12"),
                              day = c(paste0("0", 1:9), as.character(10:31)),
                              year = as.character(2020:2100))
### All dates for months with 30 days
months_30_days <- expand.grid(month = c("04", "06", "09", "11"),
                              day = c(paste0("0", 1:9), as.character(10:30)),
                              year = as.character(2020:2100))
### Start with February including 29 days in each year
feb_days <- expand.grid(month = c("02"),
                        day = c(paste0("0", 1:9), as.character(10:29)),
                        year = as.character(2020:2100))
### Now get rid of non leap years
non_leap_years <- (as.numeric(as.character(feb_days$year)) %% 4) != 0
nonexist_days <- feb_days[feb_days$day == "29" & non_leap_years,]
feb_days <- anti_join(feb_days, nonexist_days)

### Combine to get all days until Dec. 31, 2100
all_days <- bind_rows(months_31_days, months_30_days) %>% bind_rows(feb_days)
### Get strings for all dates
date_string <- paste0(all_days$month, all_days$day, all_days$year)

### Function to reverse string
strReverse <- function(x) {
    sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
}
### Get reverse date
rev_date_string <- unname(sapply(date_string, strReverse))

### which are equal
pallindromes <- which(date_string == rev_date_string)

### Look at dates
all_days[pallindromes,]

### Remove 02/02/2020
npalindromes <- length(pallindromes) - 1
