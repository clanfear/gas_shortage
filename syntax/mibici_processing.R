library(tidyverse)
library(tidylog)
library(lubridate)

mibici_locations <- read_csv("./data/MiBici/data/nomenclatura_2020_06.csv")


files_df <- "./data/mibici/" %>%
  paste0(., list.files(.)) %>%
  str_subset(., "\\.csv$") %>%
  as.data.frame(.) %>%
  setNames(., "filename") %>%
  mutate(year = str_remove_all(str_extract(filename, "_20[0-9][0-9]_"), "_")) %>%
  mutate(month = as.numeric(str_remove_all(str_extract(filename, "_[0-9][0-9]\\.csv$"), "(_)|(\\.csv)")))

read_in_file <- function(x){
  vroom::vroom(x["filename"],
               col_names = c( "trip_id", "user_id", "gender", "birth_year", "start_time", "end_time", "start_station", "end_station"),
               skip = 1) %>%
    filter(!is.na(start_time) & birth_year!= 1900) %>%
    mutate(year = as.numeric(x["year"]), month = as.numeric(x["month"])) %>%
    mutate(birth_year = as.character(birth_year))
}

mibici_files <- apply(files_df, 1, read_in_file)

correctly_parsed <- sapply(mibici_files, function(x) any(sapply(x, is.POSIXt)))

correctly_parsed_dfs <- mibici_files[correctly_parsed] %>%
  bind_rows()

incorrectly_parsed_dfs <-  mibici_files[!correctly_parsed]

lapply(incorrectly_parsed_dfs, function(x) x[c(1,nrow(x)), "start_time"])

reparsed_dfs_1 <- incorrectly_parsed_dfs[1:6] %>%
  bind_rows() %>%
  mutate(across(c(start_time, end_time), ~dmy_hm(.)))

reparsed_dfs_2 <- incorrectly_parsed_dfs[[7]] %>%
  mutate(across(c(start_time, end_time), ~mdy_hm(.)))

mibici <- bind_rows(correctly_parsed_dfs, reparsed_dfs_1, reparsed_dfs_2)

save(mibici, file = "./data/derived/mibici.RData")
