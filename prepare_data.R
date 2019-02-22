Sys.setlocale("LC_ALL", 'en_US.UTF-8')
knitr::asis_output("\U2500  \U2665  \U2666  \U2663")
lemmatization_url <- "https://raw.githubusercontent.com/michmech/lemmatization-lists/master/lemmatization-en.txt"

library(tidyverse)
library(tidytext)
library(pdftools)
library(stopwords)
library(purrr)
library(topicmodels)
library(viridis)
library(SnowballC)

source("helpers.r")
# constants
const_first_year <- 2007
const_last_year <- 2017



# RESET ALL FILES
const_clean_start <- F



# DEBUG MODE (only 100 files)
debug_mode <- F


file_names <- get_filenames(path="/Volumes/Documents/#users/ACaleroValdez/hciiscrape", clear_cache = F)
const_cache_filename <- "temp/converted_texts.rds"
if(const_clean_start) {
  file.remove(const_cache_filename)
}


# When a cache exists rewrite the file_names list with only missing files
if(file.exists(const_cache_filename)){
  text_df_cache <- readRDS(const_cache_filename)
  if(debug_mode) {
    cachenum <- dim(text_df_cache)[1]
    file_names <- head(file_names, cachenum + 50)
  }
  missing_files <- file_names [!  file_names %in% text_df_cache$filename]
  file_names <- missing_files
  rm(missing_files)
  text_df <- text_df_cache
  rm(text_df_cache)
} else {
  # Ensure data frame exists
  text_df <- data.frame()

}


# Stepping size for reading the files
step_size <- 50
#file_steps <- floor( file_count / step_size )

# How many files need reading in
file_count <- length(file_names)



if(file_count>0){
  start_time <- Sys.time()
  pb <- txtProgressBar(min = 0, max = file_count, style = 2)
  for (i in 1:file_count) {
    setTxtProgressBar(pb, i)
    files_selected <- file_names[i]
    # Debugging if fail
    {
      text_list <- map_chr(files_selected, file2text2)
      paper_list <- map_chr(files_selected, file2name)
      year_list <- map_dbl(files_selected, file2Year)
      new_text_df <- tibble(text = text_list, paper = paper_list,
                            year = year_list, filename = files_selected)
    }

    # bind new row
    text_df <- bind_rows(text_df, new_text_df)
    stop_time <- Sys.time()
    diff_time <- difftime(stop_time, start_time, units = "secs")
    per_item_time = diff_time / i
    remaining_time <- (file_count - i ) * per_item_time

    rtime <- as.numeric(remaining_time)
    hours <- rtime %/% 3600
    mins <- (rtime - (hours*3600)) %/% 60
    secs <- round((rtime - (hours*3600) - (mins*60) ))


    # write cache every step_size message yes or no?

    if(i %% step_size == 0) {
      cat(paste("\n", i, "files of", file_count,"parsed. Cache updated. Expected runtime:",
                paste0(hours,"h:",mins,"m:",secs,"s"),"\n"))
      saveRDS(text_df, const_cache_filename, compress = "gzip")
      Sys.sleep(1)
    }

    # Merge Cache with new data
  }
  saveRDS(text_df, const_cache_filename, compress = "gzip")
  rm(text_list)
  rm(paper_list)
  rm(year_list)
  close(pb)
}
