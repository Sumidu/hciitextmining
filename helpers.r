

# Generate an alternativ palette from ??
alternate_palette <- function(palette){
  l <- length(palette)
  a <- palette[1:(l/2)]  # first half
  b <- palette[(floor(l/2)+1):l] # second half
  result <- c(a, b)[order(c(seq_along(a)*2 - 1, seq_along(b)*2))]
  result
}


#filename <- "data/Aarås2007 Chapter CanVisualDiscomfortInfluenceOn/Aarås2007_Chapter_CanVisualDiscomfortInfluenceOn.pdf"
file2text2 <- function(filename){
  author <- str_match(filename, "\\/([:alpha:]*)(?:[0-9]{4})")[1,2]
  year <- str_extract(filename, "([0-9]+)")
  pages <- pdf_data(filename)

  res <- ""
  for(i in 1:length(pages)) {
    content <- pages[[i]]
    temp <- paste(content$text, collapse = " ")
    res <- paste0(res, temp)
  }
  text <- res %>% str_to_lower()
  title_pos <- str_locate(res, author)[1]
  title <- str_sub(text,1,title_pos-4) %>% str_squish()
  startpos <- str_locate(text, "references 1.")[1]
  if(is.na(startpos)) {
    locations <- str_locate_all(text, "references")
    last <- length(locations)
    startpos <- locations[[last]][1]
  }
  stripped_text <- str_sub(text, title_pos-4, startpos)
  stripped_text %>% str_remove_all("([0-9]*)") %>%
    str_remove_all("\\(") %>%
    str_remove_all("\\)") %>%
    str_remove_all("\\]") %>%
    str_remove_all("\\[") %>%
    str_replace_all("ﬀ", "ff") %>%  #ligatures
    str_replace_all("ﬁ", "fi") %>%
    str_replace_all("ﬂ", "fl") %>%
    str_replace_all("ﬃ","ffi")%>%
    str_replace_all("ﬄ", "ffl") %>%
    str_replace_all("Ĳ", "IJ") %>%
    str_replace_all("ĳ", "ij") %>%
    str_replace_all("ﬆ", "st") %>%
    iconv(from="latin1", to="ASCII", "") %>%
    str_squish() -> restext

  tibble(author=author,year=year,title=title,text=restext,filename=filename)
}

## This function gets the text from a given file name
file2text <- function(filename) {
  rawtext <- pdf_text(filename)
  text <- glue::glue_collapse(rawtext)
  text <- str_replace_all(text, "\\n", " ") %>%
    str_to_lower() %>%
    str_remove_all("([0-9]*)") %>%
    str_remove_all("\\(") %>%
    str_remove_all("\\)") %>%
    str_replace_all("ﬀ", "ff") %>%  #ligatures
    str_replace_all("ﬁ", "fi") %>%
    str_replace_all("ﬂ", "fl") %>%
    str_replace_all("ﬃ","ffi")%>%
    str_replace_all("ﬄ", "ffl") %>%
    str_replace_all("Ĳ", "IJ") %>%
    str_replace_all("ĳ", "ij") %>%
    str_replace_all("ﬆ", "st") %>%
    iconv(from="latin1", to="ASCII", "")

  text
}


## Function that extracts the paper name from AuthorYearTitle1:3
file2name <- function(filename) {
  filename <- filename %>%  iconv(from="latin1", to="ASCII", "")
  year <- str_extract(filename, "([0-9]+)")
  paper <- str_match_all(filename, "[0-9]+[ _]Chapter[ _](([A-Z]+[a-z]+)++)")
  author <- str_match(filename, "\\/([A-z- ]*)(?:[0-9]{4})")[1,2]
  paper <- unlist(as.vector(paper))
  # Concatenate Author, year, and first three Words
  paste0(author, year, paper[3])
}

# Debug Help (run file-getting from next chunk before)
#filename <- file_names[1]
#file2name(filename)

file2Year <- function(filename) {
  year <- str_extract(filename, "([0-9]+)")
  year <- as.numeric(year)
  year
}




# function to return all filenames (uses a rds cache in temp directory)
get_filenames <- function(path="", clear_cache = F) {
  cache_file <- "temp/cache_filenames.rds"
  if(path == "") {
    directory_name <- "data"
  }
  else {
    directory_name <- path
  }

  if (file.exists(cache_file) && clear_cache) {
    file.remove(cache_file)
  }
  if(file.exists(cache_file)){
    file_names <- read_rds(cache_file)
  }
  else {
    file_names <- dir(path = directory_name, pattern = ".pdf", full.names = T, recursive = T)
    write_rds(file_names, cache_file)
  }

  file_names
}

