# Construct data

library(tidyverse)
library(lubridate)

df1 <- data.frame(a = c(1,2,3,4,5), b = c("dsf df", "ddkk", "f, fds, ffff", "BB", NA), c = c(ymd("2019-07-08"), NA, ymd("2019-07-08"),NA,ymd("2019-07-09")))
df3 <- df2 <- df1

# Pretend we receive several separate text files
write_tsv(df1, "./file 1.tsv")
write_tsv(df2, "./file 2.tsv")
write_tsv(df3, "./file 3.tsv")

# Check all tsv files in work dir.
file_paths <- Sys.glob("./*.tsv")
out_file <- "./test.tsv"
file_paths <- file_paths[file_paths != out_file]

# Read each text file and concatenate into a bigger text file
# At the same time reading each of them into a dataframe
data_list <- lapply(file_paths, function(file){
  lines <- readLines(file)
  if(file == file_paths[1]){
    cat(lines, file = out_file, sep = "\n", append = FALSE)
  } else {
    cat(lines[-1], file = out_file, sep = "\n", append = TRUE)
  }
  
  return(read_tsv(file))
  
})

# Row merge the list of dataframes into a big one
combined_data <- bind_rows(data_list)

# Test to see if reading from the combined file gives the same result as the combined_data df
read_check <- read_tsv("./test.tsv")
