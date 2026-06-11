
library(jsonlite)

# 1. Loading S2AG API Dataset

#df <- stream_in(
  #gzfile("20260529_070610_00124_u6igq_08555d5d-32fe-4398-9238-19aeb3d4574a.gz"),
  #pagesize = 50000
#)

# 2. Checking Data

getwd()
head(df)
names(df)
View(df)
nrow(df)
head(df$s2fieldsofstudy)

# 3. Filtering to only biomedical related papers
bio <- subset(
  df,
  sapply(df$s2fieldsofstudy, function(x) {
    if (is.null(x)) return(FALSE)
    any(grepl("Biology|Medicine|Chemistry|Psychology",
              x$category, ignore.case = TRUE))
  })
)

# 4. Converting qualitative to quantitative data

# Number of authors
bio$n_authors <- sapply(bio$authors, function(x) {
  if (is.null(x)) return(0)
  length(x)
})

# Title length
bio$title_length <- nchar(bio$title)

# Fix missing years using median
bio$year <- ifelse(is.na(bio$year),
                   median(bio$year, na.rm = TRUE),
                   bio$year)

# 5. Extracting desired data for analysis
clean <- bio[, c("corpusid", "title", "year",
                 "n_authors", "title_length",
                 "referencecount", "citationcount", "influentialcitationcount")]

# 6. Exporting to csv file
write.csv(clean, "biomed_clean.csv", row.names = FALSE)
