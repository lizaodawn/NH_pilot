install.packages('tidyverse')
install.packages('mclm')
install.packages('here')
install.packages('kableExtra')
install.packages('readr')
install.packages("stringr")
library('tidyverse')
library('mclm')
library('here')
library('kableExtra')
library(readr)
library(stringr)
fpath_target <- here("geotext_indianregion.csv")
fpath_ref <- here("wholebooktext.csv")
# Read the CSV file
data_target <- read_csv(fpath_target)
data_ref <- read_csv(fpath_ref)


# Preprocess the "Plain_text" column
data_target$Text <- tolower(data_target$Text)
data_ref$Text <- tolower(data_ref$Text)
flist_target <- freqlist(data_target$Text, as_text = TRUE) %>% print()
flist_ref <- freqlist(data_ref$Text, as_text = TRUE) %>% print()

# calculate scores
scores_kw <- assoc_scores(flist_target, flist_ref)

# print scores, sorted by PMI
print(scores_kw, sort_order = "PMI")
print(scores_kw, sort_order = "G_signed")

top_scores_kw <- scores_kw %>% 
  filter(PMI >= 2 & G_signed >= 2)

# print top_scores_kw, sorted by PMI
top_scores_kw %>%
  print(sort_order = "PMI")

# print top_scores_kw, sorted by G_signed
top_scores_kw %>%
  print(sort_order = "G_signed")

coocs <- data_ref %>% 
  surf_cooc("(?xi)  ^ india $")
