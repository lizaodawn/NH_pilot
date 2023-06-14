install.packages('tidyverse')
install.packages('mclm')
install.packages('here')
install.packages('kableExtra')


library('tidyverse')
library('mclm')
library('here')
library('kableExtra')


fpath_target <- here("geotext_indianregion.csv")
fpath_ref <- here("wholebooktext.csv")
fname
# Read the CSV file
data_target <- read_csv(fpath_target)
data_ref <- read_csv(fpath_ref)


# Preprocess the "Plain_text" column
data_target$Text <- tolower(data_target$Text)
data_ref$Text <- tolower(data_ref$Text)

corpus_target <- unique(data_target$Text)
corpus_ref <- unique(data_ref$Text)

flist_target <- freqlist(corpus_target, as_text = TRUE) %>% print()
flist_ref <- freqlist(corpus_ref, as_text = TRUE) %>% print()

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





corpus_folder <- here("NH_wholetext")
fnames_wholetext <- get_fnames(corpus_folder) %>% 
  keep_re("[.]txt")

print(fnames_wholetext, 10, hide_path = corpus_folder)

corpus_folder <- here("NH_geotext_india")
fnames_indiatext <- get_fnames(corpus_folder) %>% 
  keep_re("[.]txt")

print(fnames_indiatext, 10, hide_path = corpus_folder)

# build frequency list for target corpus
flist_target <- fnames_indiatext %>%
  freqlist(
    re_token_splitter = r"--[(?xi)    \s+   ]--", # whitespace as token splitter
    re_token_transf_in = "[[:punct:]]", # Match punctuation marks
    token_transf_out = "" # Replace punctuation marks with an empty string
  ) %>%
  print()

# build frequency list for reference corpus
flist_ref <- fnames_wholetext %>%
  freqlist(re_token_splitter = r"--[(?xi)    \s+   ]--", # whitespace as token splitter
           re_token_transf_in = "[[:punct:]]", # Match punctuation marks
           token_transf_out = "") %>%
  print()

# calculate scores
scores_kw <- assoc_scores(flist_target, flist_ref)

# print scores, sorted by PMI
print(scores_kw, sort_order = "PMI")

coocs <- fnames_wholetext %>% 
  surf_cooc("(?xi)  ^ india $")
coocs$target_freqlist
coocs$ref_freqlist

# calculate scores
scores_colloc <- assoc_scores(coocs)

# print scores, sorted by PMI
print(scores_colloc, sort_order = "PMI")

# print scores, sorted by G_signed
print(scores_colloc, sort_order = "G_signed")



