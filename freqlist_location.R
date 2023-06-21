install.packages('tidyverse')
install.packages('mclm')
install.packages('here')
install.packages('kableExtra')
install.packages("leaflet")
install.packages("quarto")
install.packages("topicmodels")
install.packages("tm")

library(topicmodels)
library(tm)

library('tidyverse')
library('mclm')
library('here')
library('kableExtra')
library('ggplot2')
library('dplyr')
library('leaflet')

data <- read.csv("geotext_whole.csv")

summary_data <- data %>%
  group_by(ToposText_ID, Place_Name, Lat, Long) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>% print(n=20)

summary_data %>% # also valid for top_scores_colloc
  as_tibble() %>%
  select(Place_Name, Count) %>% # select 4 columns
  arrange(desc(Count)) %>%             # sort by PMI (descending) 
  head(20) %>%                       # select top 30 rows
  kbl(col.names = c("Place_Name", "Count")) %>% 
  kable_minimal() %>% 
  scroll_box(height = "400px")

# Create a bar chart using ggplot2
top_20_summary_data <- summary_data %>%
  top_n(20, Count) %>%
  ungroup()

ggplot(top_20_summary_data, aes(x = reorder(Place_Name, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  labs(x = "Place Name", y = "Count") +
  ggtitle("Top 20 Place Names Mentioned") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Create a leaflet map
m <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = top_20_summary_data,
    lat = ~Lat,
    lng = ~Long,
    radius = sqrt(top_20_summary_data$Count) * 0.8,  # Adjust the scaling factor as needed
    color = "blue",
    fill = TRUE,
    fillOpacity = 0.6,
    popup = paste("Place Name:", top_20_summary_data$Place_Name, "<br>",
                  "Count:", top_20_summary_data$Count)
  )


corpus_folder <- here("NH_wholetext")
fnames_wholetext <- get_fnames(corpus_folder) %>% 
  keep_re("[.]txt")

print(fnames_wholetext, 10, hide_path = corpus_folder)

corpus_folder <- here("NH_geotext_india")
fnames_indiatext <- get_fnames(corpus_folder) %>% 
  keep_re("[.]txt")

print(fnames_indiatext, 10, hide_path = corpus_folder)

prettyNum(n_tokens(ref_flist))
n_types(ref_flist)
prettyNum(n_tokens(tar_flist))
n_types(tar_flist)

# build frequency list for target corpus
flist_target <- fnames_indiatext %>%
  freqlist(re_token_transf_in = "[[:punct:]]", # Match punctuation marks
           token_transf_out = "",# Replace punctuation marks with an empty string
           re_drop_token = "india"
    ) %>%
  print()

# build frequency list for reference corpus
flist_ref <- fnames_wholetext %>%
  freqlist(re_token_transf_in = "[[:punct:]]", # Match punctuation marks
           token_transf_out = "") %>%
  print()

# calculate scores
scores_kw <- assoc_scores(flist_target, flist_ref)

# print scores, sorted by PMI
print(scores_kw, sort_order = "PMI")

# print scores, sorted by G_signed
print(scores_kw, sort_order = "G_signed")


keyword_PMI_tibble <- tibble(
  Word = c("ganges", "beryls", "ichthyophagi", "megasthenes", "obsidian", "bdellium", "agates", "callaina", "condensation", "gerra", "jomanes", "nonius", "prasii", "alia", "carnelian", "cophes", "hypasis", "merchandize", "peppertree", "sacae"),
  Type = c("Proper Noun", "Noun", "Plural Noun", "Proper Noun", "Noun", "Noun", "Noun", "Noun/Proper Noun", "Noun", "Noun/Proper Noun", "Noun/Proper Noun", "Noun/Proper Noun", "Noun/Proper Noun", "Adverb", "Noun", "Noun/Proper Noun", "Noun/Proper Noun", "Noun", "Noun", "Noun/Proper Noun"),
  Exp = c("a major river in India", "a type of gemstone", "a group of people who primarily subsist on fish", "a Greek historian and diplomat", "a type of volcanic glass", "a fragrant resin obtained from certain trees", "a type of semiprecious gemstone", "pale green precious stone (lat)", "the process of vapor turning into a liquid state", "war (lat)", "a Roman nomen gentile, gens or 'family name'", "a Roman nomen gentile, gens or 'family name'", "prase, green coloured gem", "by another / different way / route", "a reddish-brown variety of chalcedony", "a river that rises in the ancient Paropamise range, eventually falling into the Indus river near its confluence with the Cophes river", "a river in north India", "goods or commodities", "a tree that produces peppercorns", "the easternmost nation of Elibe, situated to the south of Ilia and the north of Bern")
) %>% print()

keyword_G_tibble <- tibble(
  Word = c("hundred", "amber", "ganges", "arabia", "thousand", "glass", "elephants", "rockcrystal", "beryls", "thence", "indus", "ethiopia", "tribe", "pepper", "lustre", "smaragdus", "gates", "iaspis", "ichthyophagi", "megasthenes"),
  Type = c("Noun", "Noun", "Proper Noun", "Proper Noun", "Noun", "Noun", "Noun", "Noun", "Noun", "Adverb", "Noun", "Noun", "Noun", "Noun", "Noun", "Noun", "Noun", "Noun", "Plural Noun", "Proper Noun"),
  Exp = c("the number equivalent to ten multiplied by ten", "a fossilized tree resin", "a major river in India", "a region in the Arabian Peninsula", "the number equivalent to ten multiplied by one hundred", "a hard, brittle substance", "large, intelligent mammals", "a transparent variety of quartz", "a type of gemstone", "from that place or from there", "a major river in South Asia", "a region in Eastern Africa", "a social group with a common ancestry", "a pungent, spicy seasoning", "the quality of light reflected from a surface", "emerald, a green gemstone", "large entrances or doorways", "a type of gemstone", "a group of people who primarily subsist on fish", "a Greek historian and diplomat")
) %>% select(Word, Type, Exp) %>%                        
  kbl(col.names = c("Word", "Type", "Exp")) %>% 
  kable_minimal() %>% 
  scroll_box(height = "400px") %>% print()



# Print the tibble
print(df_tibble)

top_scores_kw <- scores_kw %>% 
  filter(PMI >= 2 & G_signed >= 2)

# print top_scores_kw, sorted by PMI
top_scores_kw %>%
  print(sort_order = "PMI")

# print top_scores_kw, sorted by G_signed
top_scores_kw %>%
  print(sort_order = "G_signed")

coocs <- fnames_wholetext %>% 
  surf_cooc("(?xi)  ^ india $", 
            re_token_splitter = r"--[(?xi)    \s+   ]--", # whitespace as token splitter
            re_token_transf_in = "[[:punct:]]", # Match punctuation marks
            token_transf_out = "")
coocs$target_freqlist
coocs$ref_freqlist

# calculate scores
scores_colloc <- assoc_scores(coocs)

# print scores, sorted by PMI
print(scores_colloc, sort_order = "PMI")

# print scores, sorted by G_signed
print(scores_colloc, sort_order = "G_signed")

top_scores_colloc <- scores_colloc %>% 
  filter(PMI >= 2 & G_signed >= 2)

# print top_scores_colloc, sorted by PMI
top_scores_colloc %>%
  print(sort_order = "PMI")

# print top_scores_colloc, sorted by G_signed
top_scores_colloc %>%
  print(sort_order = "G_signed")

top_scores_colloc %>% # also valid for top_scores_kw
  as_tibble() %>%
  select(type, a, PMI, G_signed) %>% # select 4 columns
  arrange(desc(G_signed)) %>%        # sort by G_signed (descending)  
  head(30) %>%                       # select top 30 rows
  kbl(col.names = c("Type", "Frequency", "PMI", r"(Signed $G^2$)")) %>% 
  kable_minimal() %>% 
  scroll_box(height = "400px") %>% print()

top_scores_kw %>% # also valid for top_scores_colloc
  as_tibble() %>%
  select(type, a, PMI, G_signed) %>% # select 4 columns
  arrange(desc(PMI)) %>%             # sort by PMI (descending) 
  head(30) %>%                       # select top 30 rows
  kbl(col.names = c("Type", "Frequency", "PMI", r"(Signed $G^2$)")) %>% 
  kable_minimal() %>% 
  scroll_box(height = "400px")

corpus <- Corpus(VectorSource(character()))

for (file in fnames_indiatext) {
  text <- readLines(file, warn = FALSE)
  doc <- PlainTextDocument(text)
  corpus <- tm_add_document(corpus, doc)
}
quarto::render("paper_quarto.md", to = "html", verbose = TRUE)

