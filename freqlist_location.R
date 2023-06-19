install.packages('tidyverse')
install.packages('mclm')
install.packages('here')
install.packages('kableExtra')
install.packages("leaflet")
install.packages("quarto")

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
# Display the map
m

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

# print scores, sorted by G_signed
print(scores_kw, sort_order = "G_signed")

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
  scroll_box(height = "400px")

top_scores_kw %>% # also valid for top_scores_colloc
  as_tibble() %>%
  select(type, a, PMI, G_signed) %>% # select 4 columns
  arrange(desc(PMI)) %>%             # sort by PMI (descending) 
  head(30) %>%                       # select top 30 rows
  kbl(col.names = c("Type", "Frequency", "PMI", r"(Signed $G^2$)")) %>% 
  kable_minimal() %>% 
  scroll_box(height = "400px")

quarto::render("paper_quarto.md", to = "html", verbose = TRUE)
