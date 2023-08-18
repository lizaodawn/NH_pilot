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
library('leaflet')

data <- read.csv("geotext_whole.csv")

summary_data <- data %>%
  group_by(ToposText_ID, Place_Name, Lat, Long) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() 


# Create a bar chart using ggplot2
top_20_summary_data <- summary_data %>%
  top_n(20, Count) %>%
  ungroup()

ggplot(top_20_summary_data, aes(x = reorder(Place_Name, -Count), y = Count)) +
  geom_col() +
  labs(x = "Place name", y = "Number of occurrences") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Create a leaflet map
mapping <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = summary_data,
    lat = ~Lat,
    lng = ~Long,
    radius = sqrt(summary_data$Count) * 0.8,  # Adjust the scaling factor as needed
    color = "blue",
    fill = TRUE,
    fillOpacity = 0.6,
    popup = paste("Place name:", summary_data$Place_Name, "<br>",
                  "Number of occurences:", summary_data$Count)
  )


corpus_folder_whole <- here("NH_wholetext")
fnames_wholetext <- get_fnames(corpus_folder_whole) %>% 
  keep_re("[.]txt")

print(fnames_wholetext, 10, hide_path = corpus_folder_whole)

corpus_folder_in <- here("NH_geotext_india")
fnames_indiatext <- get_fnames(corpus_folder_in) %>% 
  keep_re("[.]txt")

print(fnames_indiatext, 10, hide_path = corpus_folder_in)


# build frequency list for reference corpus
flist_ref <- fnames_wholetext %>%
  freqlist

# build frequency list for target corpus
flist_target <- fnames_indiatext %>% 
  freqlist

# calculate scores
scores_kw <- assoc_scores(flist_target, flist_ref)

top_scores_kw <- scores_kw %>% 
  filter(PMI >= 2 & G_signed >= 2)

top_scores_kw %>% 
  as_tibble() %>%
  select(type, a, PMI, G_signed) %>% 
  arrange(desc(PMI)) %>%             
  slice_max(order_by = G_signed, n = 20, with_ties = TRUE) %>%                       
  kbl(col.names = c("Type", "Frequency", "PMI", r"(Signed $G^2$)")) %>% 
  kable_minimal() %>% 
  scroll_box(height = "400px") %>%  print()


keyword_PMI_list <- data.frame(
  Word = c("ganges", "beryls", "ichthyophagi", "megasthenes", "obsidian", "bdellium", "agates", "callaina", "condensation", "gerra", "jomanes", "nonius", "prasii", "alia", "carnelian", "cophes", "hypasis", "merchandize", "peppertree", "sacae", "sandastros", "thornbush"),
  Exp = c("a major river in India", "a type of gemstone", "a group of people who primarily subsist on fish", "a Greek historian and diplomat", "a type of volcanic glass", "a fragrant resin obtained from certain trees", "a type of semiprecious gemstone", "pale green precious stone (lat)", "the process of vapor turning into a liquid state", "war (lat)", "a Roman nomen gentile, gens or \"family name\"", "a Roman nomen gentile, gens or \"family name\"", "prase, green coloured gem", "by another / different way / route (lat)", "a reddish-brown variety of chalcedony", "a river that rises in the ancient Paropamise range, eventually falling into the Indus river near its confluence with the Cophes river", "a river in north India", "goods or commodities", "a tree that produces peppercorns", "the easternmost nation of Elibe, situated to the south of Ilia and the north of Bern", "a precious stone found in India and Arabia", "any of many thorny or spiny shrubs and bushes"),
  Tag = c("river", "goods", "people", "people", "goods", "goods", "goods", "goods", "activity", "activity", "people", "people", "goods", "route", "goods", "river", "river", "goods", "tree", "river", "goods", "tree"),
  Sub_tag = c("N/A", "stone", "group of people", "famous people", "others", "others", "stone", "stone", "producing activity", "N/A", "human name", "human name", "stone", "N/A", "stone", "N/A", "N/A", "general", "origin of goods", "N/A", "stone", "N/A")
)

keyword_PMI_list %>% 
  as_tibble() %>%
  select(Word, Exp, Tag, Sub_tag) %>%                        
  kbl(col.names = c("Word", "Exp", "Tag", "Sub_tag")) %>% 
  kable_minimal() %>% print()

tag_counts <- keyword_PMI_list %>%
  count(Tag) %>%
  arrange(desc(n))

# Plot the tag distribution
ggplot(tag_counts, aes(x = n, y = reorder(Tag, -n), fill = Tag)) +
  geom_bar(stat = "identity") +
  labs(x = "Count", y = "Tag", fill = "Tag") +
  theme_minimal() +
  coord_flip()


coocs <- fnames_wholetext %>% 
  surf_cooc("(?xi)  ^ india $",
            re_token_transf_in = "[[:punct:]]", # Match punctuation marks
            token_transf_out = "")

# calculate scores
scores_colloc <- assoc_scores(coocs)

top_scores_colloc <- scores_colloc %>% 
  filter(PMI >= 2 & G_signed >= 2)

top_scores_colloc %>% 
  as_tibble() %>%
  select(type, a, PMI, G_signed) %>% # select 4 columns
  slice_max(order_by = G_signed, n = 20, with_ties = TRUE) %>%                       # select top 30 rows
  kbl(col.names = c("Type", "Frequency", "PMI", r"(Signed $G^2$)"), escape = FALSE) %>%
  kable_minimal() %>% 
  scroll_box(height = "400px") %>% print()


conc_data <- conc(fnames_wholetext, '\\bindia\\b')

conc_data %>%
  as_tibble() %>%
  select(source, left, match, right) %>%
  mutate(
    source = short_names(source),
    book = as.integer(gsub("^(\\d+).*", "\\1", source)),
    chapter = as.numeric(gsub("^\\d+\\.(\\d+)\\.\\d+_text$", "\\1", source)),
    paragraph = as.numeric(gsub("^\\d+\\.\\d+\\.(\\d+)_text$", "\\1", source))
  ) %>%
  arrange(book, chapter, paragraph) %>%
  select(book, chapter, paragraph, left, match, right) %>%
  kbl(align = c("r", "r", "r", "c", "l", "l", "l")) %>%
  kable_paper(font_size = 15) %>%
  scroll_box(height = "400px")

 
quarto::render("paper_quarto.md", to = "html", verbose = TRUE)

