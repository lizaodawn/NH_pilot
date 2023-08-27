install.packages('tidyverse')
install.packages('mclm')
install.packages('here')
install.packages('kableExtra')
install.packages("leaflet")
install.packages("downlit")



library('tidyverse')
library('mclm')
library('here')
library('kableExtra')
library('leaflet')
library('downlit')

# read the geographical-related dataset
data <- read.csv("geotext_whole.csv")

# sort frequency of place names being mentioned
# for bar plot and map visualization
summary_data <- data %>%
  group_by(ToposText_ID, Place_Name, Lat, Long) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() 

# read the corpus files for whole texts
refcorpus_folder <- here("NH_wholetext")
fnames_wholetext <- get_fnames(refcorpus_folder) %>% 
  keep_re("[.]txt")

# read the corpus files for India-related texts
tarcorpus_folder <-  here("NH_geotext_india")
fnames_indiatext <- get_fnames(tarcorpus_folder) %>% 
  keep_re("[.]txt") 


# get the short file name for filtering out India-related texts
# from the corpus of whole texts
fnames_wholetext_short <- basename(fnames_wholetext)
fnames_indiatext_short <- basename(fnames_indiatext)

# exclude short file names with the same name from India-related texts corpus in the whole texts corpus
fnames_wholetext_short_filtered <- fnames_wholetext_short %>%
  setdiff(fnames_indiatext_short)

# filter full file names based on filtered short names
# the filtered corpus will serve as the reference corpus in keyword analysis
fnames_wholetext_filtered <- fnames_wholetext[fnames_wholetext_short %in% fnames_wholetext_short_filtered]

# textual collocation analysis
# set "india" as the target word for textual collocation analysis
coocs <- fnames_wholetext %>% 
  text_cooc("(?xi)  ^ india $")

# calculate collocation scores
scores_colloc <- assoc_scores(coocs)

# filter the collocation scores with a statistical criterion
top_scores_colloc <- scores_colloc %>% 
  filter(PMI >= 2 & G_signed >= 3.84)

# transfer the filtered collocation objects into dataframe
top_scores_colloc_df <- as_tibble(top_scores_colloc)

# transfer the obtained target freqlist and reference freqlist
# of collocation analysis into dataframes
coocs_ref_df <- as_tibble(coocs$ref_freqlist)
coocs_tar_df <- as_tibble(coocs$target_freqlist)

# compare the normalized frequency of high absolute frequency
# and signed G ranking words in textual collocation in target context 
# to that in reference context
words_to_check_colloc <- c("arabia", "indian", "ethiopia", "ganges", "rock-crystal", "hundred", "elephants", "megasthenes", "indians", "gem", "lustre", "stones", "identical", "'smaragdus", "indus", "gems", "gemstone", "pearls", "onesicritus", "mart")

filtered_top_scores_colloc <- coocs_tar_df %>%
  filter(type %in% words_to_check_colloc)

tar_nrm_freq_lookup1 <- setNames(coocs_tar_df$nrm_freq, coocs_tar_df$type)
nrm_freq_lookup1 <- setNames(coocs_ref_df$nrm_freq, coocs_ref_df$type)

results_colloc <- tibble(
  Word = filtered_top_scores_colloc$type,
  Target_Nrm_Frequency = tar_nrm_freq_lookup1[filtered_top_scores_colloc$type],
  Reference_Nrm_Frequency = nrm_freq_lookup1[filtered_top_scores_colloc$type]
) %>% 
  arrange(desc(Target_Nrm_Frequency)) %>% 
  kbl(col.names = c("Type", "Target_Nrm_Frequency", "Reference_Nrm_Frequency")) %>% 
  kable_minimal() %>% 
  scroll_box(height = "400px")

# compare the normalized frequency of high PMI ranking words
# in textual collocation in target context to that in reference context
words_to_check_colloc_pmi <- c("megasthenes", "thorn-bush", "carnelian", "obsidian", "cophes", "merchandize", "mart", "expeditions", "patala",   "arii", "ichthyophagi", "pursuits", "vermilion", "biggest", "onesicritus", "identical", "ganges", "engrave", "'smaragdus", "honey-coloured",   "reflects"
)

filtered_top_scores_colloc_pmi <- coocs_tar_df %>%
  filter(type %in% words_to_check_colloc_pmi)

results_colloc_pmi <- tibble(
  Word = filtered_top_scores_colloc_pmi$type,
  Target_Nrm_Frequency = tar_nrm_freq_lookup1[filtered_top_scores_colloc_pmi$type],
  Reference_Nrm_Frequency = nrm_freq_lookup1[filtered_top_scores_colloc_pmi$type]
) %>% 
  arrange(desc(Target_Nrm_Frequency)) %>%
  kbl(col.names = c("Type", "Target_Nrm_Frequency", "Reference_Nrm_Frequency")) %>% 
  kable_minimal() %>% 
  scroll_box(height = "400px")

# keyword analysis
# build frequency list for reference corpus
flist_ref <- fnames_wholetext_filtered %>%
  freqlist(re_token_transf_in = "'", 
           token_transf_out = "") 

# build frequency list for target corpus
flist_target <- fnames_indiatext %>% 
  freqlist(re_token_transf_in = "'", 
           token_transf_out = "")

# calculate keywords scores
scores_kw <- assoc_scores(flist_target, flist_ref)

# filter the keywords scores with a statistical criterion
top_scores_kw <- scores_kw %>% 
  filter(PMI >= 2 & G_signed >= 3.84)

# transfer the filtered keyword objects into dataframe
top_scores_kw_df <- as_tibble(top_scores_kw)


# transfer the freqlist of target and reference corpra
# of keyword analysis into dataframes
flist_ref_df <- as_tibble(flist_ref)
flist_target_df <- as_tibble(flist_target)

# compare the normalized frequency of high absolute frequency
# and signed G ranking words in keywords analysis in target corpus 
# to that in reference corpus
words_to_check <- c("india", "hundred", "stones", "arabia", "indian", "alexander",
                    "amber", "thousand", "elephants", "indus", "glass", "thence",
                    "ganges", "rock-crystal", "ethiopia", "indians")


filtered_top_scores <- flist_target_df %>%
  filter(type %in% words_to_check)

tar_nrm_freq_lookup <- setNames(flist_target_df$nrm_freq, flist_target_df$type)
nrm_freq_lookup <- setNames(flist_ref_df$nrm_freq, flist_ref_df$type)

results_keyword <- tibble(
  Word = filtered_top_scores$type,
  Target_Nrm_Frequency = tar_nrm_freq_lookup[filtered_top_scores$type],
  Reference_Nrm_Frequency = nrm_freq_lookup[filtered_top_scores$type]
) %>% 
  arrange(desc(Target_Nrm_Frequency)) %>%
  kbl(col.names = c("Type", "Target_Nrm_Frequency", "Reference_Nrm_Frequency")) %>% 
  kable_minimal() %>% 
  scroll_box(height = "400px")

# compare the normalized frequency of high PMI ranking words
# in keywords analysis in target corpus to that in reference corpus
words_to_check_pmi <- c("india", "ganges", "beryls", "indus", "taprobane",
                        "ichthyophagi", "megasthenes", "obsidian", "bdellium",
                        "agates", "callaina", "condensation", "expeditions",
                        "gerra", "jomanes", "nonius", "patala", "prasii", "alia",
                        "bactra", "carnelian", "ceylon", "cophes", "hypasis",
                        "merchandize", "pepper-tree", "sacae", "sandastros",
                        "thorn-bush")

filtered_top_scores_pmi <- top_scores_kw_df %>%
  filter(type %in% words_to_check_pmi)


results_keyword_pmi <- tibble(
  Word = filtered_top_scores_pmi$type,
  Target_Nrm_Frequency = tar_nrm_freq_lookup[filtered_top_scores_pmi$type],
  Reference_Nrm_Frequency = nrm_freq_lookup[filtered_top_scores_pmi$type]
)%>% 
  arrange(desc(Target_Nrm_Frequency)) %>%
  kbl(col.names = c("Type", "Target_Nrm_Frequency", "Reference_Nrm_Frequency")) %>% 
  kable_minimal() %>% 
  scroll_box(height = "400px")

# create a dataframe for explaination and classification of
# high PMI ranking keywords
keyword_PMI_list <- data.frame(
  Word = c("india", "indus", "ganges", "beryls", "taprobane", "ichthyophagi", "megasthenes", "obsidian", "bdellium", "agates", "callaina", "condensation", "expeditions", "gerra", "jomanes", "nonius", "patala", "prasii", "alia", "bactra", "carnelian", "ceylon", "cophes", "hypasis", "merchandize", "pepper-tree", "sacae", "sandastros", "thorn-bush"),
  Exp = c("name of the focused region of this study", "a major river in India", "a major river in India", "a type of gemstone", "island in Sri Lanka", "a group of people who primarily subsist on fish", "a Greek historian and diplomat", "a type of volcanic glass", "a fragrant resin obtained from certain trees", "a type of semiprecious gemstone", "pale green precious stone (lat)", "the process of vapor turning into a liquid state", "referring to the expedition of Alexander the Great", "war (lat)", "the most important of the affluents of the Ganges", "a Roman nomen gentile, gens or \"family name\"", "name of an ancient city at the mouth of the Indus River", "prase, green coloured gem", "by another / different way / route (lat)", "name of a city in Asia Minor", "a reddish-brown variety of chalcedony", "name of an island in Indian subcontinent", "a river that emerges in Hindu Kush mountains and empties into the Indus River", "a river in north India", "goods or commodities", "a tree that produces peppercorns", "historic Persian ethnic group", "a precious stone found in India and Arabia", "any of many thorny or spiny shrubs and bushes"),
  Category = c("region", "river", "river", "goods", "region", "people", "people", "goods", "goods", "goods", "goods", "activity", "activity", "activity", "river", "people", "region", "goods", "route", "region", "goods", "region", "river", "river", "goods", "plant", "people", "goods", "plant"),
  Remark = c("N/A", "N/A", "N/A", "gemstone", "N/A", "tribe", "referencing scholar", "other goods", "other goods", "gemstone", "gemstone", "producing activity", "N/A", "N/A", "N/A", "human name", "N/A", "gemstone", "N/A", "N/A", "gemstone", "N/A", "N/A", "N/A", "general goods", "origin of goods", "N/A", "gemstone", "N/A")
)

# check the classification distribution of 
# the high PMI ranking keywords
tag_counts <- keyword_PMI_list %>%
  count(Category) %>%
  arrange(desc(n))

