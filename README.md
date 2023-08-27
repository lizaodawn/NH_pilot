This repository contains an R project with the following subfolders and files around the paper titled "Collocation and keyword analysis on India-related text of Natural History":

### Data
- geotext_whole.csv: a comma-separated file with 8876 rows and 9 columns containing all place annotations with the textual content of *Natural History* scraped from TOPOSText.
- geotext_indianregion.csv: a comma-separated file with 229 rows and 9 columns containing all India place annotations with the textual content of *Natural History* scraped from TOPOSText.
- NH_wholetext: corpus folder containing 3493 files of all textual content in *Natural History*, each file consists of a single paragraph with letters in lowercase
- NH_geotext_india: corpus folder containing 146 files of text mentioning place names of Indian region in *Natural History*, each file consists of a single paragraph with letters in lowercase
### Scripts
- NH_pilot_exploration.R: the R script that reads the data and generates the matrix for collocation and keyword analysis
- Natural History_corpus.ipynb: the code for information scraping from TOPOSText and corpus exporting
### Quarto
- paper_quarto.qmd: the source Quarto file of the paper
- paper_quarto.html: the output in HTML format
### Helpers
- references.bib: the bibtex file for literature references
- packages.bib: the bibtex file for packages, written by write_bib() in sample-paper.qmd (after creating a blank file.)
- unified-style-sheet-for-linguistics.csl: the stylesheet for references
