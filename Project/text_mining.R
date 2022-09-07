library(tidyverse)  # Datenjudo
library(stringr)  # Textverarbeitung
library(tidytext)  # Textmining
library(lsa)  # Stopwörter 
library(SnowballC)  # Wörter trunkieren
library(wordcloud)  # Wordcloud anzeigen
library(pdftools)
library("readxl")
library("dplyr")
if(!require("quanteda")) {install.packages("quanteda"); library("quanteda")}

example_text_raw <- read_xlsx("Text_Mining_Beispiel.xlsx",col_names =  TRUE)
example_text <- read_xlsx("Text_Mining_Beispiel.xlsx",col_names =  TRUE)

example_text  %>% 
  unnest_tokens(output = token, input = Beschreibung) %>% 
  dplyr::filter(str_detect(token, "[a-z]")) -> example_text               

data(stopwords_de, package = "lsa")

stopwords_de <- data_frame(word = stopwords_de)

stopwords_de <- stopwords_de %>% 
  rename(token = word)  

example_text   %>% 
  anti_join(stopwords_de) -> example_text_no_stop

example_text_no_stop %>% 
  count(token, sort = TRUE) -> example_text_count


example_text_no_stop %>% 
  mutate(token_stem = wordStem(.$token, language = "de")) %>% 
  count(token_stem, sort = TRUE) -> example_text_count_stemmed

example_text_count_stemmed %>% 
  top_n(10) %>% 
  knitr::kable(caption = "Die häufigsten Wörter - mit 'stemming'")


example_text_count_stemmed %>% 
  top_n(10) %>% 
  ggplot() +
  aes(x = reorder(token_stem, n), y = n) +
  geom_col() + 
  labs(title = "mit Trunkierung") +
  coord_flip() -> p1

corp_immig <- corpus(example_text_raw$Beschreibung)
  
  
test.lexikon <- dictionary(list(posititive.begriffe = c("hell","aufgeräumt","sauber"," gepflegt"), negative.begriffe = c("trauer", "wut", "dunkelheit")))
test.lexikon
meine.dfm.sentiment <- dfm(corp_immig , dictionary = test.lexikon )
meine.dfm.sentiment
