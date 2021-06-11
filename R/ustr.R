library(tidyverse)
library(pdftools)
library(here)

pdf_file <- here("data-raw/ustr_editado.pdf")

pdf_data <- pdftools::pdf_data(pdf_file, font_info = TRUE)

data <- pdf_data %>% 
  bind_rows(.id = "id") %>% 
  mutate(font_name = case_when(font_name == "AAAAAF+TimesNewRomanPSMT" ~ "AAAAAB+TimesNewRomanPSMT",
                               TRUE ~ font_name))

italico <- "AAAAAH+TimesNewRomanPS-ItalicMT"

bold_pequeno <- "AAAAAE+TimesNewRomanPS-BoldMT"

#bold_pais identifica o nome do país

bold_pais <- "AAAAAD+TimesNewRomanPS-BoldMT"

df <- data %>% 
  group_by(id, y, font_name) %>% 
  mutate(italico = paste0(text, collapse = " ")) %>% 
  distinct(italico) %>%
  ungroup() %>% 
  mutate(grupo = case_when(font_name != lag(font_name) ~ "diferente",
                           TRUE ~ "igual")) %>% 
  mutate(grupo = case_when(lag(str_detect(italico, "[:upper:]+$")) ~ "diferente",
                           TRUE ~ grupo)) %>% 
  mutate(grupo = case_when(str_detect(grupo, "diferente") ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(grupo = cumsum(grupo)) %>% 
  group_by(grupo) %>% 
  mutate(total = paste0(italico, collapse = " ")) %>% 
  group_by(font_name, grupo) %>% 
  mutate(titulo = case_when(font_name == italico | font_name == bold_pequeno ~ TRUE,
                            TRUE ~ FALSE)) %>% 
  distinct(total, titulo) %>% 
  ungroup() %>% 
  mutate(child = lead(total)) %>% 
  mutate(pais = case_when(font_name == bold_pais ~ 1,
                          TRUE ~ 0)) %>% 
  mutate(pais = cumsum(pais))

paises <-  data %>% 
  filter(font_name == bold_pais) %>%
  group_by(id, y, font_name) %>% 
  mutate(italico = paste0(text, collapse = " ")) %>% 
  distinct(italico) %>% 
  ungroup() %>% 
  mutate(id = row_number()) %>% 
  select(id, italico) %>% 
  rename(pais = italico)


df_long <- df %>% 
  filter(titulo == T) %>% 
  mutate(total = str_squish(total)) %>% 
  filter(str_detect(total, ":$", negate = T)) %>% 
  select(-c(font_name, titulo, grupo)) %>% 
  rename(id = pais) %>% 
  left_join(paises)

write_csv(df_long, here("data/ustr.csv"))



