library(tidyverse)
library(pdftools)
library(here)

pdf_file <- here("data-raw/ustr_algeria.pdf")

pdf_data <- pdftools::pdf_data(pdf_file, font_info = TRUE)

data <- pdf_data %>% 
  bind_rows(.id = "id") %>% 
  mutate(font_name = case_when(font_name == "AAAAAF+TimesNewRomanPSMT" ~ "AAAAAB+TimesNewRomanPSMT",
                               TRUE ~ font_name))


italico <- "AAAAAH+TimesNewRomanPS-ItalicMT"

bold_pequeno <- "AAAAAE+TimesNewRomanPS-BoldMT"

#bold_pais identifica o nome do país

bold_pais <- "AAAAAD+TimesNewRomanPS-BoldMT"

# space == FALSE quer dizer fim de linha

# Identificar aquilo que está em bold e em itálico

# Bold

## bold pequeno

titulos_bold <- data %>% 
  filter(font_name == bold_pequeno) %>% 
  group_by(id, y) %>% 
  mutate(bold = paste0(text, collapse = " ")) %>% 
  distinct(bold) %>% 
  pull(bold)

# Itálico

titulos_italico <- data %>% 
  filter(font_name == italico) %>% 
  group_by(id, y) %>% 
  mutate(italico = paste0(text, collapse = " ")) %>% 
  distinct(italico) %>% 
  pull(italico)

# Tentando juntar tudo

df <- data %>% 
  # filter(font_name == italico) %>% 
  group_by(id, y, font_name) %>% 
  mutate(italico = paste0(text, collapse = " ")) %>% 
  distinct(italico) %>%
  ungroup() %>% 
  mutate(grupo = case_when(font_name != lag(font_name) ~ "diferente",
                           TRUE ~ "igual")) %>% 
  mutate(grupo = case_when(lag(str_detect(italico, "[:upper:]+$")) ~ "diferente",
                           TRUE ~ grupo)) %>% 
  # View()
  mutate(grupo = case_when(str_detect(grupo, "diferente") ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(grupo = cumsum(grupo)) %>% 
  group_by(grupo) %>% 
  mutate(total = paste0(italico, collapse = " ")) %>% 
  group_by(font_name, grupo) %>% 
  mutate(titulo = case_when(font_name == italico | font_name == bold_pequeno ~ TRUE,
                            TRUE ~ FALSE)) %>% 
  # View()
  distinct(total, titulo) %>% 
  ungroup() %>% 
  mutate(child = lead(total))

df %>% 
  filter(titulo == T) %>% 
  select(-c(font_name, titulo, grupo)) %>% 
  pivot_wider(names_from = total, values_from = child) %>% 
  View()



# Dois países -------------------------------------------------------------



pdf_file <- here("data-raw/ustr2.pdf")

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
  # filter(font_name == italico) %>% 
  group_by(id, y, font_name) %>% 
  mutate(italico = paste0(text, collapse = " ")) %>% 
  distinct(italico) %>%
  ungroup() %>% 
  mutate(grupo = case_when(font_name != lag(font_name) ~ "diferente",
                           TRUE ~ "igual")) %>% 
  mutate(grupo = case_when(lag(str_detect(italico, "[:upper:]+$")) ~ "diferente",
                           TRUE ~ grupo)) %>% 
  # View()
  mutate(grupo = case_when(str_detect(grupo, "diferente") ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(grupo = cumsum(grupo)) %>% 
  group_by(grupo) %>% 
  mutate(total = paste0(italico, collapse = " ")) %>% 
  group_by(font_name, grupo) %>% 
  mutate(titulo = case_when(font_name == italico | font_name == bold_pequeno ~ TRUE,
                            TRUE ~ FALSE)) %>% 
  # View()
  distinct(total, titulo) %>% 
  ungroup() %>% 
  mutate(child = lead(total)) %>% 
  mutate(pais = case_when(font_name == bold_pais ~ 1,
                          TRUE ~ 0)) %>% 
  mutate(pais = cumsum(pais))


df %>% 
  filter(titulo == T) %>% 
  select(-c(font_name, titulo, grupo)) %>% 
  pivot_wider(names_from = total, values_from = child) %>% 
  View()





