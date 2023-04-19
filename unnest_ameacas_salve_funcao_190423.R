### sistematiza ameaças listadas nas fichas das espécies alvo do PAN Sudeste
### 16-03-2023
### atualizado em 19-04-2023

# instala pacotes ----------
pacotes <- c("readxl", "dplyr", "tibble", "stringr", "tidyr", "tibble",
             "writexl")

to_install <- !pacotes %in% installed.packages()
if(any(to_install)) {
  install.packages(pacotes[to_install])
}
# devtools::install_github("ipeaGIT/geobr", subdir = "r-package")

# carrega pacotes
library(readxl)
library(dplyr)



unnest_ameacas <- function(df) { 
  ## df é uma planilha que pode ter varias colunas, mas tem que ter uma com um
  ## id pra especie (id_spp), com o nome da especie (especie) e uma coluna
  ## com as ameaças (ameaca)
  
  ameacas <- df %>% 
    dplyr::select(id_spp, especie, ameaca)
  
  ameacas <- ameacas %>% 
    dplyr::mutate(
      ameaca_sep = 
        stringr::str_split(ameaca, pattern = "(?=\n[0-9]+ -)", 
                           n = Inf, simplify = FALSE)
      # str_split(ameaca, pattern = "(?=\r\n[0-9]+ -)", 
      #           n = Inf, simplify = FALSE)
    ) %>%
    tidyr::unnest(cols = ameaca_sep)
  
  
  ameacas$ameaca_sep <- stringr::str_replace_all(ameacas$ameaca_sep, 
                                                 pattern = "\r\n", 
                                                 replacement = " ")
  
  ameacas$ameaca_sep <- stringr::str_replace_all(ameacas$ameaca_sep, 
                                                 pattern = "\n",
                                                 replacement = " ")
  
  
  
  
  
  
  ameacas <- ameacas %>% 
    dplyr::mutate(
      ameaca_sep = 
        stringr::str_split(ameaca_sep, pattern = "(?= [0-9]+.[0-9]+ -)", 
                           n = Inf, simplify = FALSE)
    ) %>%
    tidyr::unnest(cols = ameaca_sep)
  
  
  ameacas$ameaca_sep <- stringr::str_trim(ameacas$ameaca_sep)
  # ameacas$ameaca_sep
  
  titulos <- ameacas %>%
    dplyr::filter(stringr::str_detect(ameaca_sep, pattern = "^\\d+ -")) %>% 
    dplyr::pull(ameaca_sep) %>% 
    unique
  
  titulos_df <- dplyr::tibble(titulo = titulos,
                       numero = as.numeric(stringr::str_extract(titulos, "\\d+"))) %>% 
    dplyr::arrange(numero)
  
  
  
  nao_exclui <- ameacas %>% 
    dplyr::group_by(especie) %>% 
    dplyr::summarise(n=n()) %>%
    dplyr::filter(n == 1) %>% 
    dplyr::pull(especie)
  
  
  
  
  ameacas <- ameacas %>%
    dplyr::filter(especie %in% nao_exclui |
             stringr::str_detect(ameaca_sep, pattern = "([0-9].. - )"))
  
  
  
  ameacas$ameaca_sep <- stringr::str_replace_all(ameacas$ameaca_sep, 
                                                 pattern = "(?= [0-9])", 
                                                 replacement = ";")
  
  
  ameacas <- ameacas %>% 
    dplyr::mutate(numero = as.numeric(
      stringr::str_extract(ameacas$ameaca_sep, "^\\d+"))
      ) %>% 
    dplyr::left_join(titulos_df, by = "numero") %>% 
    dplyr::select(-numero)
  
  
  # 
  # 
  # ameacas %>% 
  #   group_by(ameaca_sep) %>% 
  #   summarise(n=n()) %>% 
  #   arrange(-n)
  # 
  # ameacas$ameaca_sep %>% 
  #   unique %>% 
  #   sort
  
  
  return(ameacas)
}

# pasta onde está a planilha do salve
pasta <- "C:/SIG/RAN_homeoffice"
# nome da planilha do salve
nome_arquivo <- "salve_exportacao_taxon_analitico_16_03_2023_06_46_36.xlsx"

ameacas2 <- read_xlsx(paste0(pasta, "/", nome_arquivo))
colnames(ameacas2)
ameacas2 <- ameacas2 %>% 
  dplyr::select(especie = `Táxon`,
         ameaca = `Ameaça`) %>% 
  tibble::rownames_to_column(var = "id_spp")


ameacas3 <- unnest_ameacas(df = ameacas2)

ameacas3 %>% 
  dplyr::group_by(ameaca_sep) %>% 
  summarise(n = n()) %>% 
  arrange(-n)



# pasta onde quer salvar:
pasta_salvar <- "C:/Users/bruna/ICMBio/NGEO - General/PAN/PAN_SE_AEs_Bruna/2ciclo/dados_especies"
# nome do arquivo
arquivo_salvar <- "ameacas3.xlsx"

writexl::write_xlsx(ameacas3, paste0(pasta_salvar, "/", arquivo_salvar))

