### separa arquivos vetoriais tendo uma coluna da tabela de atributos como criterio
### funciona com spatialdataframe (sp package) e com spatial feature (sf package)
### criado por Bruna Arbo Meneses
### 28/04/2022

## carrega pacotes ---------------

pacotes <- c("sf", "sp", "rgdal", "dplyr", "stringr", "spdplyr")

to_install <- !pacotes %in% installed.packages()
if(any(to_install)) {
  install.packages(pacotes[to_install])
}

library(sf)
library(sp)
library(rgdal)
library(dplyr)
library(stringr)
library(spdplyr)

## funcao --------------

split_by_atrributes <- function(arquivo_vetorial, nome_coluna, pasta) {
  ### se não definir uma pasta, salva no working diretory - getwd()
  if (missing(pasta)) {
    pasta <- getwd()
  }
  
  start <- Sys.time()
  nome_coluna <- sym(nome_coluna)
  
  if (class(arquivo_vetorial)[1] == "sf") {
    ### if class = sf
    message("sf")

    # define os valores unicos que darão nome aos novos arquivos
    unique <- arquivo_vetorial %>% 
      select(all_of(nome_coluna)) %>% 
      pull(nome_coluna) %>% 
      unique
    
    # salva o novo shape
    for (i in 1:length(unique)) {
      pontos <- arquivo_vetorial %>% 
        filter(!!nome_coluna == unique[i])
      
      nome_arquivo <- paste0(str_replace_all(unique[i], " ", "_"), ".shp")
      message(i, "/", length(unique), " - ", nome_arquivo, " -- ", nrow(pontos), 
              " features")
      
      write_sf(pontos, paste0(pasta, "/", nome_arquivo),  delete_layer = TRUE)
      
    } 
  } else {
    ### if class = sp
    message(classe <- class(arquivo_vetorial)[1])
    
    # define os valores unicos que darão nome aos novos arquivos
    unique <- arquivo_vetorial@data %>% 
      select(all_of(nome_coluna)) %>% 
      unique %>% 
      pull(nome_coluna)
    
    for (i in 1:length(unique)) {
      
      pontos <- arquivo_vetorial %>% 
        filter(!!nome_coluna == unique[i])
      
      nome_arquivo <- paste0(str_replace_all(unique[i], " ", "_"), ".shp")
      message(i, "/", length(unique), " - ", nome_arquivo, " -- ", nrow(pontos),
              " features")
      
      writeOGR(pontos, dsn = pasta, unique[i],
               driver = "ESRI Shapefile", 
               overwrite_layer=TRUE)
      
    } 
  }
  message("\nfim! :)")
  print(Sys.time() - start)
} 
