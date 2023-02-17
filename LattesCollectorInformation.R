setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(getLattes)
library(GetLattesData)
library(readxl)
library(dplyr)
library(ggplot2)
library(readr)
# library(rapp)
Qualis <- read_excel(file.choose()) # for instance classificacoes_publicadas_psicologia_2022_1672761176271.xlsx 
# Change the issn format removing the -
Qualis["issn"] <- sub('-','',Qualis$ISSN)
# Extract the issn 
QualisISSN <- Qualis[3:4]
# Define the year of onset of analysis
Year <- 2019


# Read files XLM exported from lattes.cnpq.br in zip format (it's not necessary to extract the zip files)
Arquivos <- list.files(pattern="*.zip")
# create empty databases 
Prod_artigos <- NULL
Prod_livros <- NULL
Prod_capitulos <- NULL
Prod_NoPrelo <- NULL
Prod_DoutoradoConcluido <- NULL
Prod_DoutoradoEmAndamento <- NULL
Prod_MestradoConcluido <- NULL
Prod_MestradoEmAndamento <- NULL
Prod_ICConcluido <- NULL
Prod_ICEmAndamento <- NULL



for (i in c(1:length(Arquivos)))  {
  curriculo <- xml2::read_xml(Arquivos[i])
  
  
  # extract data from articles
  Artigos <- getArtigosPublicados(curriculo)
  DadosGerais <-  getDadosGerais(curriculo)
  # Identifies and score the qualis of the journal
  if (nrow(Artigos) != 0 ){Artigos$ano_do_artigo <- as.numeric(Artigos$ano_do_artigo)}
  if (nrow(Artigos) != 0 ){
    Artigos <- subset(Artigos, ano_do_artigo>=Year)
    Artigos <- left_join(Artigos,QualisISSN)
    Artigos$Estrato[is.na(Artigos$Estrato)] <- "Sem Qualis"
    Artigos["Pontuacao"] <- 0
    Artigos$Pontuacao[Artigos$Estrato=="A1"] <- 100
    Artigos$Pontuacao[Artigos$Estrato=="A2"] <- 87.5
    Artigos$Pontuacao[Artigos$Estrato=="A3"] <- 75
    Artigos$Pontuacao[Artigos$Estrato=="A4"] <- 62.5
    Artigos$Pontuacao[Artigos$Estrato=="B1"] <- 50
    Artigos$Pontuacao[Artigos$Estrato=="B2"] <- 37.5
    Artigos$Pontuacao[Artigos$Estrato=="B3"] <- 25
    Artigos$Pontuacao[Artigos$Estrato=="B4"] <- 12.5
    # write a tsv file whit the list of articles by professor
    write_delim(Artigos, paste("Artigos_", DadosGerais$nome_completo, ".tsv", sep=""), delim="\t")
  }
  if (nrow(Artigos) != 0 ){Artigos["Professor"] <- DadosGerais$nome_completo}
  
  
  # extract book chapters
  Capitulos <- getCapitulosLivros(curriculo)
  if (nrow(Capitulos) != 0 ){
    Capitulos <- subset(Capitulos, ano>=Year)
    # write a tsv file whit the list of book chapters by professor
    write_delim(Capitulos, paste("Capitulos_", DadosGerais$nome_completo, ".tsv", sep=""), delim="\t")
    }
  
  if (nrow(Capitulos) != 0 ){Capitulos["Professor"] <- DadosGerais$nome_completo}  
 

  # extract books 
  Livros <- getLivrosPublicados(curriculo)
  if (nrow(Livros) != 0 ){
    Livros <- subset(Livros, ano>=Year)
    # write a tsv file whit the list of books by professor
    write_delim(Livros, paste("Livros_", DadosGerais$nome_completo, ".tsv", sep=""),  delim="\t")
    }
  if (nrow(Livros) != 0 ){Livros["Professor"] <- DadosGerais$nome_completo}
  
  
  
  # extract in press articles
  
  dados_basicos <- 
    xml2::xml_find_all(curriculo, ".//ARTIGOS-ACEITOS-PARA-PUBLICACAO") |>
    purrr::map(~ xml2::xml_find_all(., ".//DADOS-BASICOS-DO-ARTIGO")) |>
    purrr::map(~ xml2::xml_attrs(.)) |>
    purrr::map(~ dplyr::bind_rows(.)) |>
    purrr::map(~ janitor::clean_names(.)) 
  
  detalhamento <- 
    xml2::xml_find_all(curriculo, ".//ARTIGOS-ACEITOS-PARA-PUBLICACAO") |>
    purrr::map(~ xml2::xml_find_all(., ".//DETALHAMENTO-DO-ARTIGO")) |>
    purrr::map(~ xml2::xml_attrs(.)) |>
    purrr::map(~ dplyr::bind_rows(.)) |>
    purrr::map(~ janitor::clean_names(.)) 
  
  NoPrelo <- purrr::map2(dados_basicos, detalhamento, dplyr::bind_cols) |>
    dplyr::bind_rows() |>
    dplyr::mutate(id = getId(curriculo)) 
  
  
  if (nrow(NoPrelo) != 0 ){
    # Identifies and score the qualis of the journal
    NoPrelo <- left_join(NoPrelo,QualisISSN)
    NoPrelo$Estrato[is.na(NoPrelo$Estrato)] <- "Sem Qualis"
    NoPrelo["Pontuacao"] <- 0
    NoPrelo$Pontuacao[NoPrelo$Estrato=="A1"] <- 100
    NoPrelo$Pontuacao[NoPrelo$Estrato=="A2"] <- 87.5
    NoPrelo$Pontuacao[NoPrelo$Estrato=="A3"] <- 75
    NoPrelo$Pontuacao[NoPrelo$Estrato=="A4"] <- 62.5
    NoPrelo$Pontuacao[NoPrelo$Estrato=="B1"] <- 50
    NoPrelo$Pontuacao[NoPrelo$Estrato=="B2"] <- 37.5
    NoPrelo$Pontuacao[NoPrelo$Estrato=="B3"] <- 25
    NoPrelo$Pontuacao[NoPrelo$Estrato=="B4"] <- 12.5
    # write a tsv file whit the list of in press articles by professor
    write_delim(NoPrelo, paste("ArtigosAceitosParaPublicacao_", DadosGerais$nome_completo, ".tsv", sep=""),  delim="\t")
  }  
  
  
  # extract doctoral orientations completed
  
  Doutores <-   getOrientacoesDoutorado(curriculo)
  
  if (nrow(Doutores) != 0 ){
    Doutores <- subset(Doutores, ano>=Year)
    # write a tsv file whit the list of formed phd students by professor
    write_delim(Doutores, paste("OrientacoesConcluidasDoutorado_", DadosGerais$nome_completo, ".tsv", sep=""),  delim="\t")
  }  

  
  # extract master orientations completed
  
  Mestres <-   getOrientacoesMestrado(curriculo)
  if (nrow(Mestres) != 0 ){
    Mestres <- subset(Mestres, ano>=Year)
    # write a tsv file whit the list of formed master students by professor
    write_delim(Mestres, paste("OrientacoesConcluidasMestrado_", DadosGerais$nome_completo, ".tsv", sep=""),  delim="\t")
  }  
  
  # extract IC orientations completed
  
  dados_basicos <- 
    xml2::xml_find_all(curriculo, ".//OUTRAS-ORIENTACOES-CONCLUIDAS") |>
    purrr::map(~ xml2::xml_find_all(., ".//DADOS-BASICOS-DE-OUTRAS-ORIENTACOES-CONCLUIDAS ")) |>
    purrr::map(~ xml2::xml_attrs(.)) |>
    purrr::map(~ dplyr::bind_rows(.)) |>
    purrr::map(~ janitor::clean_names(.)) 
  
  detalhamento <- 
    xml2::xml_find_all(curriculo, ".//OUTRAS-ORIENTACOES-CONCLUIDAS") |>
    purrr::map(~ xml2::xml_find_all(., ".//DETALHAMENTO-DE-OUTRAS-ORIENTACOES-CONCLUIDAS")) |>
    purrr::map(~ xml2::xml_attrs(.)) |>
    purrr::map(~ dplyr::bind_rows(.)) |>
    purrr::map(~ janitor::clean_names(.)) 
  
  Graduados <- purrr::map2(dados_basicos, detalhamento, dplyr::bind_cols) |>
    dplyr::bind_rows() |>
    dplyr::mutate(id = getId(curriculo)) 
  
  
  if (nrow(Graduados) != 0 ){
    Graduados <- subset(Graduados, ano>=Year & natureza=="INICIACAO_CIENTIFICA")
    # write a tsv file whit the list of formed IC students by professor
    write_delim(Graduados, paste("OrientacoesConcluidasIC_", DadosGerais$nome_completo, ".tsv", sep=""),  delim="\t")
  }  
  
  # extract master orientations in progress
  
  dados_basicos <- 
    xml2::xml_find_all(curriculo, ".//ORIENTACAO-EM-ANDAMENTO-DE-MESTRADO") |>
    purrr::map(~ xml2::xml_find_all(., ".//DADOS-BASICOS-DA-ORIENTACAO-EM-ANDAMENTO-DE-MESTRADO")) |>
    purrr::map(~ xml2::xml_attrs(.)) |>
    purrr::map(~ dplyr::bind_rows(.)) |>
    purrr::map(~ janitor::clean_names(.)) 
  
  detalhamento <- 
    xml2::xml_find_all(curriculo, ".//ORIENTACAO-EM-ANDAMENTO-DE-MESTRADO") |>
    purrr::map(~ xml2::xml_find_all(., ".//DETALHAMENTO-DA-ORIENTACAO-EM-ANDAMENTO-DE-MESTRADO")) |>
    purrr::map(~ xml2::xml_attrs(.)) |>
    purrr::map(~ dplyr::bind_rows(.)) |>
    purrr::map(~ janitor::clean_names(.)) 
  
  AndamentoM <- purrr::map2(dados_basicos, detalhamento, dplyr::bind_cols) |>
    dplyr::bind_rows() |>
    dplyr::mutate(id = getId(curriculo)) 
  
  
  if (nrow(AndamentoM) != 0 ){
    AndamentoM <- subset(AndamentoM, ano>=Year)
    # write a tsv file whit the list of current master students by professor
    write_delim(AndamentoM, paste("OrientacoesEmAndamentoMestrado_", DadosGerais$nome_completo, ".tsv", sep=""),  delim="\t")
  }  
  
  #  extract phD orientations in progress
  
  dados_basicos <- 
    xml2::xml_find_all(curriculo, ".//ORIENTACAO-EM-ANDAMENTO-DE-DOUTORADO") |>
    purrr::map(~ xml2::xml_find_all(., ".//DADOS-BASICOS-DA-ORIENTACAO-EM-ANDAMENTO-DE-DOUTORADO")) |>
    purrr::map(~ xml2::xml_attrs(.)) |>
    purrr::map(~ dplyr::bind_rows(.)) |>
    purrr::map(~ janitor::clean_names(.)) 
  
  detalhamento <- 
    xml2::xml_find_all(curriculo, ".//ORIENTACAO-EM-ANDAMENTO-DE-DOUTORADO") |>
    purrr::map(~ xml2::xml_find_all(., ".//DETALHAMENTO-DA-ORIENTACAO-EM-ANDAMENTO-DE-DOUTORADO")) |>
    purrr::map(~ xml2::xml_attrs(.)) |>
    purrr::map(~ dplyr::bind_rows(.)) |>
    purrr::map(~ janitor::clean_names(.)) 
  
  AndamentoD <- purrr::map2(dados_basicos, detalhamento, dplyr::bind_cols) |>
    dplyr::bind_rows() |>
    dplyr::mutate(id = getId(curriculo)) 
  
  
  if (nrow(AndamentoD) != 0 ){
    AndamentoD <- subset(AndamentoD, ano>=Year )
    # write a tsv file whit the list of current phD students by professor
    write_delim(AndamentoD, paste("OrientacoesEmAndamentoDoutorado_", DadosGerais$nome_completo, ".tsv", sep=""),  delim="\t")
  }  
  
  #  extract IC orientations in progress
  
  dados_basicos <- 
    xml2::xml_find_all(curriculo, ".//ORIENTACAO-EM-ANDAMENTO-DE-INICIACAO-CIENTIFICA") |>
    purrr::map(~ xml2::xml_find_all(., ".//DADOS-BASICOS-DA-ORIENTACAO-EM-ANDAMENTO-DE-INICIACAO-CIENTIFICA")) |>
    purrr::map(~ xml2::xml_attrs(.)) |>
    purrr::map(~ dplyr::bind_rows(.)) |>
    purrr::map(~ janitor::clean_names(.)) 
  
  detalhamento <- 
    xml2::xml_find_all(curriculo, ".//ORIENTACAO-EM-ANDAMENTO-DE-INICIACAO-CIENTIFICA") |>
    purrr::map(~ xml2::xml_find_all(., ".//DETALHAMENTO-DA-ORIENTACAO-EM-ANDAMENTO-DE-INICIACAO-CIENTIFICA")) |>
    purrr::map(~ xml2::xml_attrs(.)) |>
    purrr::map(~ dplyr::bind_rows(.)) |>
    purrr::map(~ janitor::clean_names(.)) 
  
  AndamentoIC <- purrr::map2(dados_basicos, detalhamento, dplyr::bind_cols) |>
    dplyr::bind_rows() |>
    dplyr::mutate(id = getId(curriculo)) 
  
  
  if (nrow(AndamentoIC) != 0 ){
    AndamentoIC <- subset(AndamentoIC, ano>=Year )
    # write a tsv file whit the list of current IC students by professor
    write_delim(AndamentoIC, paste("OrientacoesEmAndamentoIC_", DadosGerais$nome_completo, ".tsv", sep=""),  delim="\t")
  }  
  
  
  
  if (nrow(Artigos) != 0 ){ Prod_artigos <-  dplyr::bind_rows(Prod_artigos,Artigos) }
  if (nrow(Capitulos) != 0 ){ Prod_capitulos <-  dplyr::bind_rows(Prod_capitulos,Capitulos) }
  if (nrow(Livros) != 0 ){ Prod_livros <-  dplyr::bind_rows(Prod_livros,Livros) }
  if (nrow(NoPrelo) != 0 ){ Prod_NoPrelo <-  dplyr::bind_rows(Prod_NoPrelo,NoPrelo) }
  if (nrow(Doutores) != 0 ){ Prod_DoutoradoConcluido <-  dplyr::bind_rows(Prod_DoutoradoConcluido,Doutores) }
  if (nrow(Mestres) != 0 ){ Prod_MestradoConcluido <-  dplyr::bind_rows(Prod_MestradoConcluido,Mestres) }
  if (nrow(Graduados) != 0 ){ Prod_ICConcluido <-  dplyr::bind_rows(Prod_ICConcluido,Graduados) }
  if (nrow(AndamentoD) != 0 ){ Prod_DoutoradoEmAndamento <-  dplyr::bind_rows(Prod_DoutoradoEmAndamento,AndamentoD) }
  if (nrow(AndamentoM) != 0 ){ Prod_MestradoEmAndamento <-  dplyr::bind_rows(Prod_MestradoEmAndamento,AndamentoM) }
  if (nrow(AndamentoIC) != 0 ){ Prod_ICEmAndamento <-  dplyr::bind_rows(Prod_ICEmAndamento,AndamentoIC) }
}


