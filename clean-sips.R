## Load packages ===========================================================
library(readxl)
library(tidyverse)
library(plyr)
library(stringr)
library(survey)
library(mice) # missing imputation
library(VIM) # visualizing imputed data
library(FactoMineR)
library(SciencesPo)
library(reshape2)
library(data.table)
library(janitor)
library(googlesheets)
# library(factoextra)
# library(gridExtra)

options(max.print = 500, scipen = 999)

## Load and join data ======================================================
setwd() # choose your working directory!
sips <- read.csv2("SIPS Valores.csv", stringsAsFactors = F, na.strings = c(888, 999, 777))
sips_sexo <- read.delim("SIPS_sexo.csv")

sips <- sips %>%
    tbl_df %>%
    setNames(tolower(names(.))) %>%
    left_join(., sips_sexo) %>%
    select(pesquisa:municipio, sexo, q1:q10.2) %>% 
    # save(sips, file = "sips_completo.Rda")

rm(sips_sexo)

egp <- read.csv2("ocupacao.txt", col.names = c("q9", rep("NULL", 4), "cd_egp"), 
                 colClasses = c("character", rep("NULL", 4), "character"))

## Correct "municipio" colum and join data with sample design data =========
sips_plano <- read_excel("SIPS1112PESO.xlsx")
sips_plano <- sips_plano %>% 
    setNames(tolower(names(.))) %>%
    dplyr::mutate(municipio = str_to_title(municipio),
           municipio = str_replace_all(municipio, "Acara", "Acará"),
           municipio = str_replace_all(municipio, "Carajas", "Carajás"),
           municipio = str_replace_all(municipio, "Muana", "Muaná"),
           municipio = str_replace_all(municipio, "Luis", "Luís"),
           municipio = str_replace_all(municipio, "Espirito Santo", "Espírito Santo"),
           municipio = str_replace_all(municipio, "São Felix", "São Félix"),
           municipio = str_replace_all(municipio, "Gloria Do Goitá", "Glória Do Goitá"),
           municipio = str_replace_all(municipio, "Riancho Das Almas", "Riacho Das Almas"),
           municipio = str_replace_all(municipio, "Aracajú", "Aracaju"),
           municipio = str_replace_all(municipio, "Monte Claros", "Montes Claros"),
           municipio = str_replace_all(municipio, "Sabara", "Sabará"),
           municipio = str_replace_all(municipio, "Cachoeiro Do", "Cachoeiro De"),
           municipio = str_replace_all(municipio, "Campos Do Goytacazes", "Campos Dos Goytacazes"),
           municipio = str_replace_all(municipio, "Lençois Paulista", "Lençóis Paulista"),
           municipio = str_replace_all(municipio, "Orlandia", "Orlândia"),
           municipio = str_replace_all(municipio, "Marilia", "Marília"),
           municipio = str_replace_all(municipio, "Pindamanhangaba", "Pindamonhangaba"),
           municipio = str_replace_all(municipio, "Santa Barbara", "Santa Bárbara"),
           municipio = str_replace_all(municipio, "Antonio Carlos", "Antônio Carlos"),
           municipio = str_replace_all(municipio, "Marilia", "Marília"),
           municipio = str_replace_all(municipio, "Blumenal", "Blumenau"),
           municipio = str_replace_all(municipio, "São Miguel D'oeste", "São Miguel Do Oeste"),
           municipio = str_replace_all(municipio, "Palmeiras De Goias", "Palmeiras De Goiás"),
           municipio = str_replace_all(municipio, "Taquaral De Goias", "Taquaral De Goiás"),
           municipio = str_replace_all(municipio, "Ceara-Mirim", "Ceará-Mirim"),
           municipio = str_replace_all(municipio, "Paraiso Do Tocantins", "Paraíso Do Tocantins"),
           municipio = str_replace_all(municipio, "Santo Antonio De Jesus", "Santo Antônio De Jesus")) %>%
    select(municipio, peso)
    
sips <- sips %>%
    dplyr::mutate(municipio = str_replace_all(municipio, "rio", "Rio"),
                  municipio = str_replace_all(municipio, "boa", "Boa"),
                  municipio = str_replace_all(municipio, "são", "São"),
                  municipio = str_replace_all(municipio, "foz", "Foz"),
                  municipio = str_replace_all(municipio, "mirim", "Mirim"),
                  municipio = str_replace_all(municipio, "Doeste", "d'Oeste"),
                  municipio = str_replace_all(municipio, "sai", "Sai"),
                  municipio = str_replace_all(municipio, "Brasilia", "Brasília"),
                  municipio = str_to_title(municipio),
                  pessoa = pesquisa,
                  q0 = sexo) %>%
    left_join(., sips_plano) %>%
    select(regiao:municipio, pessoa, peso, q0, q1:q10.2)
    
save(sips, file = "tb_sips2012.Rda")

## Missing imputation ======================================================

#' Antes do MCA, devo imputar valores faltantes para:
#' * Renda per capita (q10.1, q10.2)
#' * Idade
#' * Raca
#' * Estado civil
#' * Educacao do respondente
#' * Religia
#' * Mobilidade causa
#' * Desigualdade racial causa
#' * Influencia capacidade
#' * Voto

imp_sociodemo <- sips %>%
    mutate(q9 = as.character(q9),
           q2 = as.numeric(q2),
           q2 = ifelse(q2>3, NA, q2)) %>%
    left_join(., egp) %>%
    tbl_df %>%
    select(q1, q2, q3, q4, q9, cd_egp, q11) %>% 
    mutate_each(funs(as.factor(.)), c(q1, q2, q3, q4, q9, cd_egp, q11)) %>%
    #     sapply(., function(x) length(which(is.na(x)))/length(x)*100) # menos de 5% de valores faltantes em cada variavel
    #     md.pattern(.) %>% # 3.673 linhas sem missing 
    #     select(q31, q32) %>%
    #     marginplot(.) # o pressuposto do MCAR (missing completely at random) se sustenta?
    mice(.,  m = 1, maxit = 1, method = 'polyreg', seed = 500, 
         MaxNWts = 14000, maxcat = 200) %>%
    complete(., 1) %>%
    select(q1_imp = q1, q2_imp = q2, q3_imp = q3, q4_imp = q4, 
           q9_imp = q9, cd_egp_imp = cd_egp, q11_imp = q11)

imp_valores <- sips %>%
    tbl_df %>%
    select(q14, q15.1, q16.1, q22.1, q23, q26, q28, q31:q38) %>%
    mutate_each(funs(as.factor(.)), c(q14, q15.1, q16.1, q22.1, q23, q26, q28, q31:q38)) %>%
#     sapply(., function(x) length(which(is.na(x)))/length(x)*100) # menos de 5% de valores faltantes em cada variavel
#     md.pattern(.) %>% # 3.673 linhas sem missing 
#     select(q31, q32) %>%
#     marginplot(.) # o pressuposto do MCAR (missing completely at random) se sustenta?
    mice(.,  m = 1, maxit = 1, method = 'polyreg', seed = 500, 
         MaxNWts = 14000, maxcat = 200) %>%
    complete(., 1) %>%
    select(q14_imp = q14, q15.1_imp = q15.1, q16.1_imp = q16.1, 
           q22.1_imp = q22.1, q23_imp = q23, q26_imp = q26, q28_imp = q28, 
           q31_imp = q31, q32_imp = q32, q33_imp = q33, q35_imp = q35, 
           q36_imp = q36, q37_imp = q37, q38_imp = q38)

imp_renda <- sips %>%
    select(q10.1, q10.2) %>%
    mice(.,  m = 1, maxit = 1, method = 'pmm', seed = 500) %>%
    complete(., 1) %>%
    select(q10.1_imp = q10.1, q10.2_imp = q10.2)

imp_diversidade <- sips %>%
    tbl_df %>%
    select(q39:q44) %>%
    mutate_each(funs(as.factor(.)), c(q39:q44)) %>% 
    mice(.,  m = 1, maxit = 1, method = 'polyreg', seed = 500, 
         MaxNWts = 14000, maxcat = 200) %>%
    complete(., 1) %>%
    select(q39_imp = q39, q40_imp = q40, q41_imp = q41, q42_imp = q42,
           q43_imp = q43, q44_imp = q44)

sips <- cbind(sips, imp_sociodemo, imp_valores, imp_renda)
sips <- cbind(sips, imp_diversidade)

## Recode variables ========================================================
sips <- sips %>%
    tbl_df %>%
#     mutate(q9 = as.character(q9)) %>%
#     left_join(., egp) %>%
    # mutate_each(funs(as.numeric(.))) %>%
    dplyr::mutate(sexo = factor(q0,
                                levels = c(0, 1),
                                labels = c("Mulher", "Homem")),
                  uf = factor(uf),
                  renda_per_capita = q10.1_imp/q10.2_imp,
                  renda_per_capita = cut(renda_per_capita, 
                                         breaks = c(0, 162, 442, 1020, Inf), 
                                         labels = c("Ate R$162", 
                                                    "R$163-R$441",
                                                    "R$442-R$1.019",
                                                    "Acima de R$1.019")),
                  regiao = factor(str_to_title(regiao)),
                  q1_imp = as.integer(as.character(q1_imp)), 
                  idade = cut(q1_imp,
                              breaks = c(17, 25, 35, 45, 55, 65, 91),
                              labels = c("18-24 anos", "25-34 anos", "35-44 anos", 
                                         "45-54 anos", "55-64 anos", "65 anos ou mais"),
                              right = F),
                  raca = factor(q2_imp,
                                levels = c(1, 2, 3),
                                labels = c("Branca", "Preta", "Parda")),
                  branca = plyr::revalue(raca,
                                         c("Parda" = "Nao branca", 
                                           "Preta" = "Nao Branca")),
                  estado_civil = ifelse(as.numeric(q3_imp) > 1 & as.numeric(q3_imp) < 6, 0, 1),
                  estado_civil = factor(estado_civil,
                                        levels = c(1, 0),
                                        labels = c("Casado", "Nao casado")),
                  edu_mae = ifelse(q6 > 20, NA, q6),
                  edu_mae = cut(edu_mae,
                                breaks = c(1, 2, 12, 17, 21),
                                labels = c("Analfabeto", "Fundamental",
                                           "Medio", "Superior/pos"),
                                right = F),
                  edu_pai = ifelse(q5 > 20, NA, q5),
                  edu_pai = cut(edu_pai,
                                breaks = c(1, 2, 12, 17, 21),
                                labels = c("Analfabeto", "Fundamental",
                                           "Medio", "Superior/pos"),
                                right = F),
                  edu_resp = ifelse(as.numeric(q4_imp) > 20, NA, as.numeric(q4_imp)),
                  edu_resp = cut(edu_resp,
                                 breaks = c(1, 2, 12, 17, 21),
                                 labels = c("Analfabeto", "Fundamental",
                                            "Medio", "Superior/pos"),
                                 right = F),
                  # quais sao as referencias da recodificacao de classe "ocupacional"? descarto as donas de casa?
                  #            classe_resp = factor(ifelse(q9 == 1 | q9 == 2 | q9 == 4 | 
                  #                                     q9 == 14 | q9 == 5 | q9 == 6 |
                  #                                     q9 == 15,
                  #                                 "Profissionais e empregadores",
                  #                                 ifelse(q9 == 9 | q9 == 11 | q9 == 12 |
                  #                                            q9 == 3,
                  #                                        "Trabalhadores não manuais",
                  #                                        ifelse(q9 == 7 | q9 == 8,
                  #                                               "Trabalhadores manuais qualificados",
                  #                                               ifelse(q9 == 15 | q9 == 18 | q9 == 10 | q9 == 17 | q9 == 16 | q9 == 13,
                  #                                                      "Trabalhadores manuais não-qualificados",
                  #                                                      NA))))),
                  cd_egp_imp = as.numeric(cd_egp_imp),
                  classe_resp = factor(ifelse(cd_egp_imp < 5,
                                              "Trabalhador de colarinho branco",
                                              ifelse(cd_egp_imp == 5 | cd_egp_imp == 6,
                                                     "Pequena burguesia",
                                                     ifelse(cd_egp_imp == 7 | cd_egp_imp == 11,
                                                            "Trabalhor no setor rural",
                                                            ifelse(cd_egp_imp == 8 | cd_egp_imp == 9,
                                                                   "Trabalhador manual qualificado",
                                                                   ifelse(cd_egp_imp == 10,
                                                                          "Trabalhador manual não qualificado",
                                                                          NA)))))),
                  q11_imp = as.numeric(q11_imp),
                  religiao = ifelse(q11_imp==4, 3,
                                    ifelse(q11_imp>4 & q11_imp<17, 4, 
                                           q11_imp)),
                  religiao = factor(religiao,
                                    levels = c(1, 2, 3, 4, 17),
                                    labels = c("Acredita em Deus",
                                               "Catolica",
                                               "Protestante/Evangelica",
                                               "Outras religioes",
                                               "Ateu")),
                  auto_classe = cut(as.numeric(q14_imp), 
                                    breaks = c(0, 3, 7, 10),
                                    labels = c("Classe baixa", "Classe media", "Classe alta")),
                  lazer1 = factor(q15.1_imp,
                                  levels = c(1:21),
                                  labels = c("Cinema", "Filmes em casa",
                                             "Televisao", "Show musical", "Concerto",
                                             "Teatro", "Museu", "Galeria de arte",
                                             "Jogo de futebol/outros", "Livros",
                                             "Shopping", "Ficar em casa",
                                             "Igreja ou missa", "Praia", "Viajar",
                                             "Pescar", "Dormir", "Ginastica/caminhada",
                                             "Bar", "Parque/praca", "Outras atividades")),
                  musica1 = factor(q16.1_imp, 
                                   levels = c(1:23),
                                   labels = c("Sertanejo", "Forro", "Reggae", "Funk",
                                              "Musica classica", "Dance music",
                                              "Pagode", "Rap", "Jazz", "Mpb", "Rock",
                                              "Gospel", "Axe", "Country", "Bossa Nova",
                                              "New Age", "Brega", "Samba", "Hinos evangelicos",
                                              "Blues", "Hip hop", "Moda de viola",
                                              "Outros estilos")),
                  q22.1_imp = as.numeric(q22.1_imp),
                  mobilidade_causa = ifelse(q22.1_imp>8, NA, q22.1_imp),
                  mobilidade_causa = factor(mobilidade_causa, 
                                            levels = c(1:8), 
                                            labels = c("Familia rica", 
                                                       "Pais estudaram",
                                                       "Estudo",
                                                       "Perseverança",
                                                       "Trabalho duro",
                                                       "Conhecer pessoas influentes",
                                                       "Contato com políticos",
                                                       "Outros")),
                  #            mobilidade = ifelse(q22.1==1|q22.1==2|q22.1==6|q22.1==7, 0,
                  #                                ifelse(q22.1==3|q22.1==4|q22.1==5, 1, NA)),
                  #            mobilidade = factor(mobilidade, 
                  #                                levels = c(0, 1), 
                  #                                labels = c("Mobilidade dada", "Mobilidade adquirida")),
                  desigualdade_racial_causa = factor(q23_imp, 
                                                     levels = c(1:5), 
                                                     labels = c("Preconceito/discriminacao", 
                                                                "Negros nao aproveitam oport.",
                                                                "Faltam politicas",
                                                                "Pobreza dos negros",
                                                                "Nao existe desigualdade racial")),
                  #          negros = ifelse(q23==1, "Preconceito e o maior problema",
                  #                                    ifelse(q23!=1, "Preconceito nao e o maior problema",
                  #                                           NA)),
                  q26_imp = as.numeric(q26_imp),
                  influencia_capacidade = ifelse(q26_imp < 4, 1,
                                                 ifelse(q26_imp > 3 & q26_imp <7, 2,
                                                        ifelse(q26_imp > 6, 3, NA))),
                  influencia_capacidade = factor(influencia_capacidade, 
                                                 levels = c(1:3),
                                                 labels = c("Baixa capacidade",
                                                            "Media capacidade",
                                                            "Alta capacidade")),
                  #            influencia_gov = ifelse(q26 < 5, 0, 
                  #                                ifelse(q26 > 5, 1, 
                  #                                       ifelse(q26 == 5, 0+(runif(745)<=.50),
                  #                                              NA))),
                  #            influencia_gov = factor(influencia_gov,
                  #                          levels = c(0, 1),
                  #                          labels = c("Influencia pouco",
                  #                                     "Influencia muito")),
                  #            confianca = ifelse(q27 == 2 | q27 == 1, 1, 
                  #                               ifelse(q27 == 4 | q27 == 5, 0, 
                  #                                      ifelse(q27 == 3, 0+(runif(673)<=.50),
                  #                                             NA))),
                  #            confianca = factor(confianca,
                  #                          levels = c(1, 0),
                  #                          labels = c("Nao confia nas pessoas",
                  #                                     "Confia nas pessoas")),
                  #            voto = ifelse(q28 == 2 | q28 == 1, 1, 
                  #                          ifelse(q28 == 4 | q28 == 5, 0, 
                  #                                 ifelse(q28 == 3, 0+(runif(673)<=.50),
                  #                                        NA))),
                  q28_imp = as.numeric(q28_imp),
                  voto = ifelse(q28_imp < 3, 1,
                                ifelse(q28_imp == 3, 2, 
                                       ifelse(q28_imp > 3, 3, NA))),
                  voto = factor(voto,
                                levels = c(1:3),
                                labels = c("Pobres escolhem pior",
                                           "Pobres nao escolhem pior, nem melhor",
                                           "Pobres nao escolhem pior")),
                  desigualdade_solucao = factor(q29,
                                                levels = c(1:3),
                                                labels = c("Esforco dos pobres",
                                                           "Ajuda do governo",
                                                           "Desigualdade e natural")),
                  #            diversidade = ifelse(q30 < 3, 0, 
                  #                                 ifelse(q30 > 3, 1, 
                  #                                        ifelse(q30 == 3, 0+(runif(1116)<=.50),
                  #                                               NA))),
                  diversidade = ifelse(q30 < 3, 1, 
                                       ifelse(q30 == 3, 2, 
                                              ifelse(q30 > 3, 3, NA))),
                  diversidade = factor(diversidade,
                                       levels = c(1:3),
                                       labels = c("Diversidade e negativa",
                                                  "Diversidade e neutra",
                                                  "Diversidade e positiva")))
#            meio_ambiente = ifelse(q37 == 2 | q37 == 1, 1, 
#                                   ifelse(q37 == 4 | q37 == 5, 0, 
#                                          ifelse(q37 == 3, 0+(runif(445)<=.50),
#                                                 NA))),
#            meio_ambiente = factor(meio_ambiente,
#                          levels = c(1, 0),
#                          labels = c("Separar lixo nao e importante",
#                                     "Separar lixo e importante"))) %>%

sips <- sips %>%
    mutate(forma_influencia = ifelse(q25.1==1, 1,
                                     ifelse(q25.1>1 & q25.1<6, 2,
                                            ifelse(q25.1==6, 3,
                                                   ifelse(q25.1==7, 4,
                                                   NA)))),
           forma_influencia = factor(forma_influencia,
                                     levels = c(1:4),
                                     labels = c("Voto", "Canais ou instituicoes oficiais", "Protesto", "Nao tenho influencia")))

sips <- sips %>%
    tbl_df %>%
    mutate_each(funs(as.integer(.)), q39_imp:q44_imp) %>%
    mutate_each(funs(ifelse(.<3, 3,
                            ifelse(.>2 & .<7, 2, 
                                   ifelse(.==7, 1, NA)))), q39_imp:q44_imp) %>%
    mutate(q39_imp = factor(q39_imp,
                            levels = c(1:3),
                            labels = c("Circulo familiar (pobre)",
                                       "Circulo extrafamiliar (pobre)",
                                       "Nenhuma proximidade (pobre)")),
           q40_imp = factor(q40_imp,
                            levels = c(1:3),
                            labels = c("Circulo familiar (deficiente)",
                                       "Circulo extrafamiliar (deficiente)",
                                       "Nenhuma proximidade (deficiente)")),
           q41_imp = factor(q41_imp,
                            levels = c(1:3),
                            labels = c("Circulo familiar (negro)",
                                       "Circulo extrafamiliar (negro)",
                                       "Nenhuma proximidade (negro)")),
           q42_imp = factor(q42_imp,
                            levels = c(1:3),
                            labels = c("Circulo familiar (homossexual)",
                                       "Circulo extrafamiliar (homossexual)",
                                       "Nenhuma proximidade (homossexual)")),
           q43_imp = factor(q43_imp,
                            levels = c(1:3),
                            labels = c("Circulo familiar (umbanda)",
                                       "Circulo extrafamiliar (umbanda)",
                                       "Nenhuma proximidade (umbanda)")),
           q44_imp = factor(q44_imp,
                            levels = c(1:3),
                            labels = c("Circulo familiar (favela)",
                                       "Circulo extrafamiliar (favela)",
                                       "Nenhuma proximidade (favela)")))
    
table(sips$q43_imp)

## Create constructs for gender and environment ============================
sips <- sips %>%
    mutate_each(funs(recode(., "1" = "0", "3" = "1", "2" = "3")), q17:q21) %>%
    mutate_each(funs(as.numeric(.)), q17:q21)

sips_plano <- svydesign(ids = ~pessoa, 
                        weights = ~peso, 
                        strata = ~regiao, 
                        data = sips)

x <- scale(cbind(sips_plano$variables[, c("q17", "q18", "q19", "q20", "q21")]))
etica <- svyfactanal(~q17+q18+q19+q20+q21, sips_plano, factors = 1, rotation = "none")
etica_scores <- x%*%solve(cor(x))%*%loadings(etica)

x <- sips_plano$variables %>%
    tbl_df %>%
    mutate_each(funs(as.integer(.)), q31_imp:q35_imp) %>%
    select(q31_imp:q35_imp) %>%
    scale(.)
genero <- svyfactanal(~as.integer(q31_imp)+as.integer(q32_imp)+as.integer(q33_imp)+as.integer(q35_imp), sips_plano, factors = 1, rotation = "none")
genero_scores <- x%*%solve(cor(x))%*%loadings(genero)

x <- sips_plano$variables %>%
    tbl_df %>%
    mutate_each(funs(as.integer(.)), q36_imp:q38_imp) %>%
    select(q36_imp:q38_imp) %>%
    scale(.)
meio_ambiente <- svyfactanal(~as.integer(q36_imp)+as.integer(q37_imp)+as.integer(q38_imp), sips_plano, factors = 1, rotation = "none")
meio_ambiente_scores <- x%*%solve(cor(x))%*%loadings(meio_ambiente)

# sips_plano$variables <- sips_plano$variables %>%
#     tbl_df %>%
#     mutate(etica_fator = etica_scores, genero_fator = genero_scores, meio_ambiente_fator = meio_ambiente_scores)

sips <- mutate(sips, etica_fator = etica_scores, genero_fator = genero_scores, meio_ambiente_fator = meio_ambiente_scores)
rm(x, etica, etica_scores, genero, genero_scores, meio_ambiente, meio_ambiente_scores)

sips <- sips %>%
    mutate(etica_construto = cut(sips$etica_fator, breaks = 3, labels = c("Tolera ilegalidade", "Legalidade relativa", "Nao tolera a ilegalidade")),
           genero_construto = cut(sips$genero_fator, breaks = 3, labels = c("Menos igualitario (genero)", "Igualitario (genero)", "Mais igualitario (genero)")),
           meio_ambiente_construto = cut(sips$meio_ambiente_fator, breaks = 3, labels = c("Menos ambientalista", "Ambientalista", "Mais ambientalista")))

names(sips)

## Save data ===============================================================
setwd("..")
saveRDS(sips, file = "tb_sips2012_analitico.Rds")
write_delim(sips, "tb-sips-analitico.csv", delim = ";")

## References ==============================================================
#' http://r.789695.n4.nabble.com/Retrieve-quot-raw-scores-quot-in-factor-analysis-td3222340.html
#' https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
#' http://datascienceplus.com/imputing-missing-data-with-r-mice-package/