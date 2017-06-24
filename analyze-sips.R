## Load packages ===========================================================
library(tidyverse)
library(plyr)
library(magrittr)
library(stringr)
library(SciencesPo)
library(survey)
library(FactoMineR) # executa MCA
library(missMDA) # imputacao de valores faltantes
library(GDAtools) # calculo das "modified rates"
library(ggrepel)
library(extrafont)
library(RColorBrewer)
library(factoextra, quietly = T, verbose = F) # depende do ggplot2 para as visualizacoes
library(gridExtra)
library(reshape2)
library(data.table)
library(janitor)
library(googlesheets)
options(scipen = 999)

## Load data ===============================================================
# sips <- readRDS("tb_sips2012_analitico.Rds")
sips <- read_csv2(url("https://docs.google.com/spreadsheets/d/1Snqrzhi3m8LB2kk64KfZzLfAB85aSv5FupM4xhxYxSs/pub?gid=1676210506&single=true&output=csv"))

## Define sample design ====================================================
sips_plano <- svydesign(ids = ~pessoa, 
                        weights = ~peso, 
                        strata = ~regiao, 
                        data = sips)

## MCA =====================================================================
## seleciona variaveis para a MCA
sips_mca <- sips %>%
    select(sexo, renda_per_capita, raca, estado_civil, edu_resp, idade,
           lazer1, musica1, classe_resp, religiao, mobilidade_causa, desigualdade_racial_causa, 
           influencia_capacidade, forma_influencia, voto, desigualdade_solucao,
           diversidade, etica_construto, genero_construto, meio_ambiente_construto
           # , q39_imp, q40_imp, q41_imp, q42_imp, q43_imp, q44_imp
           )

sum(sapply(sips_mca, nlevels)) # 113 modalidades

# Categorias com frequência inferior a 5%:

#' "Analfabeto". (Retiro?);
#' "Conhecer pessoas influentes", "Contato com políticos", "Outros";
#' "Nao tenho influencia"
#' "Tolera ilegalidade"
#' "Menos igualitario (genero)"

## executa MCA

# imputa casos faltantes
# nd1 <- estim_ncpMCA(sips_mca[c(4)], ncp.max = 1)
# tab_disj_impute <- imputeMCA(sips_mca[4], ncp = 1)

# mca <- MCA(sips_mca, quali.sup = c(1, 3:4, 6:10), graph = F, 
#             method = "Indicator", row.w = sips$peso)

# getindexcat(data.frame(sips_mca[, c(2, 5, 11:20)]))
# mca <- speMCA(data.frame(sips_mca[, c(2, 5, 9, 11:20)]), 
#               excl = c(5, 14, 15, 16, 28, 38, 41),
#               row.w = sips$peso)



# apenas as variaveis atitudinais sao adicionadas como ativas
getindexcat(data.frame(sips_mca[, c(11:20)]))
mca <- speMCA(data.frame(sips_mca[, c(11:20)]), 
              excl = c(6, 7, 8, 20, 30, 33),
              row.w = sips$peso)

# taxa de variancia acumulada modificada
modif.rate(mca)[1:5, ] # as duas primeiras dimensoes explicam 77,89% da variancia
mca$eig

# interpretacao sintetica dos eixos
round(mca$var$eta2[, 1:3], 2) # mensura a associacao entre a variavel e a dimensao

contr_var <- contrib(mca)$var.ctr[, 1:3] # contribuicao das variaveis
contr_var %>% 
    tbl_df %>% 
    cbind(row.names(contrib(mca)$var.ctr), .) %>%
    arrange(desc(dim.1)) %>%
    # arrange(desc(dim.2))
    
contr_mod <- data.frame(mca$var$contrib[1:32, 1:2]) # contribuicao das modalidades
contr_mod %>%
    add_rownames() %>%
    tbl_df %>%
    arrange(desc(dim.1)) %>%
    select(rowname, dim.1) %>%
    data.frame
    mutate(mod = mca_eixo_um$mod,
           acum = cumsum(dim.1)) %>%
    select(mod, dim.1, acum) %>%
    # print(n = 13)

# interpretacao dos dois primeiros eixos
n_cat_ativas <- length(getindexcat(data.frame(sips_mca[, c(11:20)])))
n_cat_ignored <- length(c(6, 7, 8, 20, 30, 33))
100/(sum(n_cat_ativas-n_cat_ignored)) # criterio de contribuicao media: 3.12%
rm(n_cat_ativas, n_cat_ignored)

# visualizacao - eixo 1
cats <- apply(sips_mca[, c(11:20)], 2, function(x) nlevels(as.factor(x)))
mca_eixo_um <- data.frame(mca$var$coord, Variable = rep(names(cats), cats)[-c(1:3, 19, 30, 33)])
#  mca_obs_df = data.frame(mca$ind$coord) # data frame with observation coordinates

mca_eixo_um <- mca_eixo_um %>%
    tbl_df %>%
    dplyr::mutate(var = str_replace_all(row.names(mca_eixo_um), "\\..*", ""),
           mod = str_replace_all(row.names(mca_eixo_um), "\\.$", ""),
           mod = str_replace_all(row.names(mca_eixo_um), ".*\\.", "")) %>%
    select(var, mod, dim.1, dim.2)
# mca_eixo_um$mod[7] <- "Negros nao aproveitam oport"

mean(mca$var$contrib[1:32, 1]) # contribuicao media das modalidades = 3.125

contr_mod_eixo_um <- contr_mod %>% # contribuicao das modalidades para o eixo 1
    add_rownames %>%
    mutate(rowname = str_replace_all(rowname, "\\.$", ""),
           rowname = str_replace_all(rowname, ".*\\.", "")) %>%
    tbl_df %>%
    arrange(desc(dim.1)) %>%
    mutate(acum = cumsum(dim.1)) %>%
    select(rowname, contr = dim.1, acum) %>%
    # filter(acum < 81)

mca_eixo_um <- mca_eixo_um %>% 
    tbl_df %>%
    # filter(mod %in% contr_mod_eixo_um$rowname) %>%
    left_join(., contr_mod_eixo_um[, c("rowname", "contr")], by = c("mod" = "rowname"))

# mca_eixo_um %>%
#     arrange(desc(contr)) %>%
#     select(var, mod, dim.1, contr) %>%
#     data.frame

p1 <- ggplot(data = mca_eixo_um, 
       aes(x = dim.1, y = dim.2)) + # plot of variable categories
    geom_hline(yintercept = 0, colour = "black") +
    geom_vline(xintercept = 0, colour = "black") +
    # geom_text() +
    geom_point(aes(colour = factor(var)), size = mca_eixo_um$dim.1 * 5) + 
    geom_text_repel(
        aes(
            colour = factor(var),
            # size = mca_eixo_um$dim.1,
            label = mca_eixo_um$mod),
        family = "GillSans",
        point.padding = unit(0.25, "lines"),
        box.padding = unit(0.25, "lines")) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(x = "Eixo 1 (60,9%)", y = "Eixo 2 (16,9%)") +
    theme(axis.title.x = element_text(family = "GillSans", size = 14), 
          axis.title.y = element_text(family = "GillSans", size = 14))

ggplot(data = mca_eixo_um, 
       aes(x = dim.1, y = dim.2, label = mca_eixo_um$mod, 
           shape = mca_eixo_um$var)) + # plot of variable categories 
    geom_hline(yintercept = 0, colour = "black") +
    geom_vline(xintercept = 0, colour = "black") +
    # geom_text() +
    geom_point(aes(mca_eixo_um$dim.1, size = mca_eixo_um$contr)) +
    scale_shape_manual(values=1:length(unique(mca_eixo_um$var))) +
    geom_text_repel(aes(dim.1, dim.2, label = mca_eixo_um$mod),
                    family = "GillSans",
                    point.padding = unit(0.25, "lines"),
                    box.padding = unit(0.25, "lines")) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(x = "Eixo 1 (60,9%)", y = "Eixo 2 (16,9%)") +
    theme(axis.title.x = element_text(family = "GillSans", size = 14), 
          axis.title.y = element_text(family = "GillSans", size = 14))
    
mca$eig$eigen[1:3]

mca3$quali.sup$coord[1:44, 1:3]
1/(length(c(2:3, 5:8, 12:17))) # eigenvalue medio = 0.083
mca1$var$coord
round(mca1$var$eta2, 2)
round(mca1$var$eta2, 2)

round(mca3$var$eta2, 2)
round(mca3$var$contrib, 2)[which(mca3$var$contrib[, 1]>2.27), 1:3]
round(mca3$var$contrib, 2)[which(mca3$var$contrib[, 2]>2.27), 1:3]
round(mca3$var$contrib, 2)[which(mca3$var$contrib[, 3]>2.27), 1:3]
sum(mca3$var$contrib[1:44, 1][order(mca3$var$contrib[1:44, 1], decreasing = T)][1:16])
sum(mca3$var$contrib[1:44, 3][order(mca3$var$contrib[1:44, 3], decreasing = T)][1:20])

fviz_contrib(mca3, choice = "var", axes = 1)
mca1$var$contrib[, 1][order(-mca1$var$contrib[, 1])]
mca1$var$contrib[, 2][order(mca1$var$contrib[, 2])]

# voto, influencia, confianca, desigualdade (ordem democratica)
# ordem democratica: escolha política e classe, ampliação de direitos, forma e capacidade de influência
# novos direitos e valores: meio ambiente

# analise de correlacao
chisq.test(sips_mca$meio_ambiente_construto, sips_mca$genero_construto)
chisq.test(sips_mca$meio_ambiente_construto, sips_mca$etica_construto)
chisq.test(sips_mca$meio_ambiente_construto, sips_mca$desigualdade_solucao)
chisq.test(sips_mca$meio_ambiente_construto, sips_mca$forma_influencia)
chisq.test(sips_mca$meio_ambiente_construto, sips_mca$influencia_capacidade)

# AFE e AFC
svyfactanal(~mobilidade_causa+desigualdade_racial_causa+influencia_capacidade+
                forma_influencia+voto+desigualdade_solucao+diversidade+etica_construto+
                genero_construto+meio_ambiente_construto, 
            sips_plano, 
            factors = 3,
            rotation = "varimax", 
            digits = 2,
            cutoff = .2,
            sort = T)

sips_plano <- svydesign(ids = ~pessoa, 
                        weights = ~peso, 
                        strata = ~regiao, 
                        data = teste)
teste <- sips %>%
    select(1, 4, 5, 94:106) %>%
    mutate_each(funs(as.numeric(.))) %>%
    data.frame

    
faPCdirect <- fa.poly(teste, nfactors=5, rotate="varimax")
faPCdirect$fa$loadings










#' "... the Burt matrix also plays an important theoretical rôle because the
#' eigenvalues obtained from its analysis give a better approximation of
#' the inertia explained by the factors than the eigenvalues of X."
#' (https://www.utdallas.edu/~herve/Abdi-MCA2007-pretty.pdf)

#' "There is no 'rule of thumb' to choose the number of dimension to keep for
#' the data interpretation. It depends on the research question and the 
#' researcher’s need. 
#' The higher the retention, the more subtlety in the original data is 
#' retained in the low-dimensional solution" (Mike Bendixen, 2003).

## visualiza resultados do MCA
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 4, 0, 0))

fviz_screeplot(mca1) + geom_hline(yintercept=0.056, linetype=2, color="red") 
fviz_mca_biplot(mca1, axes = c(1, 2), select.ind = list(contrib = 10)) #, select.var = list(contrib = 10))
fviz_mca_biplot(mca2, axes = c(1, 2), select.ind = list(contrib = 10)) #, select.var = list(contrib = 10))
fviz_mca_biplot(mca1, axes = c(1, 2), select.ind = list(contrib = 20), invisible ="quali.sup")
fviz_mca_biplot(mca2, axes = c(1, 2), select.ind = list(contrib = 20), invisible ="quali.sup")
fviz_mca_biplot(mca3, axes = c(1, 2), select.ind = list(contrib = 20), invisible ="quali.sup")

# (Figura 1)
windowsFonts(Arial=windowsFont("TT Arial"))
fviz_mca_biplot(mca3, axes = c(1, 3), invisible = "ind", col.var = "black", repel = T, 
                col.quali.sup = "gray40", title = "", labelsize = 5,
                select.var = list(name = c("Superior/pos", "Trabalhadores manuais não-qualificados",
                                    "Acima de R$1.019", "Profissionais e empregadores",
                                    "Fundamental", "Ate R$162", "Meio", "Outras religioes",
                                    "Analfabeto", "R$442-R$1.019", "55 ou mais anos", "Branca",
                                    "Parda", "Trabalhadores manuais qualificados", "R$163-R$441",
                                    "Trabalhadores não manuais", "Jazz", "New Age", "Hinos evangelicos", "Concerto", "Pescar",
                                    "Igreja ou missa", "Show musical", "Forro", "Cinema", "Sertanejo", "Teatro"))) + theme_bw() +
    labs(x = "Eixo 1 (28,63%)", y = "Eixo 3 (8,46%)") +
    theme(axis.title.x = element_text(family = "Arial", size = 14), 
          axis.title.y = element_text(family = "Arial", size = 14))

# (Figura 2)
fviz_mca_biplot(mca3, axes = c(1, 3), invisible = "ind", col.var = "black", repel = T, col.quali.sup = "gray40", title = "", labelsize = 5,
                select.var = list(name = c("Pobres nao escolhem pior", "Separar lixo nao e importante",
                                           "Pobres escolhem pior", "Confia nas pessoas", "Influencia muito", 
                                           "Esforco dos pobres", "Influencia pouco", "Preta",
                                           "Des. natural", "Preconceito nao e o maior problema",
                                           "Branca", "Preconceito e o maior problema", 
                                           "Separar lixo e importante", "Separar lixo e importante",
                                           "Profissionais e empregadores", "Nao casado",
                                           "Trabalhadores manuais qualificados", "Outras religioes",
                                           "55 ou mais anos", "Casado", "Superior/pos",
                                           "Jazz", "New Age", "Hinos evangelicos", "Concerto", "Pescar",
                                           "Igreja ou missa", "Show musical", "Forro", "Cinema", "Sertanejo", "Teatro"))) + theme_bw() +
    labs(x = "Eixo 1 (28,63%)", y = "Eixo 3 (8,46%)") +
    theme(axis.title.x = element_text(family = "Arial", size = 14), 
          axis.title.y = element_text(family = "Arial", size = 14))

row.names(mca3$var$coord)[c(1, 4)]
fviz_mca_ind(mca1, label="none", habillage = mca1$call$X$renda_per_capita,
             addEllipses=TRUE, ellipse.level=0.95)
fviz_mca_ind(mca2, label="none")
fviz_mca_ind(mca3, label="none")
fviz_mca_ind(mca3, axes = c(1, 3), label="none")
fviz_mca_ind(mca2, label="none", habillage = mca2$call$X$renda_per_capita,
             addEllipses=TRUE, ellipse.level=0.95)
fviz_mca_ind(mca3, label="none", habillage = mca2$call$X$renda_per_capita,
             addEllipses=TRUE, ellipse.level=0.95)
fviz_mca_ind(mca2, label="none", habillage = mca2$call$X$edu_resp,
             addEllipses=TRUE, ellipse.level=0.95)
fviz_mca_ind(mca3, label="none", habillage = mca2$call$X$edu_resp,
             addEllipses=TRUE, ellipse.level=0.95)
fviz_mca_biplot(mca2, label = "var", habillage = mca2$call$X$edu_resp,
                addEllipses=TRUE, shape.var = 15)
fviz_mca_biplot(mca3, label = "var", habillage = mca3$call$X$edu_resp,
                addEllipses=TRUE, shape.var = 15, invisible = "quanti.sup")
fviz_mca_biplot(mca2, label = c("var", "quali.sup"), habillage = mca2$call$X$racismo_negros,
                addEllipses=TRUE, shape.var = 15, invisible = "quanti.sup")
fviz_mca_ind(mca3, label="var", habillage = mca2$call$X$voto,
             addEllipses=TRUE, ellipse.level=0.95)
fviz_mca_ind(mca3, label="var", habillage = mca2$call$X$meio_ambiente,
             addEllipses=TRUE, ellipse.level=0.95)
fviz_mca_biplot(mca3, label = "var", habillage = mca3$call$X$voto,
                addEllipses=TRUE, shape.var = 15, ellipse.level=0.95, 
                invisible ="quali.sup")
fviz_mca_biplot(mca3, label = "var", habillage = mca3$call$X$mobilidade,
                addEllipses=TRUE, shape.var = 15, ellipse.level = 0.95, 
                invisible ="quali.sup", select.var=list(contrib = 30))



mca3_vars_df = data.frame(mca3$var$coord, Variable = rep(names(cats2), cats2))
mca3_obs_df = data.frame(mca3$ind$coord)
ggplot(data = mca3_obs_df, aes(x = Dim.1, y = Dim.3)) +
    geom_hline(yintercept = 0, colour = "gray70") +
    geom_vline(xintercept = 0, colour = "gray70") +
    # geom_point(colour = "gray50", alpha = 0.7) +
    # geom_density2d(colour = "gray80") +
    geom_text(data = mca3_vars_df, 
              aes(x = Dim.1, y = Dim.2, 
                  label = rownames(mca3_vars_df),
                  colour = Variable)) +
#     geom_point(data = mca3_vars_df, aes(shape = Variable)) +
#     scale_shape_manual(values=seq(0, 15)) +
    scale_colour_discrete(name = "Variable") +
    theme_bw()

# individuos por grupos educacionais
# fviz_mca_ind(res.mca1, label="none",
#              habillage = res.mca1$call$X$edu_respondente,
#              addEllipses=TRUE, ellipse.level=0.95) +
#     ggtitle("Posição social dos entrevistados por faixa de escolaridade (SIPS/Ipea 2012)") +
#     theme_bw() + theme(axis.title.x = element_text(size=11), 
#                        axis.title.y = element_text(size=11), 
#                        plot.title = element_text(family = "Helvetica Neue", 
#                                                  size = 12))

# biplots of variables and groups elipses
var <- get_mca_var(res.mca1)
head(round(var$coord, 2))

#' The plot above helps to identify variables that are the most correlated 
#' with each dimension. The squared correlations between variables and the 
#' dimensions are used as coordinates.
#' 
#' It can be seen that, the variables Diarrhae, Abdominals and Fever are the
#' most correlated with dimension 1. Similarly, the variables Courgette and
#' Potato are the most correlated with dimension 2.

# Control variable colors using their contributions

fviz_mca_var(res.mca1, col.var = "contrib") +
    scale_color_gradient2(low = "white", mid = "blue",
                          high = "red", midpoint = 2) +
    theme_bw()

## Plots
windowsFonts(Helvetica = windowsFont("Helvetica"))
png(filename = "Figura_1.png")

p1 <- ggplot(na.omit(sips), aes(edupai)) + 
    geom_bar(aes(y = (..count..)/sum(..count..)*100)) +
    labs(x = "", y = "") +
    ggtitle("GR?FICO 1 \nEscolaridade do pai \n(Em %)") +
    theme_bw(base_size = 12, base_family = "Helvetica") +
    theme(panel.grid.major = element_line(size = .5, color = "grey"),
          axis.line = element_line(size=.7, color = "black")) + 
    theme(plot.title = element_text(vjust = 1.2, size = 13, hjust = .002)) +
    theme(axis.text.x = element_text(size=11))

## Multiplot panel

p1 <- ggplot(na.omit(sips), aes(edupai)) + 
    geom_bar(aes(y = (..count..)/sum(..count..)*100)) +
    ggtitle("Pai") +
    labs(x = "", y = "") +
    theme_bw(base_size = 12) +
    theme(text=element_text(family="Helvetica57-Condensed")) +
    theme(panel.grid.major = element_line(size = .5, color = "grey"),
          axis.line = element_line(size=.7, color = "black")) + 
    theme(plot.title = element_text(vjust = 1.2, size = 14, hjust = -0.03, face = "bold")) +
    theme(axis.text.x = element_text(size=11))

print(p1)

p2 <- ggplot(na.omit(sips), aes(edumae)) + 
    geom_bar(aes(y = (..count..)/sum(..count..)*100)) +
    labs(x = "", y = "") +
    ggtitle("M?e") +
    theme_bw(base_size = 12) +
    theme(text=element_text(family="Helvetica57-Condensed")) +
    theme(panel.grid.major = element_line(size = .5, color = "grey"),
          axis.line = element_line(size=.7, color = "black")) + 
    theme(plot.title = element_text(vjust = 1.2, size = 14, hjust = -0.03, face = "bold")) +
    theme(axis.text.x = element_text(size=11))

library(extrafont)
font_import()
fonts()

print(p2)
png(filename = "figura_escolaridade_pais.png")
grid.arrange(p1, p2, main = textGrob("Escolaridade dos pais", gp=gpar(fontsize=14, fontfamily = "Helvetica57-Condensed", just = c("left", "top"))))

grid.arrange(p1, p2)
dev.off()

p3 <- ggplot(na.omit(sips), aes(ocup_pai, fill = edupai)) + 
    geom_bar() +
    theme(axis.text.x = element_text(angle=90))

print(p3)

## ggplot(na.omit(sips), aes(ocup_pai)) + geom_bar() + theme(axis.text.x = element_text(angle=90))

p4 <- ggplot(na.omit(sips), aes(ocup_respodente)) + 
    geom_density(aes(group = eduent, colour = eduent)) +
    scale_colour_discrete(name="Escolaridade \ndo entrevistado") +
    theme_bw(base_size = 11) +
    theme (axis.text.x = element_text(angle=90)) + 
    labs(x = "", y = "Densidade") + 
    theme(text=element_text(family="Helvetica57-Condensed")) +
    theme(panel.grid.major = element_line(size = .5, color = "grey"),
          axis.line = element_line(size=.7, color = "black")) + 
    theme(axis.text.x = element_text(size=10))

png(filename = "figura_escola_ocupacao.png")
print(p4)
dev.off()

p5 <- ggplot(na.omit(sips), aes(edupai, renda)) + 
    geom_jitter(alpha=0.5, aes(color=ocup_pai)) +
    scale_y_continuous(limits = c(0, 7500))

print(p5)

p6 <- ggplot(sips, aes(desig, renda, colour=ocup_respodente)) +
    geom_point(alpha=.3) +
    geom_smooth(alpha=.2, size=2) +
    theme(axis.text.x = element_text(angle=90))

print(p6)

subset <- subset(sips, c(sips$ocup_respodente=="Dona de casa" | 
                             sips$ocup_respodente=="Profissionais superiores/professores"))

p7 <- ggplot(subset, aes(x = desig)) + 
    geom_density(aes(group = ocup_respodente, colour = ocup_respodente)) +
    theme(axis.text.x = element_text(angle=90))

print(p7)

p8 <- ggplot(subset, aes(x = ocup_respodente, fill = desig)) + 
    geom_bar(aes(y = (..count..)/sum(..count..)*100), position = position_dodge(width = 0.9), width = 0.5) + 
    scale_fill_brewer(name="Raz?es da desigualdade") +
    theme_bw(base_size = 11) + 
    labs(x = "", y = "") + 
    theme(text=element_text(family="Helvetica57-Condensed")) +
    theme(panel.grid.major = element_line(size = .5, color = "grey"),
          axis.line = element_line(size=.7, color = "black")) + 
    theme(axis.text.x = element_text(size=9))

png(filename = "figura_ocupacao_desigualdade.png")
print(p8)
dev.off()

sips$ocup_respodente <- ordered(sips$ocup_respodente, 
                                levels = c("Profissionais superiores/professores",
                                           "Admin. e gerentes",
                                           "Propriet. e empregadores urbanos",
                                           "Funcoes medias serv. e industria",
                                           "Apoio e auxiliares",
                                           "Vendedores",
                                           "Propr. e empregadores rurais",
                                           "Prof. seguran?a",
                                           "Dona de casa",
                                           "Aposentado",
                                           "Desempregado"))

sipsOcu <- subset(sips, !(ocup_respodente == "Aposentado" | ocup_respodente == "Desempregado"))

p9 <- ggplot(data = na.omit(sipsOcu), 
             aes(x = eduent, fill = ocup_respodente)) + 
    geom_density(alpha = .5) + 
    scale_fill_brewer(name="Ocupa??o", palette="RdYlBu") +
    theme(axis.text.x = element_text(angle=90)) + 
    theme_bw(base_size = 11) +
    theme (axis.text.x = element_text(angle=90)) + 
    labs(x = "", y = "Densidade") + 
    theme(text=element_text(family="Helvetica57-Condensed")) +
    theme(panel.grid.major = element_line(size = .5, color = "grey"),
          axis.line = element_line(size=.7, color = "black")) + 
    theme(axis.text.x = element_text(size=11))

png(filename = "figura_ocupacao_edu.png")
print(p9)
dev.off()

## Cluster analysis ========================================================
hcpc3 <- HCPC(mca3) # a arvore hierarquica sugere tres clusters
round(hcpc3$desc.var$test.chi2, 2) # as variaveis educacao e classe sao as que mais caracterizam a particao em tres clusters
round(hcpc3$desc.var$category$`1`, 2)
round(hcpc3$desc.var$category$`2`, 2)
round(hcpc3$desc.var$category$`3`, 2)
hcpc3$desc.axes
hcpc3$desc.ind
fviz_cluster(hcpc3)
head(hcpc3$data.clust, 10)


## References ==============================================================
#' http://www.sthda.com/english/wiki/print.php?id=232
#' http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization
#' http://www.sthda.com/english/wiki/fviz-mca-quick-multiple-correspondence-analysis-data-visualization-r-software-and-data-mining
#' http://www.summerschoolsineurope.eu/course/2998/multiple-correspondence-analysis-for-the-social-sciences
#' http://www.classification-society.org/csna/mda-sw/correspondances/
#' http://www.summerschoolsineurope.eu/course/2998/multiple-correspondence-analysis-for-the-social-sciences
#' http://www.exegetic.biz/blog/2013/05/introducing-r-plottin-categorical-variables/
#' http://tables2graphs.com/doku.php?id=03_descriptive_statistics#figure_5
#' http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/
#' https://mandymejia.wordpress.com/2013/11/13/10-reasons-to-switch-to-ggplot-7/
#' http://flowingdata.com/2012/05/15/how-to-visualize-and-compare-distributions/
#' http://minimaxir.com/2015/02/ggplot-tutorial/
#' http://multithreaded.stitchfix.com/blog/2015/03/17/grammar-of-data-science/
#' http://www.cs.utexas.edu/~cannata/dataVis/Class%20Notes/Beautiful%20plotting%20in%20R_%20A%20ggplot2%20cheatsheet%20_%20Technical%20Tidbits%20From%20Spatial%20Analysis%20&%20Data%20Science.pdf
#' "Beautiful plotting in R: A ggplot2 cheatsheet"
#' http://bconnelly.net/2013/10/creating-colorblind-friendly-figures/
#' http://stackoverflow.com/questions/20574580/how-to-exclude-missing-values-from-mca-multiple-correspondence-analysis-in-r
#' https://tice.agrocampus-ouest.fr/mod/forum/discuss.php?d=1598
#' https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html
#' http://factominer.free.fr/classical-methods/hierarchical-clustering-on-principal-components.html
#' https://groups.google.com/forum/#!topic/factominer-users/os2qK_wLr0Q