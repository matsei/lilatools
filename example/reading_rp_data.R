if( !"lilatools" %in% installed.packages())
  install.packages("../lilatools_0.1.0.zip", repos=NULL) # set correct path to installation file

library(lilatools)
library(tidyverse)


# read test
setwd("N:/projekte/19-009_hlnug-support19-22/bearbeitung/03_anpassung_ensemble_Rscript/01_r_skript/lilatools/")

d1 <- read_lila_block("../../02_data/04_beispieldaten_rp/pegel-qmes.lila")
d2 <- read_lila_block("../../02_data/04_beispieldaten_rp/pegel-qsim.lila")
d3 <- read_lila_block("../../02_data/04_beispieldaten_rp/pegel-qvhs.lila")



# test hybrid
d4 <- read_lila_block("data/exp_lilahybrid.lila")


# read all einzel lila in example data rp
setwd("N:/projekte/19-009_hlnug-support19-22/bearbeitung/03_anpassung_ensemble_Rscript/02_data/04_beispieldaten_rp/einzelvorhersagedateien_lfu/20190908_07/")

files <- list.files(pattern=".*lila")

d <- read_lila_einzel(files[1])
d

ddf <- d %>% select(station, datenart)
ddf2 <- d %>% filter(station=="Rebbelroth", time<"2019-09-09 00:00:00")


ddf$time < "2019-09-09 00:00"
