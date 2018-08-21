
# Chargerment des packages
library(pmeasyr)
library(dplyr, warn.conflicts = FALSE)
library(nomensland)
# library(requetr)

# ##################################
# import des données de PSL 2017 M12
# ##################################

p <- noyau_pmeasyr(
  finess = '750100125',
  annee  = 2017,
  mois   = 12,
  path   = '~/Documents/data/mco',
  tolower_names = TRUE
)

# Dézipper le fichier rsa, le tra et le ano
adezip(p, liste = c('rsa', 'tra', 'ano'), type = "out")

# Dézipper si l'archive provient de l'intranet du DIM Siège
adezip3(finess = '750100125', path = '~/Documents/data/mco', 'MCO_OUT_00066_201712.zip')
adelete(p)

# Importer les rsa
rsa <- irsa(p)
rsa
#rsa %>% purrr::map(glimpse)

# Importer le tra
tra <- itra(p)

# Importer le anohosp
ano <- iano_mco(p)
# Erreurs A20 / formats ANO incorrect :
readr::problems(ano)

rsa$rsa <- rsa$rsa %>% inner_tra(tra)

# ##################################
# Exemple   de     requête    simple
# ##################################

# Compter les ptg par le GHM
ptg1 <- rsa$rsa %>% 
  filter(substr(ghm, 1, 5) == '08C24')

# Compter les ptg par les actes NFKA
ptg2 <- rsa$actes %>% 
  filter(substr(cdccam, 1, 4) == 'NFKA') %>% 
  distinct(cle_rsa)

anti_join(ptg2, ptg1, by = 'cle_rsa') %>% 
  inner_join(rsa$rsa, by = 'cle_rsa') %>% 
  count(ghm)

rsa <- irsa(p, typi = 6)

rsa$rsa %>% 
  filter(grepl('NFKA', actes)) %>% 
  count(rghm = substr(ghm,1,5), sort = TRUE)

# Combien de séjours avec dp obésité 
obe <- rsa$rsa %>% 
  filter(grepl('E66', dp))

# ####################################
# Faire ces mêmes requête avec requetr
# ####################################
rsa <- irsa(p, typi = 6)
rsa$rsa <- rsa$rsa %>% inner_tra(tra)
rsa_r <- prepare_rsa(rsa)

requete(rsa_r, list(ghm = '08C24'), vars = c('ghm', 'duree', 'nas', 'norss'))

nkfas <- get_table('ccam_actes') %>% 
  filter(substr(code, 1, 4) == 'NFKA') %>% 
  pull(code)
requete(rsa_r, list(actes = nkfas), vars = c('ghm', 'duree', 'actes', 'um'))

requete(rsa_r, list(actes = nkfas), vars = c('ghm', 'duree', 'actes', 'um')) %>% 
  count(duree < 7) %>% 
  mutate(`%` = scales::percent(n / sum(n)))

requete(rsa, list(diags = 'E66', positions_diags = 'dp'), vars = c('ghm', 'dp', 'duree', 'actes', 'um'))

requete(rsa, list(diags = 'E66', positions_diags = 'dp'), vars = c('ghm', 'dp', 'duree', 'actes', 'um')) %>% 
  count(um, sort = TRUE)

# #####################
# Chirurgie bariatrique
# #####################
dl <- get_dictionnaire_listes()
View(dl)
bari <- get_liste("chir_bariatrique_total")

requete(rsa_r, bari)
requete(rsa_r, bari, vars = c('ghm', 'nas')) %>% 
  inner_join(ano, by = 'cle_rsa') %>% 
  filter(cok) %>% 
  count(noanon, sort = TRUE)


# ################################
# Chimiothérapie intra péritonéale
# ################################

dl <- get_dictionnaire_listes()
View(dl)
chip <- get_liste("chip")

requete(rsa_r, chip)
requete(rsa_r, chip, vars = c('ghm', 'nas')) %>% 
  inner_join(ano, by = 'cle_rsa') %>% 
  filter(cok) %>% 
  count(noanon, sort = TRUE)

# ###################
# Import des RUM
# Requête sur les RUM
# Croiser avec les UM
# ###################

adezip(p, liste = 'rss', type = "in")

adezip3(finess = '750100125', path = '~/Documents/data/mco', 'MCO_IN_00066_201712.zip')
adelete(p)

# Importer les rum
rum <- irum(p)

# transposer les diagnostics

rum <- tdiag(rum)

# #################################
# base de données avec PSL 2017 M12
# #################################
library(pmeasyr)
library(MonetDBLite)
library(DBI)
library(dplyr, warn.conflicts = F)
dbdir <- "~/Documents/data/monetdbpsl"
con <- src_monetdblite(dbdir)

db_mco_out(con, p)

# Requête dans la base de données désormais (plus rapide, sans import)

# Requête écrite
tbl_mco(con, 17, 'rsa_rsa') %>% 
  filter(dp %like% 'E66%') %>% 
  collect()

# Requête avec requetr
requete_db(con, 17, list(diags = 'E66', positions_diags = 'dp'), vars = c('dp', 'nas'))

# ###########################
# RSF - ACE avec PSL 2017 M12
# ###########################

p$path <- '~/Documents/data/rsf'
db_rsf_out(con, p)

# Combien de consultations en hépato gastro entéro ?
tbl_rsf(con, 17, 'rafael_c') %>% 
  filter(codact %in% c('C', 'CS'), specex == '08') %>% 
  count()

# Combien de passages aux urgences ?
tbl_rsf(con, 17, 'rafael_b') %>% 
  filter(substr(codact, 1, 3) == 'ATU') %>% 
  count()

# Lier les ace aux séjours
tbl_rsf(con, 17, 'rafael_b') %>% 
  filter(substr(codact, 1, 3) == 'ATU') %>% 
  inner_join(tbl_rsf(con, 17, 'rafael_ano') %>% select(numseq, noanon, cok) %>% filter(cok), by = 'numseq') %>% 
  left_join(tbl_mco(con, 17, 'rsa_ano') %>% select(nas, noanon, cok) %>% filter(cok) %>% mutate(sejour = 1), by = 'noanon') %>% 
  group_by(noanon) %>% 
  summarise(les_deux = !is.na(sum(sejour, na.rm = TRUE))) %>% 
  count(les_deux) %>% 
  ungroup() %>% 
  collect() %>% 
  mutate(`%` = scales::percent(n / sum(n)))

# se déconnecter de la bdd monetdb
dbDisconnect(con$con)

# #################################
# base de données AP-HP EJ 2017 M12 (restart RStudio cmd + shift + F10)
# #################################
library(pmeasyr)
library(MonetDBLite)
library(DBI)
library(dplyr, warn.conflicts = F)
dbdir <- "~/Documents/data/monetdb"
con <- src_monetdblite(dbdir)

library(nomensland)
cbari <- requete_db(con, 17, get_liste('chir_bariatrique_total'), vars = c('nas', 'duree'))

# Fonction par année : 
bariatrique <- function(an) {
  requete_db(con, an, get_liste('chir_bariatrique_total'), vars = c('nas', 'duree')) %>% mutate(annee = 2000 + an)
}

bariatrique(17)

11:17 %>% 
  purrr::map(bariatrique) %>% 
  bind_rows %>% 
  count(annee)

# Lancer plusieurs requêtes
re <- lancer_requete_db(con, 17, get_all_listes('Recours Exceptionnel'), vars = c('nas', 'duree'))

# se déconnecter de la bdd monetdb
dbDisconnect(con$con)
