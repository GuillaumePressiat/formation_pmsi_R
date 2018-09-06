library(pmeasyr)
library(dplyr, warn.conflicts = F)
#library(requetr)
library(nomensland)

# Importer les tarifs GHS
tarifs_ghs <- dplyr::distinct(get_table('tarifs_mco_ghs'), ghs, anseqta, .keep_all = TRUE)

# Importer les tarifs des suppléments
tarifs_supp <- get_table('tarifs_mco_supplements') %>% mutate_if(is.numeric, tidyr::replace_na, 0)


# 15.1 Accéder aux tarifs depuis R

library(pmeasyr)

noyau_pmeasyr(
  finess = '940100035',
  annee  = 2018,
  mois   = 7,
  path   = '~/Documents/data/mco',
  progress = FALSE,
  lib = FALSE, 
  tolower_names = TRUE
) -> p

adezip(p, type = "out")
# adelete(p)
vrsa <- vvr_rsa(p)
vano <- vvr_ano_mco(p)

# vrsa <- vvr_rsa(con, 17)
# vano <- vvr_ano_mco(con,  17)


# Sans PO, ni PIE, diap.
resu <- vvr_mco(
  vvr_ghs_supp(rsa = vrsa, ano =  vano, tarifs = tarifs_ghs),
  vvr_mco_sv(vrsa, vano)
)

epmsi_mco_sv(resu)



# avec PO, ni PIE, diap, et tout lancé dans une seule étape
resu <- vvr_mco(
  vvr_ghs_supp(rsa = vrsa, 
               tarifs = tarifs_ghs, 
               supplements =  tarifs_supp, 
               ano = vano, 
               porg = ipo(p), 
               diap = idiap(p), 
               pie = ipie(p), 
               full = TRUE,
               cgeo = 1.07, 
               prudent = NULL, 
               bee = FALSE),
  vvr_mco_sv(vrsa, vano)
)
epmsi_mco_sv(resu)
epmsi_mco_rav(resu)
