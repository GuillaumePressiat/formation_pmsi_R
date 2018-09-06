

library(requetr)
# library(nomensland)
library(dplyr, warn.conflicts = FALSE)
library(pmeasyr)

dms <- get_table('ghm_dms_nationales')

dms_1 <- dms %>% filter(ghs == "")

dms_2 <- dms %>% filter(ghs != "")


# premier calcul : sans enlever les outliers
library(MonetDBLite)
library(DBI)
dbdir <- "~/Documents/data/monetdbpsl"
con <- src_monetdblite(dbdir)

reel <- tbl_mco(con, 17, 'rsa_rsa') %>% 
  filter(duree > 0, rsacmd != '28', rsacmd != '90', ghm != '23Z02Z') %>% 
  group_by(noghs, ghm, anseqta) %>% 
  summarise(journees = sum(duree),
            sejours  = n()) %>% 
  collect()

ip_1 <- reel %>% 
  inner_join(dms_1, by = c('ghm' = 'ghm', 'anseqta' = 'anseqta'))
ip_2 <- reel %>% 
  inner_join(dms_2, by = c('ghm' = 'ghm', 'noghs' = 'ghs', 'anseqta' = 'anseqta'))
ip <- bind_rows(ip_1, ip_2)



resultat_ip <- ip %>% 
  mutate(theorique = sejours * dms_n, 
         ip = journees / theorique) %>% 
  ungroup()
summarise(resultat_ip, 
          reel = sum(journees),
          theorique = sum(theorique),
          ip = reel / theorique)
anti_join(reel, ip)
lib_ghm <- get_table('ghm_ghm_regroupement', 2017)

# deuxième calcul : on enlève les outliers
reel <- tbl_mco(con, 17, 'rsa_rsa') %>% 
  filter(duree > 0,
         substr(um, 4,4) == 'C',
         rsacmd != '28', rsacmd != '90', substr(ghm,1,5) != '23Z02') %>% 
  group_by(noghs, ghm, anseqta, duree) %>% 
  count() %>% 
  collect() %>% 
  ungroup()

reel_1 <- reel %>% 
  inner_join(dms_1, by = c('ghm' = 'ghm', 'anseqta' = 'anseqta'))
reel_2 <- reel %>% 
  inner_join(dms_2, by = c('ghm' = 'ghm', 'noghs' = 'ghs', 'anseqta' = 'anseqta'))
reel <- bind_rows(reel_1, reel_2) %>% 
  filter(duree >= borne_basse, duree <= borne_haute)

resultat_ip <- reel %>% 
  mutate(theorique = n * dms_n) %>% 
  group_by(ghm, noghs) %>% 
  summarise(journees = sum(duree * n),
            theorique = sum(theorique),
            n = sum(n)) %>% 
  ungroup() 

summarise(resultat_ip, 
          reel = sum(journees),
          theorique = sum(theorique),
          ip = reel / theorique)



lib_ghm <- get_table('ghm_ghm_regroupement', 2017)


