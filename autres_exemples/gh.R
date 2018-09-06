

# Faire une base par GH

liste_hop <- c('750100125', '940100035')

library(pmeasyr)
library(dplyr, warn.conflicts = FALSE)

p <- noyau_pmeasyr(
  annee = 2018, 
  mois  = 7, 
  path  = '~/Documents/data/mco', 
  progress = FALSE,
  tolower_names = TRUE
)


rsa_gh <- liste_hop %>% 
  purrr::map(function(i){
    p$finess <- i;
    adezip(p, type = "out", liste = 'rsa')
    irsa(p) %>% 
      purrr::map(function(table)
        {mutate(table, f = i)})})

rsa_gh_fin <- list()
rsa_gh_fin$rsa    <- rsa_gh %>% purrr::map('rsa')    %>% bind_rows()
rsa_gh_fin$rsa_um <- rsa_gh %>% purrr::map('rsa_um') %>% bind_rows()
rsa_gh_fin$das    <- rsa_gh %>% purrr::map('das')    %>% bind_rows()
rsa_gh_fin$actes  <- rsa_gh %>% purrr::map('actes')  %>% bind_rows()

rsa_gh_fin

ano_gh <- liste_hop %>% 
  purrr::map(function(i){
    p$finess <- i;
    adezip(p, type = "out", liste = 'ano')
    iano_mco(p) %>% mutate( f = i)}) %>% 
  bind_rows()

# Chainer en intra GH

get_hop_from_gh <- function(table, fin){
    table %>% filter(f == fin)
}

rsa_psl <- get_hop_from_gh(rsa_gh_fin$rsa, '750100125')
rsa_cfx <- get_hop_from_gh(rsa_gh_fin$rsa, '940100035')

ano_psl <- get_hop_from_gh(ano_gh, '750100125')
ano_cfx <- get_hop_from_gh(ano_gh, '940100035')

rsa_psl %>%
  select(cle_rsa, ghm, duree) %>% 
  inner_join(ano_psl %>% filter(cok) %>% select(cle_rsa, noanon, nosej), by = 'cle_rsa') %>% 
  left_join(
    rsa_cfx %>%
      select(cle_rsa, ghm, duree) %>% 
      inner_join(ano_cfx %>% filter(cok) %>% select(cle_rsa, noanon, nosej), by = 'cle_rsa'),
    by = 'noanon', 
    suffix = c('_psl', '_cfx')
  ) %>% 
  filter(!is.na(ghm_cfx)) %>% 
  mutate(delta = nosej_cfx - (nosej_psl + duree_psl)) %>% 
  filter(delta >= 0) %>% 
  arrange(noanon, delta) %>% 
  distinct(noanon, .keep_all = TRUE) %>% 
  filter(delta < 5) %>% # View
  count(ghm_psl, ghm_cfx) %>% 
  View
