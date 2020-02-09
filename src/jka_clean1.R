library(googlesheets)
library(RCurl)

config <- read_yaml("config.yaml")

source(config$toolbox, encoding = "UTF-8")

filter <- dplyr::filter # voorkom verwarring met stats::filter

gd_nip_xpr <- gs_title("Jan Kruitarchief LvdA")

kruit_archief <- gd_nip_xpr %>% gs_read(ws = "jk_lijst") 

karch1 <- kruit_archief %>% 
  mutate(lengte_ss = as.integer(60 * mm + ss)) %>% 
  select(-Genre, -Jaar, -Schijfnr, -mm, -ss)

karch2 <- karch1 %>% 
  separate(Naam, into = c("naam_1", "naam_2"), sep = ":")

karch3 <- karch2 %>% 
  separate(naam_1, into = c("naam_1a", "naam_1b"), sep = " - ") %>% 
  separate(naam_2, into = c("naam_2a", "naam_2b"), sep = " - ") 

karch4 <- karch3 %>% 
  mutate(track = str_replace(`Tracknr.`, pattern = "([0-9]{1,2}).*", replacement = "\\1")) %>% 
  select(-Tracknr.)

karch5 <- karch4 %>% group_by(Archiefnr.) %>% 
  mutate(rank1 = dense_rank(naam_1a),
         rank2 = dense_rank(naam_2a)) %>% 
  select(Archiefnr., track, rank1, rank2, everything())

cz_lcs <- function(string1, string2) {
  s1 <- unlist(strsplit(string1, split = ""))
  s2 <- unlist(strsplit(string2, split = ""))
  
  num <- matrix(0, nchar(string1), nchar(string2))
  maxlen <- 0
  start <- 0
  
  for (i1 in 1:nchar(string1)) {
    for (j1 in 1:nchar(string2)) {
      if (s1[i1] == s2[j1]) {
        if (i1 == 1 || j1 == 1) {
          num[i1, j1] <- 1
          start <- 1
        }
        else {
          num[i1, j1] <- 1 + num[i1 - 1, j1 - 1]
        }
        if (num[i1, j1] > maxlen) {
          maxlen <- num[i1, j1]
          start <- i1
        }
      }
    }
  }
  
  result <- c(maxlen, start)
}

l1 <- cz_lcs("3abcdefghi", "abcdefh")
