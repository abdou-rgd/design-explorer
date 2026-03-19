setwd("c:/Users/abdou/Desktop/ClaudeProjets/app")
library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)

source("../R/parse_design_outputs.R", local = TRUE)
source("../R/report_design.R",        local = TRUE)

# Test rapide avec les fichiers exemple 2 (optimisation)
ext_file <- "../docs/papers/bauer2021/examples/example2/warfarin2.ext"
shk_file <- "../docs/papers/bauer2021/examples/example2/warfarin2.shk"

if (file.exists(ext_file)) {
  ext <- read_ext(ext_file)
  cat("✅ read_ext() OK —", nrow(ext), "lignes,", n_distinct(ext$table_no), "bloc(s)\n")
  cat("   OFV final:", get_ofv(ext), "\n")
  rse <- get_rse(ext)
  cat("   RSE table:\n")
  print(rse)
} else {
  cat("⚠ Fichier exemple introuvable:", ext_file, "\n")
}

if (file.exists(shk_file)) {
  shk <- read_shk(shk_file)
  cat("\n✅ read_shk() OK —", nrow(shk), "lignes\n")
  ri <- get_relativeinf(shk)
  cat("   RELATIVEINF:\n")
  print(ri)
} else {
  cat("⚠ Fichier .shk introuvable:", shk_file, "\n")
}

cat("\n✅ App prête — lancer avec : shiny::runApp('app/')\n")
