# Beispielaufruf mit 3 Key-Variablen
library(SVCleanR)

raw_data_bremen_to_classify_alt <- get_unclassified_data(
  raw_data_bremen,
  "C:/SV/HEX/Scraping/data/single_universities/Universitaet_Bremen/db_data_universitaet_bremen.rds",
  key_vars = c("titel", "nummer")
)

getwd()
devtools::document()
devtools::install()

# Git-Workflow für Versionsupdate
# 1. Erst Pull ausführen (wegen Push-Konflikt)
usethis::use_git_pull()

# 2. Dann Version erhöhen
usethis::use_version("minor")

# 3. Änderungen pushen
usethis::use_git_push()