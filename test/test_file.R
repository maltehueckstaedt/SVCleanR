# Beispielaufruf mit 3 Key-Variablen
library(SVCleanR)

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Update SVCleanR ----------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


# getwd()
# devtools::document()
# devtools::install()

# # Git-Workflow für Versionsupdate
# # 1. Erst Pull ausführen (wegen Push-Konflikt)
# usethis::use_git_pull()

# # 2. Dann Version erhöhen
# usethis::use_version("minor")

# # 3. Änderungen pushen
# usethis::use_git_push()

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Update Test check_organisation ---------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# Mit devtools kannst du die aktuell geänderte Funktion (z.B. in deinem lokalen Paket "iuc") wie folgt neu laden:
devtools::load_all() # Passe den Pfad ggf. an, z.B. "../iuc" wenn das Paket im übergeordneten Verzeichnis liegt


test_data_org <- read.csv("data/test_data_organisation.csv", stringsAsFactors = FALSE)

# Zufällig einige NAs in der Organisationsspalte erzeugen (z.B. 5% der Zeilen)
set.seed(123) # für Reproduzierbarkeit
if ("organisation" %in% names(test_data_org)) {
  n <- nrow(test_data_org)
  na_indices <- sample(seq_len(n), size = ceiling(0.05 * n))
  test_data_org$organisation[na_indices] <- NA
}

# prüfen ob die Strings > 1000 Zeichen lang sind
check_organisation(test_data_org) |> View()

 