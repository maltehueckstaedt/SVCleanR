# Beispielaufruf mit 3 Key-Variablen
#remotes::install_github("maltehueckstaedt/SVCleanR", force = TRUE)

library(SVCleanR)

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Update SVCleanR ----------------------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


getwd()
devtools::document()
devtools::install()


# 2. Dann Version erhöhen
usethis::use_version("patch")

# 3. Änderungen pushen
usethis::use_git_push()

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Update Test check_organisation ---------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# Mit devtools kannst du die aktuell geänderte Funktion (z.B. in deinem lokalen Paket "iuc") wie folgt neu laden:
devtools::load_all() # Passe den Pfad ggf. an, z.B. "../iuc" wenn das Paket im übergeordneten Verzeichnis liegt


test_data <- read.csv("vignettes/data/test_data_organisation.csv", stringsAsFactors = FALSE)

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Update Test is_valid_hochschule ---------------------------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

devtools::load_all() # Passe den Pfad ggf. an, z.B. "../iuc" wenn das Paket im übergeordneten Verzeichnis liegt
 
 
agent <- check_organisation(test_data, organisation_col = "organisation")
 

# Alle Datenextrakte (Zeilen, die Fehler erzeugt haben)
fails <- pointblank::get_data_extracts(agent)

# Nur den separator_check herausziehen
sep_fails <- fails$separator_check


check_hochschule(test_data$hochschule_kurz)

 
check_db(test_data) 

 