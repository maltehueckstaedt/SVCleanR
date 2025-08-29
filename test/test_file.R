# Beispielaufruf mit 3 Key-Variablen
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
usethis::use_version("minor")

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
 
 
agent <- pb_check_organisation(test_data, organisation_col = "organisation")
 

# Alle Datenextrakte (Zeilen, die Fehler erzeugt haben)
fails <- pointblank::get_data_extracts(agent)

# Nur den separator_check herausziehen
sep_fails <- fails$separator_check


check_hochschule(test_data$hochschule_kurz)

 
check_db(test_data) 






sql_file <- "vignettes/data/hochschulen_namen_kuerzel.sql"
sql_text <- readLines(sql_file, warn = FALSE)

hochschule_lang <- str_match(sql_text, "\\([0-9]+, '([^']+)'")[,2]
hochschule_kurz <- str_match(sql_text, "\\([0-9]+, '[^']+', '([^']+)'")[,2]

hochschule_lang <- hochschule_lang[!is.na(hochschule_lang)]
hochschule_kurz <- hochschule_kurz[!is.na(hochschule_kurz)]

expected_vars <- c(
  "id", "anmerkungen", "dozierende", "ects", "fakultaet", "hochschule",
  "hochschule_kurz", "jahr", "kursbeschreibung", "kursformat_original",
  "kursformat_recoded", "lehrtyp", "lernmethode", "lernziele", "literatur",
  "module", "nummer", "organisation_orig", "organisation", "pfad",
  "pruefung", "scrape_datum", "semester", "sprache_original", "sprache_recoded",
  "studiengaenge", "sws", "teilnehmerzahl", "titel", "url",
  "voraussetzungen", "zusatzinformationen", "institut", "data_analytics_ki",
  "softwareentwicklung", "nutzerzentriertes_design", "it_architektur",
  "hardware_robotikentwicklung", "quantencomputing", "lehr_und_forschungsbereich",
  "studienbereich", "faechergruppe", "luf_code", "stub_code", "fg_code",
  "matchingart"
)

agent <- create_agent(tbl = ~ test_data) %>%
col_exists(
    columns = all_of(expected_vars),
    actions = action_levels(stop_at = 0.001)
  ) %>%
  col_vals_in_set(
    columns = vars(hochschule),
    set = werte,
    actions = action_levels(stop_at = 0.001)
  ) %>%
  col_vals_in_set(
    columns = vars(hochschule_kurz),
    set = hochschule_kurz,
    actions = action_levels(stop_at = 0.001)
  ) |> 
  interrogate()

agent %>%
  get_agent_report()












agent <- pointblank::create_agent(tbl = ~ test_data) |>
  # 1) nur korrektes " ; "
  pointblank::col_vals_regex(
    columns = pointblank::vars(organisation),
      regex = "^(?:[^;]*(?:\\s;\\s))*[^;]*$",
      preconditions = not_empty,
      actions = pointblank::action_levels(stop_at = 0.001)
    ) |>
  interrogate()

agent %>%
  get_agent_report()


