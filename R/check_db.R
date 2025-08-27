#' @importFrom SVCleanR check_hochschule check_organisation
#' @param data Dataframe mit den zu prüfenden Daten
#' @param hochschule_col Name der Hochschulspalte (Standard: "hochschule")
#' @param hochschule_kurz_col Name der Kurzform-Hochschulspalte (Standard: "hochschule_kurz")
#' @param organisation_col Name der Organisationsspalte (Standard: "organisation")
#'
#' @export
check_db <- function(data,
                     hochschule_col = "hochschule",
                     hochschule_kurz_col = "hochschule_kurz",
                     organisation_col = "organisation") {

  # Erwartete Variablen definieren
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

  # Fehlende Variablen prüfen
  missing_cols <- setdiff(expected_vars, names(data))

  if (length(missing_cols) > 0) {
    message(crayon::red("\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")) 
    message(crayon::red("⛔ Fehlende Variablen:"))
    purrr::walk(missing_cols, ~ message(crayon::red(paste0("   ❌ ", .x))))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    Sys.sleep(3)
  }

  # Nur Checks ausführen, wenn Spalten existieren
  if (hochschule_col %in% names(data)) {
    check_hochschule(data[[hochschule_col]])
    Sys.sleep(2)
  }
  if (hochschule_kurz_col %in% names(data)) {
    check_hochschule(data[[hochschule_kurz_col]])
    Sys.sleep(2)
  }
  if (organisation_col %in% names(data)) {
    check_organisation(data, organisation_col)
    Sys.sleep(2)
  }
}
