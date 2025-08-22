#' Überprüft Organisationseinträge auf verschiedene Fehler
#'
#' Diese Funktion prüft die Werte einer Organisationsspalte in einem Dataframe auf verschiedene
#' häufige Fehlerquellen. Dazu gehören unter anderem falsche oder fehlende Trennzeichen,
#' unerwünschte Sonderzeichen, zu lange Einträge und andere Auffälligkeiten.
#'
#' Folgende Checks werden durchgeführt:
#' \itemize{
#'   \item Falsche oder fehlende Trennung mit " ; " (Leerzeichen, Semikolon, Leerzeichen)
#'   \item Enthält das Zeichen "|"
#'   \item Semikolon ohne beidseitiges Leerzeichen
#'   \item Semikolon nur mit links oder rechts Leerzeichen
#'   \item Zu lange Einträge (> 1000 Zeichen)
#'   \item Enthält das Zeichen ">"
#' }
#'
#' Leere oder fehlende Werte werden ignoriert.
#'
#' @param data Dataframe mit einer Spalte für Organisationen.
#' @param organisation_col Name der Organisationsspalte als Zeichenkette (Standard: "organisation").
#'
#' @return Dataframe mit problematischen Einträgen, deren Zeilennummer, Originalwert und Art des Problems.
#'
#' @importFrom dplyr mutate filter select pull row_number distinct %>%
#' @importFrom stringr str_detect str_trim
#' @importFrom purrr discard
#' @importFrom crayon red
#' @export
check_organisation <- function(data, organisation_col = "organisation") {

  # Prüfen, ob die gewünschte Spalte im Dataframe existiert
  if (!organisation_col %in% colnames(data)) {
    stop(paste("Spalte '", organisation_col, "' nicht im Dataframe gefunden."))
  }

  # Hilfsspalten erzeugen: Länge, zu lang (>1000), enthält '>'
  data <- dplyr::mutate(
    data,
    orga_length = nchar(.data[[organisation_col]]), # Zeichenlänge des Eintrags
    orga_too_long = !is.na(.data[[organisation_col]]) & orga_length > 1000, # zu lange Einträge
    orga_has_arrow = !is.na(.data[[organisation_col]]) & stringr::str_detect(.data[[organisation_col]], ">") # enthält '>'
  )

  # Haupt-Check: Problematische Trennzeichen und Sonderzeichen erkennen
  results <- data %>%
    dplyr::mutate(
      row_index = dplyr::row_number(), # Zeilennummer für spätere Ausgabe
      original_value = .data[[organisation_col]], # Originalwert sichern
      is_na_or_empty = is.na(.data[[organisation_col]]) | .data[[organisation_col]] == "", # NA oder leer?
      # Problemtyp bestimmen (nur, wenn nicht NA/leer)
      problem_type = dplyr::case_when(
        is_na_or_empty ~ NA_character_,
        stringr::str_detect(.data[[organisation_col]], "\\|\\s") ~ "Enthält '| '",
        stringr::str_detect(.data[[organisation_col]], "\\|") ~ "Enthält '|'",
        stringr::str_detect(.data[[organisation_col]], "[^\\s];[^\\s]") ~ "Semikolon ohne Leerzeichen",
        stringr::str_detect(.data[[organisation_col]], "\\s;[^\\s]") ~ "Nur links Leerzeichen",
        stringr::str_detect(.data[[organisation_col]], "[^\\s];\\s") ~ "Nur rechts Leerzeichen",
        TRUE ~ NA_character_
      ),
      has_problem = !is.na(problem_type) # TRUE, wenn Problem erkannt
    ) %>%
    dplyr::filter(!is_na_or_empty & has_problem) %>% # nur problematische, nicht-leere Zeilen
    dplyr::select(row_index, original_value, has_problem, problem_type) # relevante Spalten auswählen

  # Statistiken berechnen: Gesamt, NA/leer, geprüft, problematisch
  total_rows <- nrow(data)
  na_rows <- data %>% dplyr::pull(!!organisation_col) %>% {sum(is.na(.) | . == "")}
  checked_rows <- total_rows - na_rows
  problem_rows <- nrow(results)

  # Übersichtliche Ausgabe der Statistiken
  message("\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("                         ORGANISATIONEN: TRENNZEICHEN-CHECK                    ")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message(sprintf("• Gesamte Zeilen:           %d", total_rows))
  message(sprintf("• NA/Leere Zeilen:          %d", na_rows))
  message(sprintf("• Überprüfte Zeilen:        %d", checked_rows))
  message(sprintf("• Problematische Zeilen:    %d", problem_rows))
  message(sprintf("• Problematische Zeilen (%%): %.2f%%", ifelse(checked_rows > 0, problem_rows/checked_rows*100, 0)))

  Sys.sleep(1) # kurze Pause für bessere Lesbarkeit

  # Zu lange Organisationseinträge (>1000 Zeichen) ausgeben
  long_orgas <- dplyr::filter(data, orga_too_long & !is.na(.data[[organisation_col]]))
  #browser()
  if (nrow(long_orgas) > 0) {
    message(crayon::red("\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("                ⚠️  SEHR LANGE ORGANISATIONSEINTRÄGE (> 1000 ZEICHEN)             "))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))

    long_orgas %>%
      dplyr::mutate(orga_wert = long_orgas[1]) %>%
      dplyr::mutate(row_in_data = match(orga_wert, data[[organisation_col]])) %>%
      dplyr::distinct(row_in_data, orga_wert) %>%
      purrr::pwalk(~ message(sprintf("• Zeile %d: %s", ..1, ..2)))

    message("Hinweis: Organisationsvariable ggf. granular cleanen. Gibt es eine Pfad-Variable, die helfen kann? Gibt es andere Variablen, die eine granulareres Cleaning ermöglicht?")
  }

  # Einträge mit Pfeilzeichen ('>') ausgeben
  arrow_orgas <- dplyr::filter(data, orga_has_arrow & !is.na(.data[[organisation_col]]))
  if (nrow(arrow_orgas) > 0) {
    Sys.sleep(1)
    message(crayon::red("\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("                     ⚠️  EINTRÄGE MIT PFEILZEICHEN ('>') GEFUNDEN                 "))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    # Korrigierte Ausgabe der betroffenen Zeilen
    # Die echte Zeilennummer aus dem Original-Dataframe mit tidyverse ermitteln und ausgeben
   arrow_orgas %>%
    dplyr::mutate(orga_wert = .data[[organisation_col]]) %>%
    dplyr::mutate(row_in_data = match(orga_wert, data[[organisation_col]])) %>%
    dplyr::distinct(row_in_data, orga_wert) %>%
    purrr::pwalk(~ message(sprintf("• Zeile %d: %s", ..1, ..2)))
    message("Hinweis: Pfeilzeichen ('>') sollten entfernt werden.")
  }

  # Alle nicht-leeren Organisationseinträge extrahieren
  orga_values <- data %>%
    dplyr::filter(!is.na(.data[[organisation_col]]) & .data[[organisation_col]] != "") %>%
    dplyr::pull(!!organisation_col)

  # Splitten an ' ; ', trimmen, leere entfernen, dann Anzahl unique Werte bestimmen
  orga_einzelwerte <- unlist(strsplit(orga_values, "\\s*;\\s*")) %>%
    stringr::str_trim() %>%
    purrr::discard(~ .x == "" | is.na(.x))
  n_unique_orga <- length(unique(orga_einzelwerte))

  Sys.sleep(1)

  # Ausgabe: Anzahl unique Organisationseinheiten
  message("\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("                    ANALYSE: UNIQUE ORGANISATIONSEINHEITEN           ")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message(sprintf("• Anzahl uniquer Organisationen (nach Split an ' ; '): %d", n_unique_orga))
  if (n_unique_orga > 1000) {
    message(crayon::red("⚠️ Hinweis: Mehr als 1000 verschiedene Organisationseinheiten – Daten prüfen!"))
  }

  # Problematische Trennzeichen ausgeben, falls vorhanden
  if (problem_rows > 0) {
    Sys.sleep(1)
    message(crayon::red("\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("                    ⚠️  PROBLEMATISCHE TRENNZEICHEN GEFUNDEN                      "))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    message(crayon::red("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"))
    # Nur einzigartige Problemfälle (nach Wert & Typ)
    unique_results <- dplyr::distinct(results, original_value, problem_type, .keep_all = TRUE)

    # Für jeden Problemtyp die betroffenen Zeilen ausgeben
    problem_types <- unique(unique_results$problem_type)
    for (ptype in problem_types) {
      message("\n--------------------------------------------------------------------------------")
      message(sprintf("PROBLEMTYP: %s", toupper(ptype)))
      message("--------------------------------------------------------------------------------")
      values <- dplyr::filter(unique_results, problem_type == ptype)
      for (i in seq_len(nrow(values))) {
        message("\n--------------------------------------------------------------------------------")
        message(sprintf("• Zeile %d: %s", values$row_index[i], values$original_value[i]))
        message("--------------------------------------------------------------------------------")
      }
    }

  } else {
    # Alles ok!
    message("\n✅ Alle Organisationseinträge sind korrekt formatiert mit ' ; '!")
  }
}
