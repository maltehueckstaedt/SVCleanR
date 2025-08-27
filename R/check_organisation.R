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
  stopifnot(is.data.frame(data))
  if (!organisation_col %in% names(data)) stop(paste("Spalte", organisation_col, "nicht gefunden."))

  x <- data[[organisation_col]]
  total_rows <- length(x)
  na_rows <- sum(is.na(x) | x == "")
  checked_rows <- total_rows - na_rows

  # Prüfen, ob str_squish() sinnvoll wäre
  squish_changes <- sum(x != stringr::str_squish(x), na.rm = TRUE)
  squish_pct <- if (checked_rows > 0) squish_changes / checked_rows * 100 else 0
  squish_examples <- tibble::tibble(
    original = x,
    squished = stringr::str_squish(x)
  ) %>%
    dplyr::filter(original != squished) %>%
    dplyr::slice_head(n = 5)

  result <- tibble::tibble(original_value = x) %>%
    dplyr::mutate(
      problem_type = purrr::map_chr(original_value, function(val) {
        if (is.na(val) || val == "") return(NA_character_)

        probs <- c()
        if (stringr::str_detect(val, "Bachelor|Master|Diplom|Staatsexamen|Staatsexamens|SS|WS|B\\.Sc\\.|M\\.Sc\\.|B\\.A\\.|M\\.A\\.")) probs <- c(probs, "Verdacht auf Nicht-Organisationseinheiten")
        if (stringr::str_detect(val, "\\|\\s"))        probs <- c(probs, "Enthält '| '")
        if (stringr::str_detect(val, "\\|"))           probs <- c(probs, "Enthält '|'")
        if (stringr::str_detect(val, "(?<!\\s);(?!\\s)")) probs <- c(probs, "Semikolon ohne Leerzeichen")
        if (stringr::str_detect(val, "\\s;(?!\\s)"))    probs <- c(probs, "Semikolon: nur links Leerzeichen")
        if (stringr::str_detect(val, "(?<!\\s);\\s"))   probs <- c(probs, "Semikolon: nur rechts Leerzeichen")
        if (nchar(val) > 1000)                          probs <- c(probs, "Eintrag zu lang (>1000)")
        if (stringr::str_detect(val, ">"))              probs <- c(probs, "Enthält '>'")
        if (val != stringr::str_squish(val)) probs <- c(probs, "Überflüssige Leerzeichen")
        if (length(probs) == 0) NA_character_ else paste(probs, collapse = ", ")
      })
    ) %>%
    dplyr::filter(!is.na(problem_type))

  # Zeilen mit echten Problemen (ohne "Verdacht auf Nicht-Organisationseinheiten")
  problem_rows <- result %>%
    dplyr::filter(!problem_type %in% "Verdacht auf Nicht-Organisationseinheiten") %>%
    dplyr::filter(!stringr::str_detect(problem_type, "^Verdacht auf Nicht-Organisationseinheiten$")) %>%
    nrow()

  problem_pct <- if (checked_rows > 0) problem_rows / checked_rows * 100 else 0

  # Einzigartige Organisationseinheiten extrahieren
  unique_orgs <- data %>%
    dplyr::filter(!is.na(.data[[organisation_col]]) & .data[[organisation_col]] != "") %>%
    dplyr::pull(.data[[organisation_col]]) %>%
    stringr::str_split("\\s*;\\s*") %>%
    purrr::flatten_chr() %>%
    stringr::str_trim() %>%
    unique() %>%
    discard(~ .x == "")

  n_unique_orgs <- length(unique_orgs)

  # Übersichtliche Ausgabe der Statistiken
  message("\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("                                 ORGANISATIONEN                                   ")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")

  message("\n:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
  message("                                    ÜBERSICHT            ")
  message(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n")

  message(sprintf("• Gesamte Zeilen:             %d", total_rows))
  message(sprintf("• NA/Leere Zeilen:            %d", na_rows))
  message(sprintf("• Überprüfte Zeilen:          %d", checked_rows))
  message(sprintf("• Problematische Zeilen:      %d", problem_rows))
  message(sprintf("• Problematische Zeilen (%%): %.2f%%", problem_pct))
  message(sprintf("• Einzigartige Organisationen: %d", n_unique_orgs))

  if (nrow(result) > 0) {
    problem_summary <- result %>%
      tidyr::separate_rows(problem_type, sep = ",\\s*") %>%
      dplyr::count(problem_type, sort = TRUE)

    message("\n⛔ Achtung: Es wurden folgende Probleme gefunden:\n")
    purrr::walk2(problem_summary$problem_type, problem_summary$n, ~ {
      message(sprintf("❌ %s: %d", .x, .y))
    })
    message("\n🔔 Hinweis: Probleme auf der Variable `organisation` bitte beheben und Check erneut durchführen.\n")
  }

  # Ergebnis nach problem_type sortieren (alphabetisch)
  if ("problem_type" %in% names(result)) {
    result <- result %>%
      dplyr::arrange(problem_type)
  }

  return(result)
}