#' Prüft Hochschulnamen und Kürzel
#'
#' Diese Funktion gleicht Eingaben mit der beigelegten SQL-Liste 
#' (`hochschulen_namen_kuerzel.sql`) ab, die in `inst/extdata/` 
#' im Paket \pkg{SVCleanR} enthalten ist. 
#' Es werden sowohl die ausgeschriebenen Namen als auch die Kürzel berücksichtigt.
#' Zusätzlich wird eine Übersicht mit \code{message} ausgegeben. 
#' 
#' Hinweise:
#' - Es wird gemeldet, wenn mehrere unterschiedliche Hochschulen im Vektor vorkommen.
#' - Nicht gefundene Werte, leere Strings oder `NA` werden explizit aufgelistet.
#' - Die aktuelle Referenzliste der gültigen Werte ist hier einsehbar:
#'   \url{http://srv-data01:30080/hex/hex/-/blob/main/hochschulen_namen_kuerzel.sql?ref_type=heads}
#'
#' @param x Ein Character-Vektor mit Namen oder Kürzeln, die überprüft werden sollen.
#'
#' @return Ein logischer Vektor: `TRUE`, wenn der Eintrag einer bekannten Hochschule 
#' entspricht, sonst `FALSE`.
#'
#' @examples
#' is_valid_hochschule(c("Universität Mannheim", "U Mannheim", ""))
#' is_valid_hochschule(c("Universität Mannheim", "Universität Heidelberg"))
#'
#' @importFrom stringr str_detect str_replace_all str_split
#' @export
check_hochschule <- function(x) {
    .get_hochschulen <- local({
      hochschulen <- NULL
      
      function() {
        if (is.null(hochschulen)) {
          sql_file <- system.file("extdata", "hochschulen_namen_kuerzel.sql", 
                                  package = "SVCleanR")
          sql_text <- readLines(sql_file, warn = FALSE)
          
          # Nur Zeilen mit Werten extrahieren
          lines <- sql_text[stringr::str_detect(sql_text, "^\\(")]
          
          # Aus jeder Zeile die Inhalte zwischen Hochkommas holen
          matches <- stringr::str_match_all(lines, "'([^']*)'")
          
          # Für jede Zeile: 1. Name, 2. Kürzel → zusammenführen
          vals <- unlist(lapply(matches, function(m) m[,2][1:2]))
          
          hochschulen <<- unique(vals)
        }
        hochschulen
      }
    })
  valid_list <- .get_hochschulen()
  res <- x %in% valid_list
  
  # Statistiken
  total_rows   <- length(x)
  na_rows      <- sum(is.na(x) | x == "")
  checked_rows <- total_rows - na_rows
  problem_rows <- sum(!res & !is.na(x) & x != "")
  n_unique_orgs <- length(unique(na.omit(x)))
  
  # Problemfälle extrahieren
  not_found <- unique(x[!res & !is.na(x) & x != ""])
  
  # Ausgabe
  message("\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("                         HOCHSCHULEN-CHECK                                      ")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message(sprintf("• Gesamte Zeilen:             %d", total_rows))
  message(sprintf("• NA/Leere Zeilen:            %d", na_rows))
  message(sprintf("• Überprüfte Zeilen:          %d", checked_rows)) 
  
  if (length(not_found) > 0) {
    message("\nNicht in Hochschulliste gefunden:")
    message(paste(" -", not_found, collapse = "\n"))
  }
  
  if (n_unique_orgs > 1) {
    message("\nHinweis: Es wurden verschiedene unique Werte gefunden!")
    message("Gefundene einzigartige Werte:")
    message(paste(" -", unique(na.omit(x)), collapse = "\n"))
    message("Es darf nur einen gültigen Wert sowohl auf der Variable >>hochschule<< als auch auf der Variable >>hochschule_kurz<< geben.")
    message("NAs, leere Strings und falsch geschriebene Werte müssen korrigiert werden.")
    message("Bitte überprüfen Sie die Eingabe.")
  }
  
  message("\n Es liegt möglicherweise eine Rechtschreibfehler vor. Hier ist die Liste mit gültigen Werten zu finden:")
  message("http://srv-data01:30080/hex/hex/-/blob/main/hochschulen_namen_kuerzel.sql?ref_type=heads")
  
  invisible(res)
}
