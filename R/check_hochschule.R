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

  var_name <- deparse(substitute(x))|> 
    stringr::str_replace(".*\\$", "") 
  
  var_name_clean <- var_name |> 
    stringr::str_replace(".*\\$", "") |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_upper()
 
  
  # Ausgabe
  message("\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message(sprintf("                            %s                                ", var_name_clean))
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")
  message("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||")


  message("\n:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
  message("                                    ÜBERSICHT            ")
  message(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")

  message(sprintf("\n• Gesamte Zeilen:             %d", total_rows))
  message(sprintf("• NA/Leere Zeilen:            %d", na_rows))
  message(sprintf("• Überprüfte Zeilen:          %d", checked_rows))

  if (length(na_rows) > 0) {
    message("\n⛔ Achtung: Es wurden NA/Leere Strings gefunden!")
    message(sprintf("\n🔔 Hinweis: Auf der Variable >>%s<< sind NA und leere Strings nicht erlaubt.", var_name))
  }

  if (length(not_found) > 0) {
    message("\n:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    message("                                UNGÜLTIGE WERTE            ")
    message(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    message(paste("\n •", not_found, collapse = "\n"))
    message(sprintf("\n⛔ Achtung: Es wurden %d ungültige Werte gefunden", length(not_found)))
    message("\n🔔 Hinweis: Gegebenenfalls liegt ein Rechtschreibfehler vor. Für eine Übersicht der gültigen Werte, siehe:")
    message("http://srv-data01:30080/hex/hex/-/blob/main/hochschulen_namen_kuerzel.sql?ref_type=heads")

  }
  
  if (n_unique_orgs > 1) {
    message("\n:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    message("                                 UNIQE WERTE            ")
    message(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    message(paste(" •", unique(na.omit(x)), collapse = "\n"))
    message(sprintf("\n⛔ Achtung: Es wurden %d einzigartige Werte gefunden", n_unique_orgs))
    message(sprintf("\n🔔 Hinweis: Es darf nur einen uniquen Wert sowohl auf der Variable >>%s<< geben. Für eine Übersicht der gültigen Werte, siehe:", var_name))
    message("http://srv-data01:30080/hex/hex/-/blob/main/hochschulen_namen_kuerzel.sql?ref_type=heads")
  }
  invisible(res)
}
