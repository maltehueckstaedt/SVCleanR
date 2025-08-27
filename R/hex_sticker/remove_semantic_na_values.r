#' Entfernt semantisch leere Texte
#'
#' Diese Funktion bereinigt Textvektoren, indem sie Einträge mit zu wenigen Buchstaben
#' als fehlend (`NA`) kennzeichnet. Satzzeichen werden vor der Längenberechnung entfernt,
#' sodass nur Buchstaben gezählt werden.
#'
#' @param texts Ein Character-Vektor mit Texten, die geprüft werden sollen.
#' @param min_num_letters Eine Ganzzahl, die die minimale Anzahl an Buchstaben angibt,
#'   die ein Text enthalten muss, um nicht als `NA` markiert zu werden. Standard: 20.
#'
#' @return Gibt einen Character-Vektor zurück, in dem alle Einträge, die weniger als
#'   `min_num_letters` Buchstaben enthalten, als `NA` gesetzt wurden.
#'
#' @details
#' Die Funktion nutzt \code{stringr::str_replace_all}, um Satzzeichen zu entfernen.
#' Danach wird die verbleibende Zeichenanzahl gezählt, und Texte, die die angegebene
#' Mindestlänge nicht erreichen, werden durch `NA` ersetzt.
#'
#' @examples
#' db_data_<hochschulname>$kursbeschreibung <- 
#'   remove_semantic_na_values(db_data_<hochschulname>$kursbeschreibung)
#'
#' @importFrom stringr str_replace_all
#'
#' @export
remove_semantic_na_values <- function(texts, min_num_letters = 20) {
  letters_only <- stringr::str_replace_all(texts, "[[:punct:]]", "")
  length_letters_only <- lapply(letters_only, nchar)
  texts[length_letters_only < min_num_letters] <- NA
  return(texts)
}