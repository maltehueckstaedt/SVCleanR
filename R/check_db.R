#' Check für DB
#'
#' Diese Funktion prüft mit \pkg{pointblank}, ob die DB
#' die erwarteten Spalten enthält und ob die Werte für Hochschulnamen und -kürzel
#' mit den in den SQL-Paketdaten hinterlegten Referenzlisten übereinstimmen.
#'
#' @param test_data Ein \code{data.frame} mit den zu prüfenden Daten.
#' @param werte Optional: Vektor erlaubter Hochschulnamen. 
#'   Falls \code{NULL}, wird automatisch die Liste der langen Hochschulnamen
#'   aus der mitgelieferten SQL-Datei verwendet.
#'
#' @details
#' Die Funktion lädt die Referenzlisten für Hochschulnamen und -kürzel aus der
#' Datei \code{extdata/hochschulen_namen_kuerzel.sql} des Pakets \pkg{SVCleanR}.
#' 
#' Folgende Checks werden durchgeführt:
#' \enumerate{
#'   \item Alle erwarteten Spaltennamen sind im Datensatz vorhanden.
#'   \item Werte in der Spalte \code{hochschule} liegen in der erlaubten Wertemenge.
#'   \item Werte in der Spalte \code{hochschule_kurz} liegen in der erlaubten Wertemenge.
#' }
#'
#' @return Eine unsichtbare Liste mit den Elementen:
#' \itemize{
#'   \item \code{agent}: Der \pkg{pointblank} Agent mit den Prüfergebnissen.
#'   \item \code{hochschule_lang}: Vektor mit erlaubten langen Hochschulnamen.
#'   \item \code{hochschule_kurz}: Vektor mit erlaubten Hochschulkürzeln.
#'   \item \code{werte}: Der tatsächlich verwendete Vergleichs-Vektor für \code{hochschule}.
#' }
#'
#' Zusätzlich wird ein HTML-Report im Viewer angezeigt.
#'
#' @examples
#' \dontrun{
#'   res <- check_db(test_data)
#'   res$agent |> pointblank::get_agent_report()
#' }
#'
#' @import pointblank
#' @import stringr
#' @importFrom dplyr all_of
#' @importFrom stats na.omit
#' @export
check_db <- function(test_data, werte = NULL) {
  requireNamespace("stringr")
  requireNamespace("pointblank")

  # SQL aus Paketinterna laden
  sql_file <- system.file("extdata", "hochschulen_namen_kuerzel.sql",
                          package = "SVCleanR")
  sql_text <- readLines(sql_file, warn = FALSE)

  hochschule_lang <- stringr::str_match(sql_text, "\\([0-9]+, '([^']+)'")[,2]
  hochschule_kurz <- stringr::str_match(sql_text, "\\([0-9]+, '[^']+', '([^']+)'")[,2]

  hochschule_lang <- hochschule_lang[!is.na(hochschule_lang)]
  hochschule_kurz <- hochschule_kurz[!is.na(hochschule_kurz)]

  # Fallback: wenn kein Set übergeben → lange Namen
  if (is.null(werte)) werte <- hochschule_lang

  expected_vars <- c(
    "id","anmerkungen","dozierende","ects","fakultaet","hochschule",
    "hochschule_kurz","jahr","kursbeschreibung","kursformat_original",
    "kursformat_recoded","lehrtyp","lernmethode","lernziele","literatur",
    "module","nummer","organisation_orig","organisation","pfad",
    "pruefung","scrape_datum","semester","sprache_original","sprache_recoded",
    "studiengaenge","sws","teilnehmerzahl","titel","url",
    "voraussetzungen","zusatzinformationen","institut","data_analytics_ki",
    "softwareentwicklung","nutzerzentriertes_design","it_architektur",
    "hardware_robotikentwicklung","quantencomputing","lehr_und_forschungsbereich",
    "studienbereich","faechergruppe","luf_code","stub_code","fg_code",
    "matchingart"
  )

  agent <- pointblank::create_agent(tbl = ~ test_data) |>
    pointblank::col_exists(columns = dplyr::all_of(expected_vars),
                           actions = pointblank::action_levels(stop_at = 0.001)) |>
    pointblank::col_vals_in_set(columns = pointblank::vars(hochschule),
                                set = werte,
                                actions = pointblank::action_levels(stop_at = 0.001)) |>
    pointblank::col_vals_in_set(columns = pointblank::vars(hochschule_kurz),
                                set = hochschule_kurz,
                                actions = pointblank::action_levels(stop_at = 0.001)) |>
    pointblank::interrogate()

  print(pointblank::get_agent_report(agent, title = "DB-Check"))
  invisible(list(
    agent = agent,
    hochschule_lang = hochschule_lang,
    hochschule_kurz = hochschule_kurz,
    werte = werte
  ))
}
