#' Prüft Organisations-Variable auf definierte Qualitätsregeln
#'
#' Diese Funktion verwendet das \pkg{pointblank}-Paket, um die Werte in der
#' Organisationsspalte systematisch zu validieren. Es werden verschiedene
#' Checks durchgeführt (Semikolon, Sonderzeichen, Länge, Leerzeichen,
#' Abschlussbegriffe). Optional wird ein HTML-Report mit Badges ausgegeben.
#'
#' @param data Ein \code{data.frame}, das die zu prüfende Spalte enthält.
#' @param organisation_col Name der Organisationsspalte (Standard: "organisation").
#' @param stop_at Schwellenwert für Fehler-Toleranz. Kann relativ (Anteil 0–1)
#'   oder absolut (z. B. 1 für „Null Toleranz“) angegeben werden.
#'   Standard: 0.001 (= 0.1 % fehlerhafte Zeilen).
#' @param show_report Logisch; wenn \code{TRUE}, wird ein pointblank-Report
#'   mit Badges im Viewer ausgegeben. Standard: \code{TRUE}.
#'
#' @details
#' Die folgenden Prüfungen werden durchgeführt:
#' \enumerate{
#'   \item Nur korrektes Semikolon \code{" ; "} oder kein Semikolon
#'   \item Kein \code{"|"} enthalten
#'   \item Kein \code{">"} enthalten
#'   \item Länge der Zeichenkette ≤ 1000
#'   \item Keine überflüssigen Leerzeichen (wie \code{str_squish})
#'   \item Warnung bei Abschluss-Begriffen (z. B. Bachelor, Master, Diplom)
#' }
#'
#' @return Ein \code{pointblank} Agent-Objekt invisibly. Über
#'   \code{pointblank::get_agent_report()} kann der Report auch separat erzeugt werden.
#'
#' @examples
#' \dontrun{
#'   agent <- pb_check_organisation(test_data, organisation_col = "organisation")
#'   pointblank::get_agent_report(agent)
#' }
#'
#' @export
check_organisation <- function(data, organisation_col = "organisation",
                                  stop_at = 0.001, show_report = TRUE) {
  stopifnot(is.data.frame(data))
  col <- rlang::ensym(organisation_col)

  # Standard-Action-Levels
  act_fail <- pointblank::action_levels(stop_at = stop_at)
  act_warn <- pointblank::action_levels(warn_at = stop_at)

  agent <- pointblank::create_agent(
    tbl = data,
    tbl_name = "Organisation",
    actions = pointblank::action_levels(
      warn_at = stop_at,
      stop_at = stop_at
    )
  )

    # 1) Nur korrektes Semikolon " ; " oder kein Semikolon
    pointblank::col_vals_regex(
      columns = !!col,
      regex   = "^(?:[^;]*(?:\\s;\\s))*[^;]*$",
      actions = act_fail,
      step_id = "separator_check",
      label   = "separator_check"
    ) |>

    # 2) Kein "|" enthalten
    pointblank::col_vals_regex(
      columns = !!col,
      regex   = "^[^|]*$",
      actions = act_fail,
      step_id = "or_check",
      label   = "or_check"
    ) |>

    # 3) Kein ">" enthalten
    pointblank::col_vals_regex(
      columns = !!col,
      regex   = "^[^>]*$",
      actions = act_fail,
      step_id = "greater_check",
      label   = "greater_check"
    ) |>

    # 4) Länge <= 1000
    pointblank::col_vals_between(
      columns = .nchar_org,
      left = 0, right = 1000,
      preconditions = function(x) dplyr::mutate(x, .nchar_org = nchar(!!col)),
      actions = act_warn,
      step_id = "length_check",
      label   = "length_check"
    ) |>

    # 5) Keine überflüssigen Leerzeichen
    pointblank::col_vals_expr(
      expr = ~ organisation == .squished,
      preconditions = function(x) {
        dplyr::mutate(x, .squished = stringr::str_squish(x[[rlang::as_string(col)]]))
      },
      actions = act_fail,
      step_id = "squished_check",
      label   = "squished_check"
    ) |>

    # 6) Warnung bei Master/Bachelor-Begriffen
    pointblank::col_vals_regex(
      columns = !!col,
      regex   = "^(?!.*(Bachelor|Master|Diplom|Staatsexamen|Staatsexamens|SS|WS|B\\.Sc\\.|M\\.Sc\\.|B\\.A\\.|M\\.A\\.)).*$",
      actions = act_warn,
      step_id = "non_orga_check",
      label   = "non_orga_check"
    ) |>
    pointblank::interrogate()

  if (isTRUE(show_report)) {
    print(pointblank::get_agent_report(agent, title = "Organisation-Check"))
  }
  invisible(agent)
}
