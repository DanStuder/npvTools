#' Exakte Brunner-Munzel Permutationsverteilung
#'
#' Berechnet die exakte Permutationsverteilung der Brunner-Munzel-Teststatistik
#' `W` für zwei unabhängige Stichproben durch eine effiziente Gruppierung,
#' die gleiche Permutationen und Rangbindungen berücksichtigt.
#'
#' Die Funktion setzt voraus, dass die Eingabedaten genau zwei Gruppen als Faktor mit
#' den Ebenen 1 und 2 enthalten, wobei Gruppe 1 `n1` Beobachtungen hat.
#' Zurückgegeben wird ein tidy tibble mit der Teststatistik `W` und der Häufigkeit
#' `n_perm` der jeweiligen Werte über alle eindeutigen Permutationen.
#'
#' @param data Ein data.frame mit den Spalten `value` (numerischer Wert)
#'   und `group` (Faktor mit den Levels 1 und 2).
#'
#' @return Ein tibble mit den Spalten:
#'   \describe{
#'     \item{W}{Brunner-Munzel Teststatistik-Wert.}
#'     \item{n_perm}{Anzahl der Permutationen, die diesen `W`-Wert liefern.}
#'   }
#'
#' @details Die interne Funktion `calc_W()` berechnet Ränge, Rangmittelwerte `R1` und `R2`,
#'   gruppenspezifische Varianzen `S1_2` und `S2_2` sowie die gepoolte Varianz `sigma2`,
#'   um die Differenz der Rangmittel zu standardisieren. Die Permutationen werden effizient
#'   mit `combn()` generiert und mittels `dplyr::group_by()` auf sortierten Vektoren
#'   gruppiert, um Doppelberechnungen zu vermeiden. Dieser Ansatz eignet sich für kleine
#'   Stichproben (z.B. Gesamtn = 20), bei denen eine vollständige Enumerierung machbar ist.
#'
#' @examples
#' data <- data.frame(
#'   value = c(0, 0, 1, 1, 1, 1, 1, 1, 3, 3),
#'   group = factor(rep(1:2, each = 5))
#' )
#'
#' exact_bm_dist(data)
#'
#' @importFrom dplyr filter mutate group_by summarise n
#' @importFrom purrr map map2_dbl
#' @importFrom utils combn globalVariables
#' @importFrom tibble tibble
#'
#' @export
exact_bm_dist <- function(data) {
  n1 <- data |>
    dplyr::filter(group == 1) |>
    base::nrow()

  N <- base::nrow(data)

  # ALTERNATIVER PERMUTATIONSANSATZ
  ## Bei dieser version wird nach dem Erstellen der Permutationen
  ## gruppiert, da es oft viele identische Permutationen gibt.
  ## Diese können zusammengenommen und nur einmal berechnet werden,
  ## was ggf. computational power sparen kann.
  perms <- utils::combn(N, n1)

  full_perm <- tibble::tibble(
    index1 = purrr::map(base::seq_len(ncol(perms)), ~ perms[, .x]),
    index2 = purrr::map(index1, ~ base::setdiff(base::seq_len(N), .x))
  ) |>
    dplyr::mutate(
      group1 = purrr::map(index1, ~ data$value[.x]),
      group2 = purrr::map(index2, ~ data$value[.x])
    )

  # Gruppierung nach sortierten Gruppenwerten zur Reduktion der Berechnungen
  full_perm_grouped <- full_perm |>
    dplyr::mutate(
      group1_sorted = purrr::map(group1, sort),
      group2_sorted = purrr::map(group2, sort)
    ) |>
    dplyr::group_by(group1_sorted, group2_sorted) |>
    dplyr::summarise(
      n_perm = dplyr::n(),
      .groups = "drop"
    )

  # Berechne W nur einmal pro eindeutiger Gruppe
  full_perm_grouped <- full_perm_grouped |>
    dplyr::mutate(
      W = purrr::map2_dbl(group1_sorted, group2_sorted, npvTools::bm_w)
    ) |>
    dplyr::group_by(W) |>
    dplyr::summarise(n_perm = base::sum(n_perm))

  return(full_perm_grouped)
}
