#' Brunner-Munzel Teststatistik W
#'
#' Berechnet die standardisierte Brunner-Munzel-Teststatistik `W` für zwei Vektoren
#' unabhängiger Beobachtungen.
#'
#' Die Funktion implementiert die studentisierte Teststatistik für den Brunner-Munzel-Test,
#' die die stochastische Überlegenheit zwischen zwei Gruppen misst. Sie berechnet Ränge,
#' Rangmittelwerte `R1` und `R2`, gruppenspezifische Varianzschätzer `S1_2` und `S2_2`
#' sowie die gepoolte Varianz `sigma2` zur Standardisierung der Rangdifferenz.
#' Die Formel lautet: `W = (R2 - R1) / sqrt(sigma2 * (n1 + n2))`.
#'
#' Diese Statistik ist robust gegenüber Varianzheterogenität und konvergiert asymptotisch
#' zu einer Standardnormalverteilung unter der Nullhypothese stochastischer Vergleichbarkeit.
#'
#' @param g1 Numerischer Vektor der Beobachtungen von Gruppe 1.
#' @param g2 Numerischer Vektor der Beobachtungen von Gruppe 2.
#'
#' @return Skalarwert der Brunner-Munzel-Teststatistik `W`.
#'
#' @details 
#' Die Berechnung erfolgt in mehreren Schritten:
#' 
#' 1. **Rangbildung**: Gesamtränge (`rank`) und Gruppenränge (`group_rank`) werden berechnet.
#' 2. **Rangmittel**: `R1` und `R2` als Mittelwerte der Gesamtränge pro Gruppe.
#' 3. **Varianzschätzer**: 
#'    \deqn{S_{i}^2 = \frac{1}{n_i - 1} \sum (rank - group\_rank - rankmean + \frac{n_i + 1}{2})^2}
#' 4. **Gepoolte Varianz**:
#'    \deqn{\sigma^2 = (n_1 + n_2) \left( \frac{\sigma_{1}^2}{n_1} + \frac{\sigma_{2}^2}{n_2} \right)}
#' 5. **Teststatistik**: `W = (R2 - R1) / sqrt(sigma2 * (n1 + n2))`.
#'
#' Bei Bindungen werden Mittelränge verwendet. Die Funktion erfordert `n1 > 1` und `n2 > 1`
#' für definierte Varianzen.
#'
#' @examples
#' # Beispiel mit den Beispieldaten
#' g1 <- c(0, 0, 1, 1, 1)  # Gruppe 1
#' g2 <- c(1, 1, 1, 3, 3)  # Gruppe 2
#' 
#' bm_w(g1, g2)
#'
#' @seealso \code{\link[=exact_bm_dist]{exact_bm_dist()}} für die exakte Permutationsverteilung.
#'
#' @importFrom dplyr mutate group_by ungroup filter summarise pull
#' @importFrom tibble tibble
#' @importFrom utils globalVariables
#'
#' @export
bm_w <- function(g1, g2) {
  n1 <- base::length(g1)
  n2 <- base::length(g2)
  
  combined <- tibble::tibble(
    value = c(g1, g2),
    group = c(rep(1, n1), rep(2, n2))
  )
  
  ranks <- combined |>
    dplyr::mutate(rank = base::rank(value)) |>
    dplyr::group_by(group) |>
    dplyr::mutate(group_rank = base::rank(value)) |>
    dplyr::ungroup()
  
  rank_means <- ranks |>
    dplyr::group_by(group) |>
    dplyr::mutate(rankmean = base::mean(rank)) |>
    dplyr::ungroup()
  
  R1 <- rank_means$rankmean[1]
  R2 <- rank_means$rankmean[n1 + 1]
  
  Si <- rank_means |>
    dplyr::mutate(
      ni = base::ifelse(group == 1, n1, n2),
      Si = (rank - group_rank - rankmean + ((ni + 1) / 2))^2
    )
  
  S1_2 <- Si |>
    dplyr::filter(group == 1) |>
    dplyr::summarise(S1_2 = base::sum(Si) / (n1 - 1)) |>
    dplyr::pull(S1_2)
  S2_2 <- Si |>
    dplyr::filter(group == 2) |>
    dplyr::summarise(S2_2 = base::sum(Si) / (n2 - 1)) |>
    dplyr::pull(S2_2)
  
  sigma1_2 <- S1_2 / n1^2
  sigma2_2 <- S2_2 / n2^2
  
  sigma2 <- (n1 + n2) * (sigma1_2 / n1 + sigma2_2 / n2)
  
  W <- (R2 - R1) / (base::sqrt(sigma2) * base::sqrt(n1 + n2))
  return(W)
}