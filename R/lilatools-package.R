#' lilatools: Ein R-Paket zum Einlesen von Dateien im LILA-Format
#'
#' \code{lilatools} stellt Funktionen bereit, mit denen Textdateien im LILA-Format eingelesen werden können. Das LILA Format stellt in standardisierter Form hydrologische Daten mit einem umfangreichen header für Metainformationen zusammen.
#' Es gibt vier verschiedene Varianten des LILA-Formats:
#' \describe{
#' \item{LILA EINZEL}{eine Datei, die nur die Daten einer Zeitreihe in einer Spalte enthält}
#' \item{LILA SPALTE}{eine Datei, deren Daten mehrere Zeitreihen mit gleicher Länge enthält, die nebeneinander in Spalten angeordnet sind}
#' \item{LILA BLOCK}{eine Datei, in der mehrere Zeitreihen inklusive der header untereinander angeordnet sind.}
#' \item{LILA hybrid}{eine Mischform aus LILA SPALTE und LILA BLOCK, also Daten neben- und untereinander}
#' }
#' Die Varianten können mit verschiedenen Dabei werden alle Metainformationen der LILA-Datei ausgelesen und in einer eigenen Klasse gespeichert.
#' Aus dem Gesamtdatensatz kann dann ein reduzierter Datensatz erstellt werden, der dann zB nur die einige wenige Metainformationen enthält, die für weitere Analysen benötigt werden.
#'
#' @section Funktionen des \code{lilatools}-Pakets:
#' \describe{
#' \item{\code{\link{read_lila}}}{Einlesen von Dateien im LILA-Format}
#' \item{\code{\link{liladata}}}{Die intern verwendete S3-Klasse um alle Informationen der LILA-Dateien effizient zu speichern}
#' }
#' Weitere Funktionen ermöglichen das Umwandeln und Auswählen der erforderlichen Daten aus einem Objekt der \code{liladata}-Klasse:
#' \describe{
#' \item{\code{\link{as.data.frame.liladata}}}{Umwandlung in data.frame unter Beibehaltung aller Metadaten}
#' \item{\code{\link{select.liladata}}}{Auswahl der gewünschten Metainformationen, die dann mit den Werten in einem data.frame zusammengeführt werden.}
#' \item{\code{\link{merge_liladata}}}{Zwei Objekte mit Klasse \code{liladata} werden zusammengeführt.}
#' }
#'
#'
#' @docType package
#' @name lilatools
#' @seealso \code{\link{read_lila}}
NULL


