# S3 class should suffice in this case
default_liladata_meta <- data.frame(station=NA_character_,
                                    datenart=NA_character_,
                                    dimension=NA_character_,
                                    zeitintervall=NA_character_)
default_liladata_data <- data.frame(id=NA_integer_, time=NA, values=NA_real_)

#' Erstellt ein Objekt der Klasse \code{liladata}
#'
#' Die Klasse ermöglicht das Verarbeiten Daten aus Dateien im LILA-Format. Insbesondere werden die gesamten Metainformationen in der Klasse beibehalten.
#'
#' Die Lese-Funktionen von LILA-Dateien erstellen ein Objekt der S3-Klasse \code{liladata}. Dieses enthält alle aus den LILA-Dateien ausgelesenen Daten. Für die Weiterverarbeitung kann das Objekt mit \code{\link[as.data.frame.liladata]{as.data.frame}} in ein data.frame umgewandelt werden. Dieser Weg ist aber nicht empfohlen, da LILA-Dateien üblicherweise umfangreiche Meta-Informationen haben, die für die gewünschte Analyse nicht erforderlich sind. Daher sollte zunächst eine Auswahl der erfordelrichen Daten mit \code{\link{select.liladata}} erfolgen.
#'
#' Intern trennt die Klasse die Daten der Zeitreihen von den Metainformationen. Dadurch soll der Speicherbedarf minimiert werden, was bei großen Datensätzen relevant werden kann. Daher enthält die Klasse zwei Listenelemente mit den Namen: data und meta. Diese können, wie von Listen gewohnt über '$data' oder '$meta' aufgerufen werden.
#' Die Daten im data-Listenelement sind als data.frame oder tibble abgelegt mit den Spalten:
#' \describe{
#' \item{time}{die Zeitschritte,}
#' \item{id}{eine eindeutige Kennzeichnung eines jeden Werteblocks und}
#' \item{value}{die jeweiligen Werte}
#'}
#' Die Metainformationen  im meta-Listenelement sind ebenfalls als data.frame oder tibble abgelegt. Dort ist ebenfalls die id enthalten, mit der die Metainformationen den Werteblöcken eindeutig zugeordnet werden kann.
#' Als weitere Informationen müssen mindestens die folgenden Spalten enthalten sein, wie dies für das LILA-Format definiert ist:
#' \itemize{
#' \item station
#' \item datenart
#' \item dimension
#' \item zeitintervall
#' }
#' Zusätzlich sind beliebig viele weitere Spalten möglich.
#'
#' Eine Veränderung der id ist nicht zu empfehlen, und sollte wenn, dann sowohl in \code{data} als auch in \code{meta} erfolgen. Normalerweise ist eine Manipulation auch nicht erforderlich. Jegliche Veränderung und Veränderung der Daten in R sollte erst nach einer Auswahl und Umwandlung in ein data.frame oder tibble erfolgen. Dies macht die Analyse ohnehin einfacher.
#'
#' @param data data.frame enthält die tatsächlichen Werte des Datensatzes
#' @param meta data.frame mit den Metainformationen
#'
#'
#'
#' @return Objekt der Klasse liladata
#' @export
#' @family liladata_methods
#' @examples
#' # empty object
#' liladata()
#'
#' # example object
#' liladata(data=data.frame(id="1-1",
#' time=c("09.09.2019 00:00", "09.09.2019 01:00", "09.09.2019 02:00", "09.09.2019 03:00", "09.09.2019 04:00", "09.09.2019 00:50"),
#' values=c(0.9, 0.9, 0.9, 0.9, 0.8, 0.8)),
#' meta=data.frame(id='1-1', station="Rebbelroth", gewaesser="Agger", stationsnummer=01320010, stationskennung='REBR',
#' datenart='Q',dimension='cbm/s', datenbezug='GTS', zeitintervall='01:00', datentyp='M',
#' datenursprung='vhs', flaeche=109.860, flusskilometer=63.693, vorhersagezeitpunkt='08.09.2019 13:00',
#' kommentar='Abfluss Vorhersage', austauschkennung='RP_01320010-qvhs-d01-201908270700'))
liladata <- function(data, meta){
  if(missing(data))
    data <- default_liladata_data
  if(missing(meta))
    meta <- cbind(data.frame(id=NA_integer_), default_liladata_meta)
  # check if minimum requirements are provided by data and meta
  if (!is.data.frame(data)) stop("data must be a data.frame") else
    if (!all(colnames(default_liladata_data)%in%colnames(data))) stop("required columns are missing in provided data")
  if (!is.data.frame(meta)) stop("meta must be a data.frame") else
    if (!all(colnames(default_liladata_meta)%in%colnames(meta))) stop("required columns are missing in provided meta information")
  # return object of class liladata
  structure(list(data=tbl_df(data), meta=tbl_df(meta)), class="liladata")
}

# calculate summary of class object
summary.liladata <- function(data){
  summary(data$meta)
  summary(data$data)
}

# print function to show main info
print.liladata <- function(data){
  cat("meta information:")
  print(data$meta)
  cat("data:")
  print(data$data)
}

# select a data.frame from liladata class
#' Datenauswahl erstellen
#'
#'Die gewüschten Metainformationen werden aus einem \code{liladata}-Objekt ausgewählt, mit den Werten zusammengeführt und in ein \code{data.frame} umgewandelt.
#'
#' Die Auswahl der gewünschten Spalten erfolgt bequem wie bei der Funktion \code{\link[dplyr]{select}}. Die Spaltennamen können direkt, durch Kommas getrennt, aber ohne Anführungsstriche aufgeführt werden. Alternativ kann vor die Spaltennamen auch ein Minus '-' gestellt werden. Dann werden die betroffenen Spalten nicht verwendet.
#'
#' Auch weitere Auswahlfunktionen können verwendet werden (siehe dazu die Dokumentation von \code{\link[dplyr]{select}})
#'
#' Eine Sonderstellung nimmt die id-Spalte ein. Diese wird immer ausgegeben, da sie für das Zusammenführen von Werten und Metainformationen verwendet wird. Innerhalb der Klasse liladata stellt sie sicher, dass Metadaten und Werte korrekt zugeordnet werden. Wenn sie nicht mehr benötigt wird, kann die id-Spalte also gelöscht werden. Im Fall, dass die Daten aber in das ursprüngliche \code{liladata}-Objekt zurückgeführt werden soll, sollte die id-Spalte erhalten bleiben.
#'
#' Bei der Auswahl sollte darauf geachtet werden, das ausreichend Metainformationen ausgegeben werden, sodass alle Werte eindeutig identifizert werden. Wenn also beispielsweise zwei Stationen denselben Namen tragen, sollte stattdessen eine eindeutig definierte Spalte verwendet werden, zB "Stationskennung".
#'
#' @param .data Objekt der Klasse \code{liladata}
#' @param ... Auswahl der gewünschten Spalten, getrennt durch Kommas
#'
#' @return data.frame mit den Spalten id, time, values und den gewählten Spalten mit Metainformationen.
#' @export
#' @family liladata_methods
#'@seealso \code{\link{liladata}} für Informationen zur \code{\link{liladata}}-S3-Klasse, \code{\link[dplyr]{select}} für weitere Informationen über Auswahl-Funktionalitäten
select.liladata <- function(.data, ...)
{
  # select meta info
  selection <- enquos(...)
  metasel <- dplyr::select(.data=.data$meta, id, !!! selection)
  datadf <- merge(.data$data, metasel, by="id")
  return(datadf)
}

# generate one data.frame from lila class
as.data.frame.liladata <- function(.data)
{
  datadf <- merge(.data$data, .data$meta, by="id")
}

# filter data
# filter.liladata <- function(.data, ...)
# {
#   # select meta info
#   filterquo <- enquos(...)
#   as_label(filterquo[[1]])
#   #TODO check if time or values are filtered
#   metasel <- filter(.data$meta, !!! filterquo)
#   datadf <- merge(.data$data, metasel, by="id")
#   return(datadf)
# }

# merge two liladata objects by appending rows
rbind.liladata <- function(.x, .y)
{
  # set source, if missing or identical x and y
  # set new id, depending on source
  print("not finished")
}
