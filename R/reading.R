#' Funktion zum Einlesen von Dateien im LILA format
#'
#' Die Funktion ermöglicht das Einlesen aller Varianten des LILA-Formats
#'
#'  LILA-Dateien in den LILA-Formaten SPALTE, EINZEL, BLOCK und hybrid können eingelesen werden. Für LILA-EINZEL ist eine vereinfachte und daher beschleunigte Routine implementiert, die über das Argument \code{lila_einzel=TRUE} aktiviert werden kann.
#'
#'  Wenn Dateien im Format LILA EINZEL eingelesen werden, kann die Option \code{filename_metainfo=TRUE} gesetzt werden, der Dateiname dem LEG-Format entspricht. DAnn werden die im Dateinamen enthaltenen Informationen den Metainformationen hinzugefügt.
#'
#' @param file path of file (char) to read
#' @param lila_einzel hat einzulesende Datei das Format LILA-EINZEL (optional)
#' @param ... andere Optionen, siehe Details
#'
#' @return Objekt mit Klasse \code{\link{liladata}}
#' @export
#' @family read_lila
#' @examples
#' read_lila(system.file("extdata", "exp_lilaeinzel.lila", package="lilatools"))
#' read_lila(system.file("extdata", "exp_lilaeinzel.lila", package="lilatools"), lila_einzel=T)
#' read_lila(system.file("extdata", "exp_lilablock.lila", package="lilatools"))
read_lila <- function(file, lila_einzel=FALSE, ...)
{
  if (lila_einzel)
    liladata <- read_lila_einzel(file, ...) else
    liladata <- read_lila_block(file, ...)
  #TODO loop over files and read files

  return(liladata)
}

get_lila_filemeta <- function(file, encoding="latin1")
{
  tworows <- scan(file=file, what=list(character(), character()), nlines = 2, sep=";", flush=T, encoding = encoding, quiet=T)
  if (tolower(tworows[[1]][1]) %in% c("sprache", "langue")) {
    filemeta <- data.frame(sprache=tworows[[2]][1])
    if (tolower(tworows[[1]][2]) %in% c("gesamtkommentar", "commentaire entiere"))
      filemeta <- cbind(filemeta, data.frame(gesamtkommentar=tworows[[2]][2]))
  } else
    filemeta <- NA
  return(filemeta)
}


#' Lesen von Dateien im LILA-EINZELDATEI-Format
#'
#'Die Funktion ist optimiert fürs Einlesen von Dateien im LILA-EINZELDATEI-Format
#'
#'Eine Datei im LARSIM format LILA EINZELDATEI wird eingelesen. Abweichende LILA-Formate können nicht gelesen werden.
#'Wenn der der Dateiname der 'Formatdefinition der Austauschkennung zum länderübergreifenden Austausch von Vorhersagen V 1.0' (LEG) entsprcht, werden bei angabe der der Option inspect_filename zusätzliche Informationen aus dem Dateinamen entnommen.
#'
#'Die Lokaleinstellungen mit \code{locale} umfassen zB die Einstellungen für das Dateiencoding. In der Regel muss daran nichts geändert werden. Allerdings empfiehlt es sich das locale extern als Variable zu speichern und vorzugeben, wenn viele Dateien eingelesen werden. Dann wird das Einlesen erheblich beschleunigt.
#' @param file Pfad der zu lesenden Datei
#' @param filename_metainfo logisch (default: FALSE), inspiziert Dateinamen mit Standardformat nach zusätzlichen Metainformationen (siehe Details)
#' @param locale Lokaleinstellungen, können extern vorgegeben werden, default: locale(decimal_mark = ".", date_names = "de", encoding="latin1")
#' @param n_max maximale Anzahl an Zeilen, die aus dem Datenblock (also unterhalb der Metadaten) eingelesen werden
#' @return Objekt der Klasse \code{\link{liladata}}
#' @export
#' @family read_lila
#'
#'
#' @examples
#' read_lila_einzel(system.file("extdata", "exp_lilaeinzel.lila", package="lilatools"))

read_lila_einzel <- function(file, filename_metainfo=FALSE, locale="default", n_max=Inf)
{
  #set default locale
  if(first(locale=="default"))
    locale <- locale(decimal_mark = ".", date_names = "de", encoding="latin1")
  ####################
  #read file name meta
  ####################
  if (filename_metainfo){
    filename_meta <- check_legformat(basename(file))
    if(!filename_meta$check) warning(paste("Dateiname entspricht nicht LEG-Format (filename_meta wird irgnoriert):", file))
  }
  ####################
  #read file meta info
  ####################
  filemeta <- get_lila_filemeta(file)
  if (!identical(filemeta, NA)) skipmeta <- ncol(filemeta) else
    skipmeta <- 0
  ####################
  # read meta block
  ####################
  # first determine length of header block
  header_length = min(grep(pattern="^[0-9]{2}.*", scan(file, what=character(), flush=T, sep=";", skip = skipmeta, encoding = locale$encoding, quiet = T)))-1
  # read meta info from header block
  meta <- scan(file=file, what=list(character(), character()), sep=";", skip = skipmeta, na.strings = "-", nlines=header_length, encoding=locale$encoding, quiet = T)
  metadf <- as.list(meta[[2]])
  names(metadf) <- meta[[1]]
  metadf <- as_tibble(metadf)
  # add sourcefile name to meta
  metadf$source <- file
  # add id
  metadf$id <- "1-1"
  # add filename meta
  if(filename_metainfo)
    if(filename_meta$check){
      # compare datenart and datenursprung with meta
      duplicate_cols <- colnames(filename_meta$leg_infos)[colnames(filename_meta$leg_infos) %in% colnames(metadf)]
      identical_cols <- map_dfc(metadf[,c("datenart", "datenursprung")], tolower) == map_dfc(filename_meta$leg_infos[,c("datenart", "datenursprung")], tolower)
      if (!all(identical_cols))
        warning(paste("Daten", paste(collapse = ", ", colnames(identical_cols)[!identical_cols]),
                      "in Datei", file, "entsprechen nicht Angabe in Dateinamen (LEG-Format)"))
      # add filename_meta info to meta info
      metadf <- cbind(metadf, filename_meta$leg_infos[, !colnames(filename_meta$leg_infos) %in% colnames(metadf)])
    }
  # add file meta
  if(!identical(filemeta, NA))
    metadf <- cbind(metadf, ncol(filemeta))
  ####################
  #read data
  ####################
  # values <- read.csv(file=file, colClasses = c("character", "numeric", "NULL"), sep=";", skip = skipmeta + header_length, header=F, na.strings="-",
  #                             nrows=n_max)
  # colnames(values) <- c("time", "values")
  # values$time <- as.POSIXct(strptime(format="%d.%m.%Y %H:%M", x=values$time)) # is this faster?

  # original code with read_delim:
  # values <- readr::read_delim(file=file, col_types = cols_only(col_datetime(format="%d.%m.%Y %H:%M"), col_double(), col_skip()), delim=";", skip = skipmeta + header_length, na="-", col_names=c("time", "values"),
  #                             locale=locale(decimal_mark = ".", date_names = "de", encoding=encoding), n_max=n_max)
  # changed with external locale
  values <- readr::read_delim(file=file, col_types = cols_only(col_datetime(format="%d.%m.%Y %H:%M"), col_double(), col_skip()), delim=";", skip = skipmeta + header_length, na="-", col_names=c("time", "values"),
                              locale=locale, n_max=n_max)

  values$id <- "1-1"
  ####################
  #merge meta and values to liladata class
  ####################
  data <- liladata(meta=metadf, data=values)
  return(data)
}

get_blocks <- function(data)
{
  # get block starts by backwards comparison
  starts <- c(1, 1 + which(data[-1] != data[-length(data)]))
  # get block ends
  ends <- c(starts[-1] -1 , length(data))
  # merge to table
  blocks <- data.frame(no = 1:length(starts),
                       val = data[starts],
                       start =starts,
                       end = ends)
  return(blocks)
}

get_block_col_types <- function(file, skip, encoding="latin1")
{
  # get number of columns
  first_line <- scan(file = file, what = character(), encoding=encoding, nlines = 1, sep=";", skip = skip, na.strings = "-", quiet =T)
  # get number of columns within first and last, which require special treatment
  n_datacol <- length(first_line)-2
  # check if first columns is time
  if(grepl(pattern = "[0-9]{2}.[0-9]{2}.[0-9]{4} [0-9]{2}:[0-9]{2}", first_line[1]))
    first_col <- readr::cols(col_datetime(format="%d.%m.%Y %H:%M")) else
      first_col <- readr::cols(col_character())
  # check type of data columns
  if(all(grepl(pattern = "^[0-9.]+$", first_line[2:(1+n_datacol)])))
    data_cols <- rep(readr::cols(col_double())$cols, n_datacol) else
      data_cols <- rep(readr::cols(col_character())$cols, n_datacol)
  # check if last column is empty
  if (last(first_line)=="")
    last_col <- readr::cols("-")$cols else
      last_col <- data_cols[1]
  # define expected column types
  col_types <- first_col
  col_types$cols <- c(col_types$cols, data_cols, last_col)
}

#' Einlesen von LILA-BLOCK-Format
#'
#' Die Funktion ist eine flexible Routine, um eine Datei im LILA-BLOCK-Format einzulesen.
#'
#' \code{read_lila_block} ist die flexibelste der \code{read_lila}-Funktionen: Sie kann neben dem Format LILA-BLOCK auch LILA EINZEL, LILA SPALTE und LILA-hybrid einlesen. Dabei bestehen aber Geschwindigkeitsnachteile.
#'
#'\code{locale} hat den folgenden default-Wert:
#'
#'locale(decimal_mark = ".", date_names = "de", encoding="latin1", time_format="\%d.\%m.\%Y \%H:\%M")
#'
#'Er kann verändert werden, wozu indem der angegebene Aufruf von locale kopiert wird und nach bedarf verändert wird. Dabei sollten die mindestens die verwendeten Parameter gesetzt werden.
#'Es empfiehlt sich, \code{locale} mit den Standardparametern einmal für eine Session zu definieren, besonders wenn viele Dateien eingelesen werden sollen. Locale hat leider einen relativ großen Rechenbedarf beim Aufruf.
#'Wenn es nur einmal aufgerufen wird hat das also große Geschwindigkeitsvorteile (siehe Beispiel).
#' @param file Pfad zur einzuleseneden Datei
#' @param filename_metainfo (logisch, default: FALSE) enthält der Dateipfad Metadaten, die importiert werden sollen?
#' @param locale setzt einige wichtige Informationen für den Lesevorgang, default siehe Details
#' @param n_max maximale Anzahl an Zeilen, die aus dem Datenblock (also unterhalb der Metadaten) eingelesen werden
#'
#' @return Objekt in liladata-Klasse
#' @family read_lila
#' @export
#' @examples
#' # read_lila_block(system.file("extdata", "exp_lilablock.lila", package="lilatools"))
#' # locale extern aufrufen
#' loc <- locale(decimal_mark = ".", date_names = "de", encoding="latin1")
#' read_lila_block(system.file("extdata", "exp_lilablock.lila", package="lilatools"), locale=loc)
read_lila_block <- function(file, filename_metainfo=F, locale="default", n_max=Inf)
{
  # set locale
  if (first(locale=="default"))
    locale <- locale(decimal_mark = ".", date_names = "de", encoding="latin1", time_format="%d.%m.%Y %H:%M")
  ####################
  #read file meta info
  ####################
  # check file LEG format
  if (filename_metainfo){
    filename_meta <- check_legformat(basename(file))
    if(!filename_meta$check) warning(paste("Dateiname entspricht nicht LEG-Format (filename_meta wird irgnoriert):", file))
  }
  # check for header in
  filemeta <- get_lila_filemeta(file)
  if (!identical(filemeta, NA)) skipmeta <- ncol(filemeta) else
    skipmeta <- 0
  # first determine the blocks: start and end lines of header (FALSE) and value blocks (TRUE)
  fileblocks = get_blocks(grepl(pattern="^[0-9]{2}.*", scan(file, what=character(), flush=T, sep=";", skip = skipmeta, quiet = T)))
  # check if number of header and value blocks are equal
  if (sum(fileblocks$val) != sum(!fileblocks$val)) stop(paste("file", file, " has unequal number of header blocks (", sum(!fileblocks$val), ") and value blocks (", sum(fileblocks$val), ")"))
  # determine id
  fileblocks$id <- paste("1", rep(1:(nrow(fileblocks)/2), each=2), sep="-")
  # read meta info from header blocks
  allmeta <- plyr::adply(.data=dplyr::filter(.data=fileblocks, !val), .margins=1, .fun=function(thisblock)
  {
    col_types <- get_block_col_types(file=file, skip = skipmeta + thisblock$start -1, encoding=locale$encoding)
    # read one meta info block
    meta <- readr::read_delim(file=file, delim=";", skip = skipmeta + thisblock$start -1, n_max=thisblock$end - thisblock$start +1, na="-",
                              locale=locale,
                              col_names=F, col_types = col_types, progress=F)
    meta <- t(meta)
    meta <- meta[!apply(is.na(meta), MARGIN=1, all),] # remove empty columns
    # create data frame
    colnames(meta) <- meta[1,]
    rownames(meta) <- 1:nrow(meta)
    meta <- data.frame(meta, stringsAsFactors=F)
    meta <- meta[-1,]
    rownames(meta) <- 1:nrow(meta)
    # remove unused factor levels
    meta <- droplevels(meta)
    # add block_id (required in hybrid format)
    meta$block_id <- 1:nrow(meta)
    return(meta)
  })
  allmeta$id <- paste(allmeta$id, allmeta$block_id, sep="-")
  # remove block info
  allmeta <- dplyr::select(.data=allmeta, -no, -val, -start, -end, -block_id)
  # add file meta info
  if (!identical(filemeta, NA))
    allmeta <- cbind(allmeta, filemeta)
  # add filename meta
  if(filename_metainfo)
    if (filename_meta$check){
      # compare datenart and datenursprung with meta
      duplicate_cols <- colnames(filename_meta$leg_infos)[colnames(filename_meta$leg_infos) %in% colnames(allmeta)]
      identical_cols <- purrr::map_dfc(allmeta[,c("datenart", "datenursprung")], tolower) == purrr::map_dfc(filename_meta$leg_infos[,c("datenart", "datenursprung")], tolower)
      if (!all(identical_cols))
        warning(paste("Daten", paste(collapse = ", ", colnames(identical_cols)[!identical_cols]),
                      "in Datei", file, "entsprechen nicht Angabe in Dateinamen (LEG-Format)"))
      # add filename_meta info to meta info
      allmeta <- cbind(allmeta, filename_meta$leg_infos[, !colnames(filename_meta$leg_infos) %in% colnames(allmeta)])
    }

  # add sourcefile name to meta
  allmeta$source <- file
  # lower case column names
  colnames(allmeta) <- tolower(colnames(allmeta))
  #TODO fix issues occurring due to factors by working with tibble
  ####################
  #read file values
  ####################
  # read data blocks
  alldata <- plyr::adply(.data=dplyr::filter(.data=fileblocks, val), .margins=1, .fun=function(thisblock)
  {
    col_types <- get_block_col_types(file=file, skip = skipmeta + thisblock$start -1, encoding=locale$encoding)
    # read one data block
    data <- readr::read_delim(file=file, delim=";", skip = skipmeta + thisblock$start -1,
                              n_max=min(c(n_max=Inf, thisblock$end - thisblock$start +1)),
                              na="-", locale=locale,
                              col_types = col_types, progress = F,
                              col_names = F)
    # set column names
    colnames(data) <- c("time", paste("c", 1:(ncol(data)-1), sep="_"))
    # gather all columns into one value column
    data <- tidyr::gather(data=data, key="block_id", value = "values", -1)
    colnames(data) <- c("time", "block_id", "values")
    return(data)
  })
  alldata$id <- paste(alldata$id, gsub(".*_(.*)", "\\1", alldata$block_id), sep="-")
  # remove block info
  alldata <- dplyr::select(.data=alldata, -no, -val, -start, -end, -block_id)
  #TODO simplify id: only file-no, not file-block-no
  ####################
  #merge meta and values to liladata class
  ####################
  data <- liladata(meta=allmeta, data=alldata)
  return(data)
}


#' Dateinamen im LEG-Format prüfen und zerlegen
#'
#' Die Konformität des Dateinamens mit den Vorgaben zum LEG-Format werden geprüft. Die einzelnen Informationen im Dateinamen werden extrahiert

#' @param filename zu prüfender Dateiname (character)
#'
#' @return \code{list} mit den Elementen \code{check} (logische Angabe ob file dem Format entspricht) und \code{leg_format} (Informationen im Dateinamen als \code{data.frame})
#' @export
#'
#' @examples
#' check_legformat("BY_24215903-tzugtsvhs-e13-201805150000-LARSIM_WHM-201805100500-VAR1.lila")
#' check_legformat("BY_24215903-qvhs.lila")
check_legformat <- function(filename)
{
  # add .lila, if missing
  if (!grepl(pattern = ".lila$", filename))
    filename <- paste(filename, ".lila", sep="")
  # patterns to check
  patterns <- list(pegelkennung = "^([[:alpha:]]{2,3}_[[:digit:]]+)",
                   datenart = "-(q|w|twas|tzugts|Q|W|TWAS|TZUGTS)",
                   datenursprung = "([mesivhabpMESIVHABP+]+)",
                   vhs_met = "-([[:lower:]]{1}[[:digit:]]{2}|[[:lower:]]{3})",
                   vzp_met = "-([0-9]{12})",
                   vhs_hyd = "-([a-zA-Z_^]+)",
                   vzp_hyd = "-([0-9]{12})",
                   var = "-([a-zA-Z_0-9^ ]+)",
                   lila = "[.]{1}lila$")

  # general check
  if(!grepl(pattern = paste(sep="", patterns$pegelkenn, patterns$datenart, patterns$datenursprung, ".*", patterns$lila), filename)){
    check <- F
    leg_infos <- NA
  } else {
    check <- T
    # get required info
    leg_infos <- list()
    for (i in 1:3){
      thisinfo <- gsub(paste(paste(collapse="", patterns[1:3]), ".*", sep=""), paste(sep="", "\\", i), filename)
      names(thisinfo) <- names(patterns)[i]
      leg_infos <- append(leg_infos, thisinfo)
    }
    # get flex part (after datenursprung)
    flex <- gsub(paste(sep="", patterns$pegelkenn, patterns$datenart, patterns$datenursprung, "(.*)", patterns$lila), "\\4", filename)
    if (nchar(flex)>0){
      # split flex part
      flexparts <- strsplit(flex, "-")[[1]][-1]
      # catch special format error
      if (grepl("-$", flex)) flexparts <- append(flexparts, "")
      # iterate patterns
      flex_ok <- T
      i <- 1
      while(flex_ok & i<=length(flexparts)){
        if (grepl(paste(sep="", "^", gsub("^-","", patterns[3+i]), "$"), flexparts[i])){
          thisinfo <- list(flexparts[i])
          names(thisinfo) <- names(patterns)[3+i]
          leg_infos <- append(leg_infos, thisinfo)
          i <- i+1
        } else
        {
          check <- F
          flex_ok <- F
        }
      }
    }
    # set default for var, im missing
    if (!"var" %in% colnames(leg_infos))
      leg_infos$var <- ""
    leg_infos <- tibble::as_tibble(leg_infos)
  }
  # prepare result
  out <- list(check=check,
              leg_infos=leg_infos)
  return(out)
}
