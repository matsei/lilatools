# check requirements (only necessary if standalone script is distributed)
#' Funktion zum Einlesen von Dateien im LILA format
#'
#' Die Funktion ermöglicht das Einlesen aller Varianten des LILA-Formats
#'
#'  LILA-Dateien in den LILA-Formaten SPALTE, EINZEL, BLOCK und hybrid können eingelesen werden. Für LILA-EINZEL ist eine vereinfachte und daher beschleunigte Routine implementiert, die über das Argument \code{lila_einzel=TRUE} aktiviert werden kann.
#'
#' @param file path of file (char) to read
#' @param lila_einzel hat einzulesende Datei das Format LILA-EINZEL (optional)
#' @param ... andere Optionen, siehe Details
#'
#' @return Objekt mit Klasse \code{\link{liladata}}
#' @export
#' @family read_lila
#' @examples
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
#' @param file Pfad der zu lesenden Datei
#' @param inspect_filename logisch (default: FALSE), inspiziert Dateinamen mit Standardformat nach zusätzlichen Metainformationen (siehe Details)
#'
#'Eine Datei im LARSIM format LILA EINZELDATEI wird eingelesen. Abweichende LILA-Formate können nicht gelesen werden.
#'Wenn der der Dateiname der 'Formatdefinition der Austauschkennung zum länderübergreifenden Austausch von Vorhersagen V 1.0' (LEG) entsprcht, werden bei angabe der der Option inspect_filename zusätzliche Informationen aus dem Dateinamen entnommen.
#' @return Objekt der Klasse \code{\link{liladata}}
#' @export
#' @family read_lila
#'
#'
#' @examples
read_lila_einzel <- function(file, filename_metainfo=NA, encoding="latin1")
{
  ####################
  #read file meta info
  ####################
  filemeta <- get_lila_filemeta(file)
  if (!identical(filemeta, NA)) skipmeta <- ncol(filemeta) else
    skipmeta <- 0
  # first determine length of header block
  header_length = min(grep(pattern="^[0-9]{2}.*", scan(file, what=character(), flush=T, sep=";", skip = skipmeta, encoding = encoding, quiet = T)))-1
  # read meta info from header block
  meta <- scan(file=file, what=list(character(), character()), sep=";", skip = skipmeta, na.strings = "-", nlines=header_length, encoding=encoding, quiet = T)
  metadf <- data.frame(as.list(meta[[2]]))
  colnames(metadf) <- meta[[1]]
  # add sourcefile name to meta
  metadf$source <- file
  # add id
  metadf$id <- "1-1"
  ####################
  #read file meta info
  ####################
  values <- readr::read_delim(file=file, col_types = cols_only(col_datetime(format="%d.%m.%Y %H:%M"), col_double(), col_skip()), delim=";", skip = skipmeta + header_length, na="-", col_names=c("time", "values"),
                              locale=locale(decimal_mark = ".", date_names = "de", encoding=encoding))
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
    first_col <- cols(col_datetime(format="%d.%m.%Y %H:%M")) else
      first_col <- cols(col_character())
  # check type of data columns
  if(all(grepl(pattern = "^[[:digit:]]$", first_line[2:(1+n_datacol)])))
    data_cols <- rep(cols(col_double())$cols, n_datacol) else
      data_cols <- rep(cols(col_character())$cols, n_datacol)
  # check if last column is empty
  if (last(first_line)=="")
    last_col <- cols("-")$cols else
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
#' @param file Pfad zur einzuleseneden Datei
#' @param filename_metainfo (logisch, default: FALSE) enthält der Dateipfad Metadaten, die importiert werden sollen?
#'
#' @return Objekt in liladata-Klasse
#' @family read_lila
#' @export
#'
#' @examples
read_lila_block <- function(file, filename_metainfo=F, encoding="latin1")
{
  ####################
  #read file meta info
  ####################
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
    col_types <- get_block_col_types(file=file, skip = skipmeta + thisblock$start -1, encoding=encoding)
    # read one meta info block
    meta <- readr::read_delim(file=file, delim=";", skip = skipmeta + thisblock$start -1, n_max=thisblock$end - thisblock$start +1, na="-",
                              locale=locale(decimal_mark = ".", date_names = "de", encoding=encoding),
                              col_names=F, col_types = col_types)
    meta <- t(meta)
    meta <- meta[!apply(is.na(meta), MARGIN=1, all),] # remove empty columns
    # create data frame
    colnames(meta) <- meta[1,]
    rownames(meta) <- 1:nrow(meta)
    meta <- data.frame(meta)
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
  # add sourcefile name to meta
  allmeta$source <- file
  # lower case column names
  colnames(allmeta) <- tolower(colnames(allmeta))
  ####################
  #read file values
  ####################
  # read meta info from header blocks
  alldata <- plyr::adply(.data=dplyr::filter(.data=fileblocks, val), .margins=1, .fun=function(thisblock)
  {
    col_types <- get_block_col_types(file=file, skip = skipmeta + thisblock$start -1, encoding=encoding)
    # read one meta info block
    data <- readr::read_delim(file=file, delim=";", skip = skipmeta + thisblock$start -1,
                              n_max=thisblock$end - thisblock$start +1, na="-",
                              locale=locale(decimal_mark = ".", date_names = "de", encoding=encoding, time_format="%d.%m.%Y %H:%M"),
                              #col_types = col_types)
                              col_types = col_types,
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


read_lila_spalte <- function(file, filename_metainfo=NA)
{
  indata <- readr::read_csv2(file=file)
  data <- indata
  return(data)
}

# check if filename resembles the standard format (LEG)
check_legformat <- function(filename)
{
  # add .lila, if missing
  if (!grepl(pattern = ".lila$", filename))
    filename <- paste(filename, ".lila", sep="")
  check <- grepl(pattern = "(.*)(^[[:alpha:]]{2,3})_([[:digit:]]+)-(q|w|twas|tzugts|Q|W|TWAS|TZUGTS)([mesivhabpMESIVHABP+]+)(-([[:lower:]]{1}[[:digit:]]{2}|[[:lower:]]{3}))?(-([[:digit:]]{12}))?(-([a-zA-Z_^ ]+))?(-([[:digit:]]{12}))?(-([a-zA-Z_0-9^ ]+))?.lila$", filename)
  #check <- grepl(pattern = "(.*)(^[[:alpha:]]{2,3})_([[:digit:]]+)-(Q|W|TWAS|TZUGTS)(mes|sim|vhs|abs|p*_|-)(-([[:lower:]]{1}[[:digit:]]{2}|[[:lower:]]{3}))?(-([[:digit:]]{12}))?(-([:alpha:]+))?(-([:digit:]]{12}))?(-([[:alpha:]]+))?(.lila$)?", filename)
  return(check)
}
