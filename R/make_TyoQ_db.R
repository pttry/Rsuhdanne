#' Read Tyovoimatiedustelun maakuntatauluja and write to rds-file
#'
#'
#' Read "Maakunta1" and "Maakunta2" px-files and write to rds file.
#' Add Ita-Uusimaa to Uusimaa if prensent.
#' writes variables from for Maakunta1.
#' Ratios are omit because aggregating Uusimaa and Ita-Uusimaa.
#' 
#' 
#' @param to_file A path to file to write. If \code{NULL} just returns a data.frame (invisibly)
#' @param ... parametres to \code{link{tyoQfiles}}. For now \code{ddir} for
#'            directory of input data.
#' 
#' @return (invisibly) a data.frame and write to rds-file, if given
#'
#' @family tyoQ
#' @import dplyr 
#' @export
#' @encoding UTF-8
#' @keywords IO
#' @examples 
#'   \dontrun{
#'   tyoq_dir <- file.path(path.expand('~'), "..", 
#'   "Pellervon Taloustutkimus PTT ry/Data - Tiedostot/Työmarkkinat/Työvoimatutkimus MK")
#' k <- make_TyoQ_1_rds(ddir = tyoq_dir)
#' k <- make_TyoQ_2_rds(ddir = tyoq_dir)
#' }
make_TyoQ_1_rds <- function(to_file = NULL, ...){
    
  # Luettavat tiedostot 1
  files1 <- tyoQfiles("aakunta1", ...)
  
  # read px-files
  dat_px <- lapply(files1, function(x) as.data.frame(read_px(x, encoding = "latin1")))
  
  ## clean data
  # names
  dat0 <- lapply(dat_px, function(x){
    nn <- names(x)
    nn[grep("aakunta", nn)] <- "Maakunta"
    nn[grep("Ik\u00e4", nn)] <- "Ika"
    names(x) <- nn
    x
  })
  
  # to data.frame
  dat1 <- do.call(rbind, dat0)
  
  # only last observations
  dat1 <- dplyr::distinct(dat1[nrow(dat1):1L,], 
                          Maakunta, Ajanjakso, Vuosi, Ika, Tiedot, Sukupuoli,
                          .keep_all = TRUE)
  dat1 <- dat1[nrow(dat1):1L,]

  # delete missing (fron end) and compelte (for other deleted)
  dat1 <- subset(dat1, !is.na(data))
  dat1 <- tidyr::complete(dat1, 
                          Maakunta, tidyr::nesting(Ajanjakso, Vuosi), Ika, Tiedot, Sukupuoli)
  
  # Unify naming
  dat1$Tiedot <- plyr::mapvalues(dat1$Tiedot, "Ty\u00f6voimaan kuulumattomat yhteens\u00e4", "Ty\u00f6voimaan kuulumattomat")  
  
  # Ita-Uusimaa to uusimaa
  dat1 <- dplyr::filter(dat1, !(Maakunta == "It\u00e4-Uusimaa" & Vuosi == "2010"))   # I-Uusimaa is already in Uusimaa in most resent 2010 data
  dat1$Maakunta <- plyr::mapvalues(dat1$Maakunta, "It\u00e4-Uusimaa", "Uusimaa")

  
  dat2 <- dat1 %>%
    filter(Maakunta != " ",
           Maakunta != "",
           Tiedot %in% c("Ty\u00f6ik\u00e4inen v\u00e4est\u00f6", "Ty\u00f6lliset", "Ty\u00f6tt\u00f6m\u00e4t", "Ty\u00f6voima", "Ty\u00f6voimaan kuulumattomat"))  %>%            # Tyhj\u00e4 maakunta pois
    dplyr::distinct() %>%
    dplyr::group_by(Maakunta, Ajanjakso, Vuosi, Ika, Tiedot, Sukupuoli) %>%
    dplyr::summarise(data = sum(data, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    
    droplevels()
  
  if (!is.null(to_file)) saveRDS(dat2, file = to_file)
  
  invisible(dat2)
  
} 

#' @rdname make_TyoQ_1_rds
#' @export
make_TyoQ_2_rds <- function(to_file = NULL, ...){
  
  # Luettavat tiedostot 1
  files1 <- tyoQfiles("aakunta2", ...)[-1] # ensimmaisessa vanha toimialajako, ei kayteta
  
  # read px-files
  dat_px <- lapply(files1, function(x) as.data.frame(read_px(x, encoding = "latin1")))
  
  ## clean data
  # names
  dat0 <- lapply(dat_px, function(x){
    nn <- names(x)
    nn[grep("aakunta", nn)] <- "Maakunta"
    nn[grep("TOL", nn)] <- "tol"
    names(x) <- nn
    x
  })
  
  # to data.frame
  dat1 <- do.call(rbind, dat0)
  

  # Unify tol
  
  tol_key <- c(
    "A-B" = "Alkutuotanto",
    "C-E" = "Teollisuus",
    "F" = "Rakentaminen",
    "G" = "Kauppa",
    "H" = "Kuljetus",
    "I" = " Majoitus- ja ravitsemistoiminta",
    "J" = "Informaatio ja viestint\u00e4",
    "K-L" = "Rahoitus, vakuutus ja kiinteist\u00f6",
    "M-N" = "Liike-el\u00e4m\u00e4n palvelut",
    "O" = "Julkinen hallinto",
    "P" = "Koulutus",
    "Q" = "Terveys- ja sosiaalipalvelut",
    "R-U" = "Muut palvelut",
    "000" = "Yhteens\u00e4",
    "X" = "Toimiala tuntematon")
  
  dat1 <- dat1 %>% 
    filter(!(tol == " ")) %>% 
    mutate(tol_code = stringr::str_extract(tol, "^.{1,4}  |Toimialat"),
           tol_code = stringr::str_trim(tol_code),
           tol_code = stringr::str_replace(tol_code, ", ", "-"),
           tol_code = forcats::fct_recode(tol_code, "000" = "Toimialat"),
           tol_code = factor(tol_code, levels = names(tol_key)),
           tol = factor(tol_code, levels = names(tol_key), labels = tol_key))
  
  # only last observations
  dat1 <- dplyr::distinct(dat1[nrow(dat1):1L,], 
                          Maakunta, Ajanjakso, Vuosi, tol_code, tol, Tiedot, .keep_all = TRUE)
  dat1 <- dat1[nrow(dat1):1L,]  
  
  # delete missing (fron end) and compelte (for other deleted)
  
  dat1 <- subset(dat1, !is.na(data))
  dat1 <- tidyr::complete(dat1, 
                          Maakunta, tidyr::nesting(Ajanjakso, Vuosi), 
                          tidyr::nesting(tol_code, tol), Tiedot)
  
  # Ita-Uusimaa to uusimaa
  dat1 <- dplyr::filter(dat1, !(Maakunta == "It\u00e4-Uusimaa" & Vuosi == "2010"))   # I-Uusimaa is already in Uusimaa in most resent 2010 data
  dat1$Maakunta <- dplyr::recode(dat1$Maakunta, "It\u00e4-Uusimaa" = "Uusimaa")
  
  
  dat2 <- dat1 %>%
    dplyr::filter(Maakunta != " ",
                  Maakunta != "",
                  !is.na(tol))  %>%         # Tyhj\u00e4 maakunta ja tol pois
    dplyr::distinct() %>%
    dplyr::group_by(Maakunta, Ajanjakso, Vuosi, tol_code, tol, Tiedot) %>%
    dplyr::summarise(data = sum(data, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    droplevels()
  
  if (!is.null(to_file)) saveRDS(dat2, file = to_file)
  
  invisible(dat2)
  
} 
