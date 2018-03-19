#' Read Tyovoimatiedustelun maakuntatauluja
#' 
#' read files based on their vars list. Add Itä-Uusimaa to Uusimaa if prensent 
#' Set a file specific vars list because variables and they names change. If
#' changes in future code on this function might have to be changed. Calls
#' \code{\link{read_tyoQ}}
#' @param vuosi a numeric, a year for time-series to start
#' @param mk A vector for maakunnat
#' @param file1 a string, filepath for the first query to write
#' @param file2 a string, filepath for the second query to write
#' @param tiedot1 a vector for variables in the first query
#' @param TOL1 a vector for the first set of TOL names in the second query
#' @param TOL2 a vector for the second set of TOL names in the second query
#' @param TOL3 a vector for the 3. set of TOL names
#' @param TOL4 a vector for the 4 set of TOL names
#'   
#' @family tyoQ
#' @export
#' @encoding UTF-8
#' @keywords IO
makeTyoQ <- function(
  vuosi,
  mk = c("Koko maa", "Uusimaa", "Itä-Uusimaa", "Varsinais-Suomi", "Satakunta", "Kanta-Häme", "Pirkanmaa", 
          "Päijät-Häme", "Kymenlaakso", "Etelä-Karjala", "Etelä-Savo", "Pohjois-Savo", "Pohjois-Karjala", 
          "Keski-Suomi", "Etelä-Pohjanmaa", "Pohjanmaa", "Keski-Pohjanmaa", "Pohjois-Pohjanmaa", "Kainuu", 
          "Lappi", "Ahvenanmaa"),
  file1 = file.path(tietok_dir(), "SarjatTK/MK_Työlliset.xlsx"),
  file2 = file.path(tietok_dir(), "SarjatTK/MK_Työlliset_TOL.xlsx"),
  tiedot1 = c("Työikäinen väestö", "Työvoima", "Työlliset", "Työttömät"),
  TOL1 = c("A, B  Maatalous, metsätalous, kalatalous; kaivostoiminta (01-09)",                    
            "C-E  Teollisuus; sähkö-, lämpö-, vesi- ja jätehuolto yms. (10-39)",                   
            "F  Rakentaminen (41-43)",                                                             
            "G  Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus (45-47)",
            "H   Kuljetus ja varastointi (49-53)",                                                 
            "I   Majoitus- ja ravitsemistoiminta (55-56)",                                         
            "J   Informaatio ja viestintä (58-63)",                                                
            "K, L  Rahoitus- ja vakuutustoiminta; kiinteistöala (64-68)",                          
            "M, N  Liike-elämän palvelut (69-82)",                                                 
            "O   Julkinen hallinto ja maanpuolustus; pakollinen sosiaalivakuutus (84)",            
            "P   Koulutus (85)",                                                                   
            "Q   Terveys- ja sosiaalipalvelut (86-88)",                                            
            "R-U  Muu palvelutoiminta (90-99)",                                                    
            "Toimialat yhteensä (00-99)"                                                          
            ),
  TOL2 = c("A, B   Maa-, metsä- ja kalatalous; kaivostoiminta (01-09)",                            
            "C-E   Teollisuus; sähkö-, lämpö-, vesi- ja jätehuolto yms. (10-39)",                   
            "F   Rakentaminen (41-43)",                                                             
            "G   Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus (45-47)",
            "H   Kuljetus ja varastointi (49-53)",                                                  
            "I   Majoitus- ja ravitsemistoiminta (55-56)",                                          
            "J   Informaatio ja viestintä (58-63)",                                                 
            "K, L   Rahoitus- ja vakuutustoiminta; kiinteistöala (64-68)",                          
            "M, N   Liike-elämän palvelut (69-82)",                                                 
            "O   Julkinen hallinto ja maanpuolustus; pakollinen sosiaalivakuutus (84)",             
            "P   Koulutus (85)",                                                                    
            "Q   Terveys- ja sosiaalipalvelut (86-88)",                                             
            "R-U   Muu palvelutoiminta (90-99)",                                                    
            "Toimialat yhteensä (00-99)"                                                           
            ),
  
  TOL3 = c("A, B   Maatalous, metsätalous, kalatalous; kaivostoiminta (01-09)",                    
            "C-E   Teollisuus; sähkö-, lämpö-, vesi- ja jätehuolto yms. (10-39)",
            "F   Rakentaminen (41-43)",
            "G   Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus (45-47)",
            "H   Kuljetus ja varastointi (49-53)",
            "I   Majoitus- ja ravitsemistoiminta (55-56)",
            "J   Informaatio ja viestintä (58-63)",
            "K, L   Rahoitus- ja vakuutustoiminta; kiinteistöala (64-68)",
            "M, N   Liike-elämän palvelut (69-82)",                              
            "O   Julkinen hallinto ja maanpuolustus; pakollinen sosiaalivakuutus (84)",
            "P   Koulutus (85)",
            "Q   Terveys- ja sosiaalipalvelut (86-88)",
            "R-U   Muu palvelutoiminta (90-99)",
            "Toimialat yhteensä (00-99)"
            ),
  
  TOL4 = c("A, B   Maatalous, metsätalous, kalatalous; kaivostoiminta (01-09)",                    
           "C-E   Teollisuus; sähkö-, lämpö-, vesi- ja jätehuolto yms. (10-39)",
           "F   Rakentaminen (41-43)",
           "G   Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus (45-47)",
           "H   Kuljetus ja varastointi (49-53)",
           "I   Majoitus- ja ravitsemistoiminta (55-56)",
           "J   Informaatio ja viestintä (58-63)",
           "K, L   Rahoitus- ja vakuutustoiminta; kiinteistöala (64-68)",
           "M, N   Liike-elämän palvelut (69-82)",                              
           "O   Julkinen hallinto ja maanpuolustus; pakollinen sosiaalivakuutus (84)",
           "P   Koulutus (85)",
           "Q   Terveys- ja sosiaalipalvelut (86-88)",
           "R-U   Taiteet, viihde ja virkistys; muu palvelutoiminta (90-99)",
           "Toimialat yhteensä (00-99)"
            )  
  )
{

  vars1.1 = list(
    maakunta_1998 = mk,
    "Ikä" = "15-74",
    Tiedot = tiedot1,
    Sukupuoli = "Sukupuolet yhteensä"
  )
  vars1.2 <- vars1.1
  names(vars1.2)[names(vars1.2) == "maakunta_1998"] <- "maakunta_2011"
  vars1.3 <- vars1.2
  names(vars1.3)[names(vars1.3) == "maakunta_2011"] <- "Maakunta_2011"
  
  # Luettavat tiedostot 1
  files1 <- tyoQfiles("aakunta1")
  
  vars1 <- vector("list", length(files1))
  for (i in 1:9) vars1[[i]] <- vars1.1
  for (i in 10:11) vars1[[i]] <- vars1.2
  for (i in 12:length(files1)) vars1[[i]] <- vars1.3
  
  data1 <- lapply(seq_along(tiedot1), function(x, files = files1, vars = vars1){
    for (i in seq_along(files)) vars[[i]]$Tiedot <- vars[[i]]$Tiedot[x]
    d <- read_tyoQ(files, vars, year = vuosi)
    d
  })
  
  write2xlsx(data1, sheetnames = tiedot1, file = file1, overwrite = TRUE)
  
  # Toimialadata
  
  # Muuttujat eri vuosille
  vars2.1 = list(
    Maakunta_1998 = mk,
    Tiedot = "Työlliset yhteensä",
    TOL.2008 = TOL1
  )
  vars2.2 = list(
    maakunta_2011 = mk,
    Tiedot = "Työlliset yhteensä",
    TOL.2008 = TOL2
  )
  vars2.3 = list(
    maakunta_2011 = mk,
    Tiedot = "Työlliset yhteensä",
    TOL.2008 = TOL3
  )
  vars2.4 = list(
    maakunta_2011 = mk,
    Tiedot = "Työlliset yhteensä",
    TOL.2008 = TOL4
  )
  
  #luettavat tiedostot 2
  files2 <- tyoQfiles("Maakunta2")[-1] # ensimmaisessa vanha toimialajako, ei kayteta
  
  vars2 <- vector("list", length(files2))
  for (i in 1:8) vars2[[i]] <- vars2.1
  for (i in 9:16) vars2[[i]] <- vars2.2
  for (i in 17:24) vars2[[i]] <- vars2.3
  for (i in 25:length(files2)) vars2[[i]] <- vars2.4
  
  data2 <- lapply(seq_along(TOL1), function(x, files = files2, vars = vars2){
    for (i in seq_along(files)) vars[[i]]$TOL.2008 <- vars[[i]]$TOL.2008[x]
    message(vars[[i]]$TOL.2008)
    d <- read_tyoQ(files, vars, year = vuosi)
    d
  })
  
  
  
  write2xlsx(data2, sheetnames = substr(TOL2, 1, 15), file = file2, overwrite = TRUE)
TRUE
} 

