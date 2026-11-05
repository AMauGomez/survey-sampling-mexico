# Download the public data from the official sources ------------------------------
#
# 1. INE, Computos Distritales 2021 (federal deputies): one row per polling-station
#    tally sheet (acta), 163,666 rows. https://computos2021.ine.mx/base-de-datos
# 2. INEGI, Encuesta Nacional de Vivienda (ENVI) 2020, household table (THOGAR),
#    open data. https://www.inegi.org.mx/programas/envi/2020/
# 3. INEGI, ENVI 2020 basic tabulations, theme 5 (housing needs), used only to
#    check that our estimates reproduce the official ones.
#
# Files are saved in data/raw/ (not tracked by git).

options(timeout = 900)

get_zip <- function(url) {
  tmp <- tempfile(fileext = ".zip")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  tmp
}

# 1. INE: the download is a zip that contains another zip
ine_csv <- "data/raw/ine_diputaciones_2021.csv"
if (!file.exists(ine_csv)) {
  outer <- get_zip("https://computos2021.ine.mx/20210611_1000_CW.zip")
  exdir <- tempfile(); unzip(outer, exdir = exdir)
  inner <- list.files(exdir, pattern = "\\.zip$", full.names = TRUE)[1]
  unzip(inner, exdir = exdir)
  src <- list.files(exdir, pattern = "^diputaciones\\.csv$", recursive = TRUE, full.names = TRUE)
  file.copy(src, ine_csv)
}

# 2. ENVI 2020 household table
envi_csv <- "data/raw/envi_2020_thogar.csv"
if (!file.exists(envi_csv)) {
  z <- get_zip("https://www.inegi.org.mx/contenidos/programas/envi/2020/datosabiertos/conjunto_de_datos_envi_2020_csv.zip")
  f <- "conjunto_de_datos_thogar_envi_2020/conjunto_de_datos/conjunto_de_datos_thogar_envi_2020.csv"
  exdir <- tempfile(); unzip(z, files = f, exdir = exdir)
  file.copy(file.path(exdir, f), envi_csv)
}

# 3. Official tabulations (theme 5)
tab_zip <- "data/raw/envi_2020_tabulados_tema05.zip"
if (!file.exists(tab_zip)) {
  download.file("https://www.inegi.org.mx/contenidos/programas/envi/2020/tabulados/envi_2020_tema_05_xlsx.zip",
                tab_zip, mode = "wb", quiet = TRUE)
}

cat("Data ready in data/raw/\n")
