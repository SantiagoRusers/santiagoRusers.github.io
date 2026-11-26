#' Crear una invitación de calendario (.ics) para una reunión por Zoom
#'
#' La hora se guarda en UTC para que cada calendario la muestre en la zona
#' horaria local de quien la importa.
#'
#' @param inicio Fecha y hora de inicio, como texto "AAAA-MM-DD HH:MM" o POSIXct.
#' @param zoom Link a la reunión de Zoom.
#' @param titulo Título del evento.
#' @param duracion Duración en horas.
#' @param tz Zona horaria de `inicio` (si es texto).
#' @param carpeta Carpeta donde guardar el archivo.
#' @return Nombre del archivo creado (invisible).
crear_ics <- function(
  inicio,
  zoom,
  titulo = "Grupo de usuarios/as de R, Santiago de Chile",
  duracion = 2,
  tz = "America/Santiago",
  carpeta = "."
) {
  if (is.character(inicio)) {
    inicio <- as.POSIXct(inicio, tz = tz)
  }
  utc <- \(x) format(x, "%Y%m%dT%H%M%SZ", tz = "UTC")
  archivo <- paste0("reunion_", format(inicio, "%Y-%m-%d"), ".ics")

  lineas <- c(
    "BEGIN:VCALENDAR",
    "VERSION:2.0",
    "PRODID:-//santiagoRusers//ES",
    "BEGIN:VEVENT",
    # UID estable: re-importar el archivo actualiza el evento en vez de duplicarlo
    paste0("UID:", archivo, "@santiagorusers.github.io"),
    paste0("DTSTAMP:", utc(Sys.time())),
    paste0("DTSTART:", utc(inicio)),
    paste0("DTEND:", utc(inicio + duracion * 3600)),
    paste0("SUMMARY:", escapar_ics(titulo)),
    paste0("LOCATION:", escapar_ics(zoom)),
    paste0("URL:", zoom),
    paste0(
      "DESCRIPTION:",
      escapar_ics(paste0(
        "Conéctate por Zoom: ",
        zoom,
        "\n\nPara más información: https://santiagorusers.github.io"
      ))
    ),
    "END:VEVENT",
    "END:VCALENDAR"
  )

  lineas <- vapply(lineas, plegar_linea_ics, character(1), USE.NAMES = FALSE)
  writeLines(
    enc2utf8(lineas),
    file.path(carpeta, archivo),
    sep = "\r\n",
    useBytes = TRUE
  )
  invisible(archivo)
}

# Escapa caracteres especiales de texto según RFC 5545
escapar_ics <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub(";", "\\;", x, fixed = TRUE)
  x <- gsub(",", "\\,", x, fixed = TRUE)
  gsub("\n", "\\n", x, fixed = TRUE)
}

# Corta líneas a 75 bytes como pide RFC 5545, sin partir caracteres UTF-8
plegar_linea_ics <- function(linea, max_bytes = 75) {
  chars <- strsplit(enc2utf8(linea), "")[[1]]
  bytes <- nchar(chars, type = "bytes")
  partes <- character()
  actual <- ""
  n <- 0
  limite <- max_bytes
  for (i in seq_along(chars)) {
    if (n + bytes[i] > limite) {
      partes <- c(partes, actual)
      actual <- ""
      n <- 0
      # las líneas de continuación empiezan con un espacio
      limite <- max_bytes - 1
    }
    actual <- paste0(actual, chars[i])
    n <- n + bytes[i]
  }
  paste(c(partes, actual), collapse = "\r\n ")
}
