.zip_u16 <- function(value) {
  as.raw(c(value %% 256, floor(value / 256) %% 256))
}

.zip_u32 <- function(value) {
  as.raw(c(
    value %% 256,
    floor(value / 256) %% 256,
    floor(value / 256^2) %% 256,
    floor(value / 256^3) %% 256
  ))
}

.zip_crc32 <- function(payload) {
  hex <- digest::digest(payload, algo = "crc32", serialize = FALSE)
  starts <- c(7L, 5L, 3L, 1L)
  as.raw(strtoi(substring(hex, starts, starts + 1L), base = 16L))
}

write_stored_zip <- function(entries, path) {
  stopifnot(is.list(entries), !is.null(names(entries)))
  output <- file(path, open = "wb")
  on.exit(close(output), add = TRUE)

  central_records <- vector("list", length(entries))
  offset <- 0
  for (i in seq_along(entries)) {
    member_name <- names(entries)[[i]]
    name_raw <- charToRaw(enc2utf8(member_name))
    payload <- entries[[i]]
    if (is.character(payload)) {
      payload <- charToRaw(enc2utf8(payload))
    }
    stopifnot(is.raw(payload))
    size <- length(payload)
    crc <- .zip_crc32(payload)

    local_header <- c(
      as.raw(c(0x50, 0x4b, 0x03, 0x04)),
      .zip_u16(20),
      .zip_u16(0),
      .zip_u16(0),
      .zip_u16(0),
      .zip_u16(0),
      crc,
      .zip_u32(size),
      .zip_u32(size),
      .zip_u16(length(name_raw)),
      .zip_u16(0),
      name_raw
    )
    writeBin(local_header, output)
    writeBin(payload, output)

    central_records[[i]] <- c(
      as.raw(c(0x50, 0x4b, 0x01, 0x02)),
      .zip_u16(20),
      .zip_u16(20),
      .zip_u16(0),
      .zip_u16(0),
      .zip_u16(0),
      .zip_u16(0),
      crc,
      .zip_u32(size),
      .zip_u32(size),
      .zip_u16(length(name_raw)),
      .zip_u16(0),
      .zip_u16(0),
      .zip_u16(0),
      .zip_u16(0),
      .zip_u32(0),
      .zip_u32(offset),
      name_raw
    )
    offset <- offset + length(local_header) + size
  }

  central_directory <- do.call(c, central_records)
  writeBin(central_directory, output)
  writeBin(
    c(
      as.raw(c(0x50, 0x4b, 0x05, 0x06)),
      .zip_u16(0),
      .zip_u16(0),
      .zip_u16(length(entries)),
      .zip_u16(length(entries)),
      .zip_u32(length(central_directory)),
      .zip_u32(offset),
      .zip_u16(0)
    ),
    output
  )
  invisible(path)
}

write_global_pax_tar <- function(path) {
  payload <- charToRaw("5 a=\n")
  header <- raw(512L)
  name <- charToRaw("GlobalHead.1")
  header[seq_along(name)] <- name
  header[125:136] <- c(
    charToRaw(sprintf("%011o", length(payload))),
    as.raw(0L)
  )
  header[[157L]] <- charToRaw("g")
  header[149:156] <- charToRaw("        ")
  checksum <- sum(as.integer(header))
  header[149:156] <- c(
    charToRaw(sprintf("%06o", checksum)),
    as.raw(0L),
    charToRaw(" ")
  )

  padding <- raw((512L - (length(payload) %% 512L)) %% 512L)
  archive <- c(header, payload, padding, raw(1024L))
  output <- gzfile(path, open = "wb")
  on.exit(close(output), add = TRUE)
  writeBin(archive, output)
  invisible(path)
}
