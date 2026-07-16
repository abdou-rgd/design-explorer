# =============================================================================
# archive_utils.R -- bounded, path-safe archive extraction helpers
# =============================================================================

.DESIGN_ARCHIVE_PATTERN <- paste0(
  "\\.(ext|shk|coi|clt|tab|bfm|cpu|ctl|mod|con|cpp)$"
)
.PATAB_ARCHIVE_PATTERN <- "\\.tab(-sim-[0-9]+|-[0-9]+(-[0-9]+)?)$"

.archive_default_max_entries <- function() {
  getOption("designExplorer.archive.maxEntries", 10000L)
}

.archive_default_max_bytes <- function() {
  getOption("designExplorer.archive.maxBytes", 512 * 1024^2)
}

.archive_default_max_pax_records <- function() {
  getOption("designExplorer.archive.maxPaxRecords", 10000L)
}

.archive_validate_limit <- function(value, name, integer = FALSE) {
  if (
    length(value) != 1L ||
      is.na(value) ||
      !is.numeric(value) ||
      !is.finite(value) ||
      value < 1 ||
      (isTRUE(integer) && value != floor(value))
  ) {
    stop(
      name,
      " must be a positive finite ",
      if (isTRUE(integer)) "integer" else "number",
      ".",
      call. = FALSE
    )
  }
  as.numeric(value)
}

.archive_display_name <- function(path) {
  encoded <- encodeString(path, quote = "'", na.encode = TRUE)
  if (nchar(encoded, type = "bytes") > 180L) {
    paste0(substr(encoded, 1L, 176L), "...'")
  } else {
    encoded
  }
}

.validate_archive_members <- function(
  members,
  max_entries = .archive_default_max_entries()
) {
  max_entries <- .archive_validate_limit(
    max_entries,
    "max_entries",
    integer = TRUE
  )
  if (!is.character(members)) {
    stop("Archive member names must be a character vector.", call. = FALSE)
  }
  if (length(members) > max_entries) {
    stop(
      sprintf(
        "Archive contains too many members (%d; limit %.0f).",
        length(members),
        max_entries
      ),
      call. = FALSE
    )
  }
  if (length(members) == 0L) {
    return(data.frame(
      original = character(),
      normalized = character(),
      is_directory = logical(),
      stringsAsFactors = FALSE
    ))
  }
  if (anyNA(members) || any(!nzchar(members))) {
    stop("Archive contains an empty or missing member path.", call. = FALSE)
  }

  slashed <- gsub("\\\\", "/", members)
  is_directory <- grepl("/$", slashed)
  normalized <- character(length(slashed))

  for (i in seq_along(slashed)) {
    member <- slashed[[i]]
    shown <- .archive_display_name(members[[i]])
    if (grepl("[[:cntrl:]]", member)) {
      stop(
        "Archive member path contains control characters: ",
        shown,
        ".",
        call. = FALSE
      )
    }
    if (grepl("^/", member)) {
      stop(
        "Archive contains an absolute member path: ",
        shown,
        ".",
        call. = FALSE
      )
    }
    if (grepl("^[A-Za-z]:", member)) {
      stop(
        "Archive contains a drive-qualified member path: ",
        shown,
        ".",
        call. = FALSE
      )
    }

    parts <- strsplit(member, "/", fixed = TRUE)[[1]]
    if (".." %in% parts) {
      stop(
        "Archive member path escapes the extraction root ('..'): ",
        shown,
        ".",
        call. = FALSE
      )
    }
    parts <- parts[nzchar(parts) & parts != "."]
    if (length(parts) == 0L) {
      if (is_directory[[i]]) {
        normalized[[i]] <- "."
        next
      }
      stop(
        "Archive contains an empty member path after normalization: ",
        shown,
        ".",
        call. = FALSE
      )
    }
    if (any(grepl(":", parts, fixed = TRUE))) {
      stop(
        "Archive member path contains a disallowed ':': ",
        shown,
        ".",
        call. = FALSE
      )
    }
    if (any(grepl("[. ]$", parts))) {
      stop(
        "Archive member path has a segment ending in a dot or space: ",
        shown,
        ".",
        call. = FALSE
      )
    }
    device_stems <- sub("\\..*$", "", parts)
    if (
      any(grepl(
        "^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])$",
        device_stems,
        ignore.case = TRUE
      ))
    ) {
      stop(
        "Archive member path contains a reserved Windows device name: ",
        shown,
        ".",
        call. = FALSE
      )
    }

    candidate <- paste(parts, collapse = "/")
    if (nchar(candidate, type = "bytes") > 4096L) {
      stop("Archive member path is too long: ", shown, ".", call. = FALSE)
    }
    normalized[[i]] <- candidate
  }

  duplicate_key <- tolower(normalized)
  duplicate_idx <- which(duplicated(duplicate_key))[1]
  if (!is.na(duplicate_idx)) {
    first_idx <- match(duplicate_key[[duplicate_idx]], duplicate_key)
    stop(
      "Archive contains duplicate member paths after normalization: ",
      .archive_display_name(members[[first_idx]]),
      " and ",
      .archive_display_name(members[[duplicate_idx]]),
      ".",
      call. = FALSE
    )
  }

  data.frame(
    original = members,
    normalized = normalized,
    is_directory = is_directory,
    stringsAsFactors = FALSE
  )
}

.archive_validate_pattern <- function(pattern) {
  if (
    !is.character(pattern) ||
      length(pattern) != 1L ||
      is.na(pattern) ||
      !nzchar(pattern)
  ) {
    stop("pattern must be one non-empty regular expression.", call. = FALSE)
  }
  invisible(pattern)
}

.archive_select_members <- function(member_info, pattern) {
  .archive_validate_pattern(pattern)
  !member_info$is_directory &
    grepl(
      pattern,
      basename(member_info$normalized),
      ignore.case = TRUE,
      perl = TRUE
    )
}

.archive_prepare_root <- function(exdir) {
  if (
    !is.character(exdir) ||
      length(exdir) != 1L ||
      is.na(exdir) ||
      !nzchar(exdir)
  ) {
    stop("exdir must be one non-empty path.", call. = FALSE)
  }
  if (file.exists(exdir) && !dir.exists(exdir)) {
    stop(
      "Extraction destination is not a directory: ",
      exdir,
      ".",
      call. = FALSE
    )
  }
  if (
    !dir.exists(exdir) &&
      !dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  ) {
    stop("Unable to create extraction destination: ", exdir, ".", call. = FALSE)
  }
  normalizePath(exdir, winslash = "/", mustWork = TRUE)
}

.archive_prepare_destinations <- function(root, members) {
  paths <- character(nrow(members))
  dirs <- character(nrow(members))
  for (i in seq_len(nrow(members))) {
    member_dir <- tempfile(
      pattern = sprintf("archive-member-%05d-", i),
      tmpdir = root
    )
    if (!dir.create(member_dir, showWarnings = FALSE)) {
      stop(
        "Unable to create an isolated archive member directory.",
        call. = FALSE
      )
    }
    dirs[[i]] <- member_dir
    paths[[i]] <- file.path(member_dir, basename(members$normalized[[i]]))
  }
  list(paths = paths, dirs = dirs)
}

.archive_read_exact <- function(con, size, output = NULL) {
  if (size == 0) {
    return(0)
  }
  remaining <- as.numeric(size)
  copied <- 0
  while (remaining > 0) {
    chunk_size <- as.integer(min(65536, remaining))
    chunk <- readBin(con, what = "raw", n = chunk_size)
    if (length(chunk) != chunk_size) {
      stop("Archive ended before the declared member size.", call. = FALSE)
    }
    if (!is.null(output)) {
      writeBin(chunk, output)
    }
    copied <- copied + length(chunk)
    remaining <- remaining - length(chunk)
  }
  copied
}

.archive_copy_zip_member <- function(zipfile, member, destination, expected) {
  input <- NULL
  output <- NULL
  tryCatch(
    {
      input <- unz(zipfile, member, open = "rb")
      output <- file(destination, open = "wb")
      copied <- .archive_read_exact(input, expected, output)
      trailing <- readBin(input, what = "raw", n = 1L)
      if (length(trailing) > 0L) {
        stop(
          "ZIP member exceeds its declared uncompressed size.",
          call. = FALSE
        )
      }
      copied
    },
    error = function(e) {
      stop(
        "Unable to read ZIP member ",
        .archive_display_name(member),
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
    },
    finally = {
      if (!is.null(output) && isOpen(output)) {
        close(output)
      }
      if (!is.null(input) && isOpen(input)) close(input)
    }
  )
}

# Limits can be overridden per call or globally with
# options(designExplorer.archive.maxEntries=, designExplorer.archive.maxBytes=).
safe_extract_zip <- function(
  path,
  exdir,
  pattern,
  max_entries = .archive_default_max_entries(),
  max_uncompressed_bytes = .archive_default_max_bytes()
) {
  max_entries <- .archive_validate_limit(
    max_entries,
    "max_entries",
    integer = TRUE
  )
  max_uncompressed_bytes <- .archive_validate_limit(
    max_uncompressed_bytes,
    "max_uncompressed_bytes"
  )
  .archive_validate_pattern(pattern)
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !file.exists(path) ||
      dir.exists(path)
  ) {
    stop("ZIP archive not found: ", path, ".", call. = FALSE)
  }

  listing_warning <- NULL
  listing <- tryCatch(
    withCallingHandlers(
      utils::unzip(path, list = TRUE),
      warning = function(w) {
        listing_warning <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      stop(
        "Unable to inspect ZIP archive: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!is.null(listing_warning)) {
    stop("Unable to inspect ZIP archive: ", listing_warning, call. = FALSE)
  }
  if (
    !is.data.frame(listing) ||
      !all(c("Name", "Length") %in% names(listing))
  ) {
    stop("ZIP archive has an unreadable member listing.", call. = FALSE)
  }

  members <- .validate_archive_members(
    as.character(listing$Name),
    max_entries = max_entries
  )
  sizes <- suppressWarnings(as.numeric(listing$Length))
  if (
    length(sizes) != nrow(members) ||
      anyNA(sizes) ||
      any(!is.finite(sizes)) ||
      any(sizes < 0) ||
      any(sizes != floor(sizes))
  ) {
    stop(
      "ZIP archive contains an invalid uncompressed member size.",
      call. = FALSE
    )
  }
  total_size <- sum(sizes)
  if (!is.finite(total_size) || total_size > max_uncompressed_bytes) {
    stop(
      sprintf(
        "ZIP archive expands to %.0f bytes, above the %.0f-byte limit.",
        total_size,
        max_uncompressed_bytes
      ),
      call. = FALSE
    )
  }

  selected <- .archive_select_members(members, pattern)
  if (!any(selected)) {
    return(character())
  }
  selected_members <- members[selected, , drop = FALSE]
  selected_sizes <- sizes[selected]
  root <- .archive_prepare_root(exdir)
  prepared <- .archive_prepare_destinations(root, selected_members)
  complete <- FALSE
  on.exit(
    {
      if (!complete) {
        unlink(prepared$dirs, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )

  copied <- numeric(nrow(selected_members))
  for (i in seq_len(nrow(selected_members))) {
    copied[[i]] <- .archive_copy_zip_member(
      path,
      selected_members$original[[i]],
      prepared$paths[[i]],
      selected_sizes[[i]]
    )
  }
  if (!identical(as.numeric(copied), as.numeric(selected_sizes))) {
    stop("ZIP member sizes changed during extraction.", call. = FALSE)
  }

  complete <- TRUE
  stats::setNames(prepared$paths, selected_members$normalized)
}

.tar_field_string <- function(block, start, width) {
  value <- block[seq.int(start, length.out = width)]
  nul <- which(value == as.raw(0L))[1]
  if (!is.na(nul)) {
    value <- if (nul == 1L) raw() else value[seq_len(nul - 1L)]
  }
  if (length(value) == 0L) "" else rawToChar(value)
}

.tar_octal <- function(block, start, width, field) {
  value <- block[seq.int(start, length.out = width)]
  if (length(value) > 0L && as.integer(value[[1]]) >= 128L) {
    stop("Unsupported binary TAR ", field, " field.", call. = FALSE)
  }
  text <- trimws(.tar_field_string(block, start, width))
  if (!nzchar(text)) {
    return(0)
  }
  if (!grepl("^[0-7]+$", text)) {
    stop("Invalid TAR ", field, " field.", call. = FALSE)
  }
  digits <- as.numeric(strsplit(text, "", fixed = TRUE)[[1]])
  sum(digits * 8^rev(seq_along(digits) - 1L))
}

.tar_verify_header <- function(block) {
  expected <- .tar_octal(block, 149L, 8L, "checksum")
  checked <- block
  checked[149:156] <- charToRaw("        ")
  unsigned <- sum(as.integer(checked)) %% 2^24
  signed <- sum(ifelse(
    as.integer(checked) > 127L,
    as.integer(checked) - 256L,
    as.integer(checked)
  )) %%
    2^24
  if (!(expected %in% c(unsigned, signed))) {
    stop("TAR archive contains a header checksum error.", call. = FALSE)
  }
  invisible(TRUE)
}

.tar_trim_metadata_string <- function(payload) {
  if (length(payload) == 0L) {
    return("")
  }
  nul <- which(payload == as.raw(0L))[1]
  if (!is.na(nul)) {
    payload <- if (nul == 1L) raw() else payload[seq_len(nul - 1L)]
  }
  while (
    length(payload) > 0L &&
      tail(payload, 1L) %in% as.raw(c(10L, 13L))
  ) {
    payload <- head(payload, -1L)
  }
  if (length(payload) == 0L) "" else rawToChar(payload)
}

.tar_parse_pax <- function(
  payload,
  max_records = .archive_default_max_pax_records()
) {
  max_records <- .archive_validate_limit(
    max_records,
    "max_records",
    integer = TRUE
  )
  records <- list()
  position <- 1L
  payload_length <- length(payload)
  space_positions <- which(payload == charToRaw(" "))
  space_index <- 1L
  record_count <- 0L
  while (position <= payload_length) {
    record_count <- record_count + 1L
    if (record_count > max_records) {
      stop(
        sprintf(
          "TAR pax metadata contains too many records (%d; limit %.0f).",
          record_count,
          max_records
        ),
        call. = FALSE
      )
    }
    while (
      space_index <= length(space_positions) &&
        space_positions[[space_index]] < position
    ) {
      space_index <- space_index + 1L
    }
    if (
      space_index > length(space_positions) ||
        space_positions[[space_index]] == position
    ) {
      stop("Malformed TAR pax header.", call. = FALSE)
    }
    space_position <- space_positions[[space_index]]
    length_raw <- payload[position:(space_position - 1L)]
    if (
      length(length_raw) > 20L ||
        any(!length_raw %in% charToRaw("0123456789"))
    ) {
      stop("Malformed TAR pax record length.", call. = FALSE)
    }
    record_length <- suppressWarnings(as.numeric(rawToChar(length_raw)))
    if (
      is.na(record_length) ||
        !is.finite(record_length) ||
        record_length != floor(record_length) ||
        record_length < (space_position - position + 3L)
    ) {
      stop("Malformed TAR pax record length.", call. = FALSE)
    }
    record_end <- position + record_length - 1L
    if (record_end > payload_length) {
      stop("Truncated TAR pax record.", call. = FALSE)
    }
    value_start <- space_position + 1L
    record <- payload[value_start:record_end]
    if (tail(record, 1L) == charToRaw("\n")) {
      record <- head(record, -1L)
    }
    equals <- which(record == charToRaw("="))[1]
    if (!is.na(equals) && equals > 1L) {
      key <- rawToChar(record[seq_len(equals - 1L)])
      value <- if (equals == length(record)) {
        ""
      } else {
        rawToChar(record[(equals + 1L):length(record)])
      }
      records[[key]] <- value
    }
    position <- record_end + 1L
  }
  attr(records, "record_count") <- record_count
  records
}

.tar_read_payload <- function(con, size, capture = FALSE) {
  if (isTRUE(capture)) {
    payload <- readBin(con, what = "raw", n = as.integer(size))
    if (length(payload) != size) {
      stop("TAR archive ended inside a metadata record.", call. = FALSE)
    }
  } else {
    .archive_read_exact(con, size)
    payload <- NULL
  }
  padding <- (512 - (size %% 512)) %% 512
  if (padding > 0) {
    .archive_read_exact(con, padding)
  }
  payload
}

.tar_walk <- function(
  path,
  max_entries,
  max_uncompressed_bytes,
  max_pax_records,
  destinations = NULL
) {
  con <- gzfile(path, open = "rb")
  on.exit(close(con), add = TRUE)
  rows <- list()
  header_count <- 0L
  payload_bytes <- 0
  pending_name <- NULL
  pending_link <- NULL
  pending_pax <- list()
  extracted <- character()
  member_count <- 0L
  pax_record_count <- 0L

  repeat {
    block <- readBin(con, what = "raw", n = 512L)
    if (length(block) == 0L) {
      break
    }
    if (length(block) != 512L) {
      stop("TAR archive ends with an incomplete header.", call. = FALSE)
    }
    if (all(block == as.raw(0L))) {
      break
    }
    header_count <- header_count + 1L
    if (header_count > (4 * max_entries + 100)) {
      stop(
        sprintf(
          "TAR archive contains too many metadata headers (%d).",
          header_count
        ),
        call. = FALSE
      )
    }
    .tar_verify_header(block)

    name <- .tar_field_string(block, 1L, 100L)
    prefix <- .tar_field_string(block, 346L, 155L)
    if (nzchar(prefix)) {
      name <- paste(prefix, name, sep = "/")
    }
    size <- .tar_octal(block, 125L, 12L, "size")
    type <- .tar_field_string(block, 157L, 1L)
    link <- .tar_field_string(block, 158L, 100L)

    if (type == "g") {
      stop(
        paste0(
          "TAR global pax headers are not supported because they can ",
          "override paths and sizes for subsequent members."
        ),
        call. = FALSE
      )
    }
    if (type %in% c("L", "K", "x")) {
      if (size > 1024^2) {
        stop("TAR metadata record exceeds the 1 MiB limit.", call. = FALSE)
      }
      payload_bytes <- payload_bytes + size
      if (
        !is.finite(payload_bytes) ||
          payload_bytes > max_uncompressed_bytes
      ) {
        stop(
          sprintf(
            "TAR archive expands above the %.0f-byte limit.",
            max_uncompressed_bytes
          ),
          call. = FALSE
        )
      }
      payload <- .tar_read_payload(con, size, capture = TRUE)
      if (type == "L") {
        pending_name <- .tar_trim_metadata_string(payload)
      } else if (type == "K") {
        pending_link <- .tar_trim_metadata_string(payload)
      } else if (type == "x") {
        parsed_pax <- .tar_parse_pax(
          payload,
          max_records = max_pax_records
        )
        pax_record_count <- pax_record_count +
          attr(parsed_pax, "record_count", exact = TRUE)
        if (pax_record_count > max_pax_records) {
          stop(
            sprintf(
              paste0(
                "TAR archive contains too many pax metadata records ",
                "(%d; limit %.0f)."
              ),
              pax_record_count,
              max_pax_records
            ),
            call. = FALSE
          )
        }
        attr(parsed_pax, "record_count") <- NULL
        pending_pax <- parsed_pax
      }
      next
    }

    if (!is.null(pending_name)) {
      name <- pending_name
    }
    if (!is.null(pending_link)) {
      link <- pending_link
    }
    if (!is.null(pending_pax$path)) {
      name <- pending_pax$path
    }
    if (!is.null(pending_pax$linkpath)) {
      link <- pending_pax$linkpath
    }
    if (!is.null(pending_pax$size)) {
      pax_size <- suppressWarnings(as.numeric(pending_pax$size))
      if (
        is.na(pax_size) ||
          !is.finite(pax_size) ||
          pax_size < 0 ||
          pax_size != floor(pax_size)
      ) {
        stop("Invalid TAR pax size field.", call. = FALSE)
      }
      size <- pax_size
    }
    pending_name <- NULL
    pending_link <- NULL
    pending_pax <- list()

    member_count <- member_count + 1L
    if (member_count > max_entries) {
      stop(
        sprintf(
          "TAR archive contains too many members (%d; limit %.0f).",
          member_count,
          max_entries
        ),
        call. = FALSE
      )
    }
    if (!nzchar(name)) {
      stop("TAR archive contains an empty member name.", call. = FALSE)
    }
    is_regular <- type %in% c("", "0", "7")
    payload_bytes <- payload_bytes + size
    if (
      !is.finite(payload_bytes) ||
        payload_bytes > max_uncompressed_bytes
    ) {
      stop(
        sprintf(
          "TAR archive expands above the %.0f-byte limit.",
          max_uncompressed_bytes
        ),
        call. = FALSE
      )
    }

    rows[[length(rows) + 1L]] <- data.frame(
      name = name,
      size = size,
      type = type,
      link_target = link,
      stringsAsFactors = FALSE
    )

    destination_idx <- if (is.null(destinations)) {
      0L
    } else {
      match(name, names(destinations), nomatch = 0L)
    }
    if (destination_idx > 0L) {
      if (!is_regular) {
        stop(
          "Refusing to extract non-regular TAR member ",
          .archive_display_name(name),
          ".",
          call. = FALSE
        )
      }
      output <- file(destinations[[destination_idx]], open = "wb")
      tryCatch(
        .archive_read_exact(con, size, output),
        finally = close(output)
      )
      extracted <- c(extracted, name)
      padding <- (512 - (size %% 512)) %% 512
      if (padding > 0) .archive_read_exact(con, padding)
    } else {
      .tar_read_payload(con, size, capture = FALSE)
    }
  }

  info <- if (length(rows) == 0L) {
    data.frame(
      name = character(),
      size = numeric(),
      type = character(),
      link_target = character(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, rows)
  }
  list(info = info, extracted = extracted)
}

# PAX metadata is additionally bounded by
# options(designExplorer.archive.maxPaxRecords=).
safe_extract_tar <- function(
  path,
  exdir,
  pattern,
  max_entries = .archive_default_max_entries(),
  max_uncompressed_bytes = .archive_default_max_bytes(),
  max_pax_records = .archive_default_max_pax_records()
) {
  max_entries <- .archive_validate_limit(
    max_entries,
    "max_entries",
    integer = TRUE
  )
  max_uncompressed_bytes <- .archive_validate_limit(
    max_uncompressed_bytes,
    "max_uncompressed_bytes"
  )
  max_pax_records <- .archive_validate_limit(
    max_pax_records,
    "max_pax_records",
    integer = TRUE
  )
  .archive_validate_pattern(pattern)
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !file.exists(path) ||
      dir.exists(path)
  ) {
    stop("TAR archive not found: ", path, ".", call. = FALSE)
  }

  scan <- tryCatch(
    .tar_walk(
      path,
      max_entries = max_entries,
      max_uncompressed_bytes = max_uncompressed_bytes,
      max_pax_records = max_pax_records
    ),
    error = function(e) {
      stop(
        "Unable to inspect TAR archive: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  members <- .validate_archive_members(
    scan$info$name,
    max_entries = max_entries
  )
  selected <- .archive_select_members(members, pattern)
  if (!any(selected)) {
    return(character())
  }
  regular_types <- scan$info$type %in% c("", "0", "7")
  invalid <- which(selected & !regular_types)[1]
  if (!is.na(invalid)) {
    stop(
      "Refusing to extract non-regular TAR member ",
      .archive_display_name(scan$info$name[[invalid]]),
      ".",
      call. = FALSE
    )
  }

  selected_members <- members[selected, , drop = FALSE]
  root <- .archive_prepare_root(exdir)
  prepared <- .archive_prepare_destinations(root, selected_members)
  destinations <- stats::setNames(
    prepared$paths,
    scan$info$name[selected]
  )
  complete <- FALSE
  on.exit(
    {
      if (!complete) {
        unlink(prepared$dirs, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )

  extraction <- tryCatch(
    .tar_walk(
      path,
      max_entries = max_entries,
      max_uncompressed_bytes = max_uncompressed_bytes,
      max_pax_records = max_pax_records,
      destinations = destinations
    ),
    error = function(e) {
      stop(
        "Unable to extract TAR archive: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!identical(scan$info, extraction$info)) {
    stop("TAR archive changed during extraction.", call. = FALSE)
  }
  if (!identical(unname(extraction$extracted), unname(names(destinations)))) {
    stop("TAR extraction did not produce every selected member.", call. = FALSE)
  }
  sizes <- file.info(prepared$paths)$size
  expected <- scan$info$size[selected]
  if (anyNA(sizes) || !identical(as.numeric(sizes), as.numeric(expected))) {
    stop("TAR member sizes changed during extraction.", call. = FALSE)
  }

  complete <- TRUE
  stats::setNames(prepared$paths, selected_members$normalized)
}
