# maxdiff-prep.R
# Utilities to (1) read Qualtrics numeric MaxDiff exports, (2) reformat for choiceTools,
# (3) optionally add anchor columns, and (4) write Qualtrics-style 3-row CSVs.

# Imports expected in package DESCRIPTION:
# Imports: dplyr, readr

# ---------------------------
# 1) Read + filter Qualtrics
# ---------------------------

#' Read Qualtrics MaxDiff numeric download and set sys_RespNum
#'
#' @param filename CSV path
#' @param skip Rows to skip before data begins (default 2)
#' @param header_rows Rows to read to capture correct column names (default 4)
#' @param id_col Column used to build sys_RespNum (default "ResponseId")
#' @return data.frame with correct column names and sys_RespNum
read_qualtrics_numeric_maxdiff <- function(
  filename,
  skip = 2,
  header_rows = 4,
  id_col = "ResponseId"
) {
  df <- readr::read_csv(filename, skip = skip, show_col_types = FALSE)
  col_names <- readr::read_csv(filename, n_max = header_rows, show_col_types = FALSE)

  names(df) <- names(col_names)

  if (!(id_col %in% names(df))) {
    stop("Expected id_col '", id_col, "' not found in file.")
  }

  df$sys_RespNum <- seq_along(df[[id_col]])
  df
}

#' Filter to rows with MaxDiff present
#'
#' @param df Qualtrics df
#' @param maxdiff_flag_col Column indicating MaxDiff presence (default "vers_MAXDIFF")
#' @return filtered df
filter_maxdiff_rows <- function(df, maxdiff_flag_col = "vers_MAXDIFF") {
  if (!(maxdiff_flag_col %in% names(df))) {
    stop("Expected maxdiff_flag_col '", maxdiff_flag_col, "' not found.")
  }
  dplyr::filter(df, !is.na(.data[[maxdiff_flag_col]]))
}

# -------------------------------------------
# 2) Reformat MaxDiff to choiceTools wide CSV
# -------------------------------------------

#' Reformat Qualtrics MaxDiff into choiceTools-wide CSV structure (no anchor)
#'
#' @param df Filtered Qualtrics df with sys_RespNum
#' @param all_labels Character vector of item labels in master order (length = num_items)
#' @param num_questions Number of MaxDiff tasks
#' @param num_item_per_question Items shown per task
#' @param choice_prefix Prefix for choice columns (default "C"), expects C{q}_{i}
#' @param label_suffix Suffix for label columns (default "_MAXDIFF"), expects {q}.{i}_MAXDIFF
#' @return list(df_expanded, headers=list(first,second,third), lookup)
maxdiff_reformat_for_choicetools <- function(
  df,
  all_labels,
  num_questions,
  num_item_per_question,
  choice_prefix = "C",
  label_suffix = "_MAXDIFF"
) {
  stopifnot(length(all_labels) > 1)
  num_items <- length(all_labels)

  lookup <- data.frame(
    label = all_labels,
    code = seq_along(all_labels),
    stringsAsFactors = FALSE
  )

  # Qualtrics-style 3 header rows expected by parse.md.qualtrics
  second.row <- c("sys_RespNum")
  third.row  <- c("{'ImportId': 'sys_RespNum'}")

  df_expanded <- data.frame(sys_RespNum = df$sys_RespNum)

  for (q in seq_len(num_questions)) {
    choice_cols <- paste0(choice_prefix, q, "_", seq_len(num_item_per_question))
    label_cols  <- paste0(q, ".", seq_len(num_item_per_question), label_suffix)

    missing_choice <- setdiff(choice_cols, names(df))
    missing_label  <- setdiff(label_cols, names(df))
    if (length(missing_choice) > 0 || length(missing_label) > 0) {
      stop(
        "Missing columns for question ", q, ". Missing choice: ",
        paste(missing_choice, collapse = ", "),
        " | Missing labels: ",
        paste(missing_label, collapse = ", ")
      )
    }

    # Create per-item columns Q{q}_{idx} filled with choice code when shown
    for (idx in seq_len(num_items)) {
      item <- all_labels[idx]
      new_col <- paste0("Q", q, "_", idx)
      df_expanded[[new_col]] <- NA

      second.row <- c(second.row, paste0("(Your MaxDiff Question Body)?%-%", item))
      third.row  <- c(third.row,  paste0("{'ImportId':", new_col, "}"))

      for (i in seq_len(num_item_per_question)) {
        mask <- df[[label_cols[i]]] == item
        df_expanded[[new_col]][mask] <- df[[choice_cols[i]]][mask]
      }
    }

    # Display order column, numeric codes joined with "|"
    label_num <- matrix(NA_integer_, nrow = nrow(df), ncol = num_item_per_question)
    for (i in seq_len(num_item_per_question)) {
      label_num[, i] <- lookup$code[match(df[[label_cols[i]]], lookup$label)]
    }
    df_expanded[[paste0("DO-Q-Q", q)]] <- apply(label_num, 1, paste, collapse = "|")

    second.row <- c(second.row, "Display Order: (Your MaxDiff Question Body)?")
    third.row  <- c(third.row,  paste0("{'ImportId':", "DO-Q-Q", q, "}"))
  }

  headers <- list(
    first  = colnames(df_expanded),
    second = second.row,
    third  = third.row
  )

  list(df_expanded = df_expanded, headers = headers, lookup = lookup)
}

# ---------------------------------------
# 3) Write Qualtrics-style 3-row CSV file
# ---------------------------------------

#' Write choiceTools-compatible Qualtrics-style CSV (3 header rows + data)
#'
#' @param df_wide Wide df to write
#' @param headers list(first, second, third)
#' @param exportfile output path
#' @return exportfile (invisible)
write_qualtrics_3row_csv <- function(df_wide, headers, exportfile) {
  hdr <- rbind(headers$first, headers$second, headers$third)
  write.table(hdr, file = exportfile, sep = ",", row.names = FALSE, col.names = FALSE)
  write.table(df_wide, file = exportfile, sep = ",",
              row.names = FALSE, col.names = FALSE, append = TRUE)
  invisible(exportfile)
}

# --------------------------------------------------------
# 4) Anchor prep mode A: Direct Binary (single raw prefix)
#    Raw columns: {question_prefix}{1..total_item} coded 1/2
#    Output columns appended: Imp_1..k, NotImp_1..k (item IDs)
# --------------------------------------------------------

#' Prepare anchor columns (Direct Binary) and combine with expanded MaxDiff data
#'
#' @param df_raw Raw df with sys_RespNum and anchor columns
#' @param df_expanded Output from maxdiff_reformat_for_choicetools()
#' @param headers Headers from maxdiff_reformat_for_choicetools()
#' @param question_prefix Raw anchor prefix (e.g. "QID123_")
#' @param total_item Total items in anchor question
#' @param num_anchor_item Number of Imp/NotImp slots to store (default 5)
#' @param out_prefix Pair for output prefix: c("Imp","NotImp") (default)
#' @param exportfile If not NULL, write the anchored CSV
#' @return list(combined_df, headers)
maxdiff_anchor_from_direct_binary <- function(
  df_raw,
  df_expanded,
  headers,
  question_prefix,
  total_item,
  num_anchor_item = 5,
  out_prefix = c("Imp", "NotImp"),
  exportfile = NULL
) {
  if (length(out_prefix) != 2) stop("out_prefix must be length-2, e.g. c('Imp','NotImp').")

  anchor_cols <- paste0(question_prefix, seq_len(total_item))
  missing <- setdiff(anchor_cols, names(df_raw))
  if (length(missing) > 0) {
    stop("Missing anchor columns in raw CSV: ", paste(missing, collapse = ", "))
  }

  df_anchor <- df_raw %>% dplyr::select(sys_RespNum, dplyr::all_of(anchor_cols))

  # Build output Imp_*/NotImp_* (item IDs)
  out <- data.frame(sys_RespNum = df_anchor$sys_RespNum)
  for (k in seq_len(num_anchor_item)) {
    out[[paste0(out_prefix[1], "_", k)]] <- NA_integer_
    out[[paste0(out_prefix[2], "_", k)]] <- NA_integer_
  }

  for (r in seq_len(nrow(df_anchor))) {
    row_vals <- suppressWarnings(as.numeric(df_anchor[r, anchor_cols, drop = TRUE]))
    imp_items <- which(row_vals == 1)
    not_items <- which(row_vals == 2)

    if (length(imp_items) > 0) {
      n <- min(length(imp_items), num_anchor_item)
      out[r, paste0(out_prefix[1], "_", seq_len(n))] <- imp_items[seq_len(n)]
    }
    if (length(not_items) > 0) {
      n <- min(length(not_items), num_anchor_item)
      out[r, paste0(out_prefix[2], "_", seq_len(n))] <- not_items[seq_len(n)]
    }
  }

  combined_df <- dplyr::left_join(df_expanded, out, by = "sys_RespNum")

  # Extend headers for new columns
  new_cols <- setdiff(names(combined_df), headers$first)
  headers2 <- headers
  headers2$first  <- names(combined_df)
  headers2$second <- c(headers$second, paste0("Anchor%-%", new_cols))
  headers2$third  <- c(headers$third,  paste0("{'ImportId':", new_cols, "}"))

  if (!is.null(exportfile)) {
    write_qualtrics_3row_csv(combined_df, headers2, exportfile)
  }

  list(combined_df = combined_df, headers = headers2)
}

# ---------------------------------------------------------
# 5) Anchor mode B: Already paired columns exist in raw file
#    Raw columns: Imp_1..k and NotImp_1..k (or custom prefixes)
#    These store item IDs already; we just join them to df_expanded.
# ---------------------------------------------------------

#' Combine expanded MaxDiff data with existing anchor pair columns in raw file
#'
#' @param df_raw Raw df with sys_RespNum and anchor pair columns
#' @param df_expanded Expanded MaxDiff df
#' @param headers Headers from maxdiff_reformat_for_choicetools()
#' @param anchor_prefix Pair prefix c("Imp","NotImp") (default)
#' @param num_anchor_items Number of columns per prefix (default 5)
#' @param exportfile If not NULL, write anchored CSV
#' @return list(combined_df, headers)
maxdiff_anchor_from_pair_columns <- function(
  df_raw,
  df_expanded,
  headers,
  anchor_prefix = c("Imp", "NotImp"),
  num_anchor_items = 5,
  exportfile = NULL
) {
  if (length(anchor_prefix) != 2) stop("anchor_prefix must be length-2, e.g. c('Imp','NotImp').")
  if (!is.numeric(num_anchor_items) || length(num_anchor_items) != 1 || num_anchor_items < 1) {
    stop("num_anchor_items must be a positive integer.")
  }
  num_anchor_items <- as.integer(num_anchor_items)

  imp_cols    <- paste0(anchor_prefix[1], "_", seq_len(num_anchor_items))
  notimp_cols <- paste0(anchor_prefix[2], "_", seq_len(num_anchor_items))
  needed <- c(imp_cols, notimp_cols)

  missing <- setdiff(needed, names(df_raw))
  if (length(missing) > 0) {
    stop("Missing anchor pair columns in raw CSV: ", paste(missing, collapse = ", "))
  }

  anchor_df <- df_raw %>% dplyr::select(sys_RespNum, dplyr::all_of(needed))
  combined_df <- dplyr::left_join(df_expanded, anchor_df, by = "sys_RespNum")

  # Extend headers for the joined columns
  new_cols <- setdiff(names(combined_df), headers$first)
  headers2 <- headers
  headers2$first  <- names(combined_df)
  headers2$second <- c(headers$second, paste0("Anchor%-%", new_cols))
  headers2$third  <- c(headers$third,  paste0("{'ImportId':", new_cols, "}"))

  if (!is.null(exportfile)) {
    write_qualtrics_3row_csv(combined_df, headers2, exportfile)
  }

  list(combined_df = combined_df, headers = headers2)
}

# ----------------------------
# 6) One-shot wrapper function
# ----------------------------

#' One-shot pipeline: read Qualtrics numeric MaxDiff, reformat for choiceTools,
#' and optionally add anchor columns (direct-binary OR already-paired).
#'
#' @param filename Raw Qualtrics numeric CSV path
#' @param all_labels Character vector of item labels (master item list)
#' @param num_questions Number of MaxDiff tasks
#' @param num_item_per_question Items shown per task
#' @param maxdiff_flag_col Column indicating MaxDiff presence (default "vers_MAXDIFF")
#' @param id_col Column used to build sys_RespNum (default "ResponseId")
#' @param export_maxdiff If not NULL, writes the non-anchored reformatted CSV here
#' @param export_anchor If not NULL, writes the anchored reformatted CSV here
#'
#' Anchor control:
#' @param anchor_mode One of c("none","pair","direct_binary")
#'   - "none": no anchor step
#'   - "pair": join existing anchor columns (Imp_1..k + NotImp_1..k, or custom prefixes)
#'   - "direct_binary": build Imp_*/NotImp_* from a single raw prefix coded 1/2
#' @param anchor_prefix Pair prefix for output or pair-mode columns, default c("Imp","NotImp")
#' @param num_anchor_items Number of anchor columns per prefix (default 5)
#'
#' Direct-binary only:
#' @param anchor_question_prefix Raw anchor question prefix (e.g., "QID123_")
#' @param anchor_total_item Total items in the raw anchor question
#'
#' @return list(df_raw, df_expanded, headers, df_anchor_combined, headers_anchor)
maxdiff_prepare_all <- function(
  filename,
  all_labels,
  num_questions,
  num_item_per_question,
  maxdiff_flag_col = "vers_MAXDIFF",
  id_col = "ResponseId",
  export_maxdiff = NULL,
  export_anchor = NULL,
  anchor_mode = c("none", "pair", "direct_binary"),
  anchor_prefix = c("Imp", "NotImp"),
  num_anchor_items = 5,
  anchor_question_prefix = NULL,
  anchor_total_item = NULL
) {
  anchor_mode <- match.arg(anchor_mode)

  # 1) Read + filter
  df_raw <- read_qualtrics_numeric_maxdiff(filename, id_col = id_col)
  df_raw <- filter_maxdiff_rows(df_raw, maxdiff_flag_col = maxdiff_flag_col)

  # 2) Reformat MaxDiff
  prep <- maxdiff_reformat_for_choicetools(
    df = df_raw,
    all_labels = all_labels,
    num_questions = num_questions,
    num_item_per_question = num_item_per_question
  )

  df_expanded <- prep$df_expanded
  headers <- prep$headers

  # 3) Write non-anchored file
  if (!is.null(export_maxdiff)) {
    write_qualtrics_3row_csv(df_expanded, headers, export_maxdiff)
  }

  # 4) Optional anchor
  df_anchor_combined <- NULL
  headers_anchor <- NULL

  if (anchor_mode != "none") {
    if (is.null(export_anchor)) {
      stop("anchor_mode is '", anchor_mode, "' but export_anchor is NULL. Provide export_anchor path.")
    }

    if (anchor_mode == "pair") {
      anch <- maxdiff_anchor_from_pair_columns(
        df_raw = df_raw,
        df_expanded = df_expanded,
        headers = headers,
        anchor_prefix = anchor_prefix,
        num_anchor_items = num_anchor_items,
        exportfile = export_anchor
      )
    } else if (anchor_mode == "direct_binary") {
      if (is.null(anchor_question_prefix) || is.null(anchor_total_item)) {
        stop("For anchor_mode='direct_binary', provide anchor_question_prefix and anchor_total_item.")
      }
      anch <- maxdiff_anchor_from_direct_binary(
        df_raw = df_raw,
        df_expanded = df_expanded,
        headers = headers,
        question_prefix = anchor_question_prefix,
        total_item = anchor_total_item,
        num_anchor_item = num_anchor_items,
        out_prefix = anchor_prefix,
        exportfile = export_anchor
      )
    }

    df_anchor_combined <- anch$combined_df
    headers_anchor <- anch$headers
  }

  invisible(list(
    df_raw = df_raw,
    df_expanded = df_expanded,
    headers = headers,
    df_anchor_combined = df_anchor_combined,
    headers_anchor = headers_anchor
  ))
}


