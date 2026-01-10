# maxdiff_prep_single_file.R
# ------------------------------------------------------------
# One-file pipeline to:
# 1) Read Qualtrics numeric MaxDiff export (July 2025 style)
# 2) Reformat to choiceTools wide format (reformatted.csv)
# 3) Optionally add Direct-Binary anchor question (reformatted_anchor.csv)
#
# IMPORTANT: we DO NOT use variable name `df` to avoid collision with stats::df()
#
# Dependencies: readr, dplyr
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# ----------------------------
# Helper: write 3-row header CSV
# ----------------------------
write_qualtrics_3row_csv <- function(df_wide, headers, exportfile) {
  # headers: list(first=..., second=..., third=...)
  hdr <- rbind(headers$first, headers$second, headers$third)
  write.table(hdr, file = exportfile, sep = ",", row.names = FALSE, col.names = FALSE)
  write.table(df_wide, file = exportfile, sep = ",",
              row.names = FALSE, col.names = FALSE, append = TRUE)
  invisible(exportfile)
}

# ----------------------------
# Step 1: read Qualtrics numeric export and filter to MaxDiff rows
# ----------------------------
read_qualtrics_maxdiff_numeric <- function(
  filename,
  skip = 2,
  header_rows = 4,
  maxdiff_flag_col = "vers_MAXDIFF",
  response_id_col = "ResponseId"
) {
  df_raw <- read_csv(filename, skip = skip, show_col_types = FALSE)
  col_names <- read_csv(filename, n_max = header_rows, show_col_types = FALSE)
  colnames(df_raw) <- colnames(col_names)

  if (!(maxdiff_flag_col %in% names(df_raw))) {
    stop("Expected column not found: ", maxdiff_flag_col)
  }
  if (!(response_id_col %in% names(df_raw))) {
    stop("Expected column not found: ", response_id_col)
  }

  df_raw <- df_raw %>% filter(!is.na(.data[[maxdiff_flag_col]]))
  df_raw$sys_RespNum <- seq_along(df_raw[[response_id_col]])
  df_raw
}

# -------------------------------------------
# Step 2: reformat MaxDiff to choiceTools wide format (NO anchor)
# Returns: list(df_expanded, headers, df_raw)
# -------------------------------------------
maxdiff_reformat_no_anchor <- function(
  df_raw,
  all_labels,
  num_question = 6,
  num_item_per_question = 5,
  exportfile = "reformatted.csv",
  choice_prefix = "C",
  label_suffix = "_MAXDIFF"
) {
  # Lookup table label -> code
  lookup <- data.frame(
    label = all_labels,
    code = seq_along(all_labels),
    stringsAsFactors = FALSE
  )

  # Qualtrics 3-row header skeleton
  second.row <- c("sys_RespNum")
  third.row  <- c("{'ImportId': 'sys_RespNum'}")

  # Expanded wide dataframe
  df_expanded <- data.frame(sys_RespNum = df_raw$sys_RespNum)

  # Expand per question
  for (q in seq_len(num_question)) {
    choice_cols <- paste0(choice_prefix, q, "_", seq_len(num_item_per_question))
    label_cols  <- paste0(q, ".", seq_len(num_item_per_question), label_suffix)

    missing_choice <- setdiff(choice_cols, names(df_raw))
    missing_label  <- setdiff(label_cols, names(df_raw))
    if (length(missing_choice) > 0 || length(missing_label) > 0) {
      stop(
        "Missing columns for q=", q,
        " | choice: ", paste(missing_choice, collapse = ", "),
        " | labels: ", paste(missing_label, collapse = ", ")
      )
    }

    for (idx in seq_along(all_labels)) {
      item <- all_labels[idx]
      new_col <- paste0("Q", q, "_", idx)
      df_expanded[[new_col]] <- NA

      second.row <- c(second.row, paste0("(Your MaxDiff Question Body)?%-%", item))
      third.row  <- c(third.row,  paste0("{'ImportId':", new_col, "}"))

      for (i in seq_len(num_item_per_question)) {
        mask <- df_raw[[label_cols[i]]] == item
        df_expanded[[new_col]][mask] <- df_raw[[choice_cols[i]]][mask]
      }
    }
  }

  # Identify which label columns are the MaxDiff items (used for DO / numeric conversion)
  maxdiff_cols <- grep(paste0(label_suffix, "$"), names(df_raw), value = TRUE)

  # Preserve your old behavior: skip first matched, then take exactly Q*items columns
  maxdiff_cols <- maxdiff_cols[2:(num_item_per_question * num_question + 1)]

  # Create numeric versions of MaxDiff item labels
  for (col in maxdiff_cols) {
    new_col <- paste0(col, "_NUM")
    df_raw[[new_col]] <- lookup$code[match(df_raw[[col]], lookup$label)]
  }

  # Create display order columns DO-Q-Q#
  for (q in seq_len(num_question)) {
    cols <- paste0(q, ".", seq_len(num_item_per_question), label_suffix, "_NUM")
    missing_do <- setdiff(cols, names(df_raw))
    if (length(missing_do) > 0) {
      stop("Missing numeric label columns for display order q=", q, ": ",
           paste(missing_do, collapse = ", "))
    }

    df_expanded[[paste0("DO-Q-Q", q)]] <- apply(df_raw[cols], 1, paste, collapse = "|")
    second.row <- c(second.row, "Display Order: (Your MaxDiff Question Body)?")
    third.row  <- c(third.row,  paste0("{'ImportId':", "DO-Q-Q", q, "}"))
  }

  headers <- list(
    first = colnames(df_expanded),
    second = second.row,
    third = third.row
  )

  # Write out file
  write_qualtrics_3row_csv(df_expanded, headers, exportfile)

  invisible(list(
    df_expanded = df_expanded,
    headers = headers,
    df_raw = df_raw
  ))
}

# --------------------------------------------------------
# Step 3: add Direct-Binary anchor columns and join to expanded df
# Anchor raw columns are: paste0(anchor_prefix, 1:total_item)
# Values: 1 = Important, 2 = Not Important
# Output columns appended: Imp_1..num_anchor_item, NotImp_1..num_anchor_item
# --------------------------------------------------------
maxdiff_add_anchor_direct_binary <- function(
  df_raw,
  df_expanded,
  headers,
  anchor_prefix,
  total_item,
  num_anchor_item = 5,
  exportfile = "reformatted_anchor.csv",
  out_prefix = c("Imp", "NotImp")
) {
  if (length(out_prefix) != 2) stop("out_prefix must be c('Imp','NotImp')")

  anchor_cols <- paste0(anchor_prefix, seq_len(total_item))
  missing <- setdiff(anchor_cols, names(df_raw))
  if (length(missing) > 0) {
    stop("Missing anchor columns: ", paste(missing, collapse = ", "))
  }

  df_anchor <- df_raw %>%
    select(sys_RespNum, all_of(anchor_cols))

  # Build reformatted anchor dataframe
  df_anchor_reformatted <- data.frame(sys_RespNum = df_anchor$sys_RespNum)
  for (k in seq_len(num_anchor_item)) {
    df_anchor_reformatted[[paste0(out_prefix[1], "_", k)]] <- NA_integer_
    df_anchor_reformatted[[paste0(out_prefix[2], "_", k)]] <- NA_integer_
  }

  for (r in seq_len(nrow(df_anchor))) {
    row_vals <- suppressWarnings(as.numeric(df_anchor[r, anchor_cols, drop = TRUE]))
    imp_items <- which(row_vals == 1)
    not_items <- which(row_vals == 2)

    if (length(imp_items) > 0) {
      n <- min(length(imp_items), num_anchor_item)
      df_anchor_reformatted[r, paste0(out_prefix[1], "_", seq_len(n))] <- imp_items[seq_len(n)]
    }
    if (length(not_items) > 0) {
      n <- min(length(not_items), num_anchor_item)
      df_anchor_reformatted[r, paste0(out_prefix[2], "_", seq_len(n))] <- not_items[seq_len(n)]
    }
  }

  # Reorder anchor columns: sys_RespNum, Imp_*, NotImp_*
  imp_cols <- grep(paste0("^", out_prefix[1], "_"), names(df_anchor_reformatted), value = TRUE)
  not_cols <- grep(paste0("^", out_prefix[2], "_"), names(df_anchor_reformatted), value = TRUE)
  df_anchor_reformatted <- df_anchor_reformatted[, c("sys_RespNum", imp_cols, not_cols), drop = FALSE]

  combined_df <- df_expanded %>%
    left_join(df_anchor_reformatted, by = "sys_RespNum")

  # Extend headers
  new_cols <- setdiff(names(combined_df), headers$first)
  headers2 <- headers
  headers2$first <- names(combined_df)
  headers2$second <- c(headers$second, paste0("Anchor%-%", new_cols))
  headers2$third  <- c(headers$third,  paste0("{'ImportId':", new_cols, "}"))

  write_qualtrics_3row_csv(combined_df, headers2, exportfile)

  invisible(list(
    combined_df = combined_df,
    headers = headers2,
    df_anchor_reformatted = df_anchor_reformatted
  ))
}

# ----------------------------
# One-shot wrapper
# Provide raw file, parameters, and optional anchor prefix
# ----------------------------
maxdiff_prepare_files <- function(
  raw_filename,
  all_labels,
  num_question = 6,
  num_item_per_question = 5,
  # read params
  skip = 2,
  header_rows = 4,
  maxdiff_flag_col = "vers_MAXDIFF",
  response_id_col = "ResponseId",
  # outputs
  export_maxdiff = "reformatted.csv",
  export_anchor = NULL,
  # anchor params (direct-binary)
  anchor_prefix = NULL,
  anchor_total_item = NULL,
  num_anchor_item = 5,
  out_prefix = c("Imp", "NotImp")
) {
  # 1) read + filter
  df_raw <- read_qualtrics_maxdiff_numeric(
    filename = raw_filename,
    skip = skip,
    header_rows = header_rows,
    maxdiff_flag_col = maxdiff_flag_col,
    response_id_col = response_id_col
  )

  # 2) reformat (no anchor)
  prep <- maxdiff_reformat_no_anchor(
    df_raw = df_raw,
    all_labels = all_labels,
    num_question = num_question,
    num_item_per_question = num_item_per_question,
    exportfile = export_maxdiff
  )

  # 3) optional anchor
  anch <- NULL
  if (!is.null(export_anchor)) {
    if (is.null(anchor_prefix) || is.null(anchor_total_item)) {
      stop("To write export_anchor, provide anchor_prefix and anchor_total_item.")
    }
    anch <- maxdiff_add_anchor_direct_binary(
      df_raw = df_raw,
      df_expanded = prep$df_expanded,
      headers = prep$headers,
      anchor_prefix = anchor_prefix,
      total_item = anchor_total_item,
      num_anchor_item = num_anchor_item,
      exportfile = export_anchor,
      out_prefix = out_prefix
    )
  }

  invisible(list(
    df_raw = df_raw,
    df_expanded = prep$df_expanded,
    headers = prep$headers,
    reformatted_file = export_maxdiff,
    anchor = anch,
    anchor_file = export_anchor
  ))
}

# ----------------------------
# Example usage (copy/paste)
# ----------------------------
# labels <- c("Dog","Cat","Fish","Bird","Hamster","Rabbit","Lizard","Turtle","Guinea pig","Horse")
#
# # MaxDiff only:
# out <- maxdiff_prepare_files(
#   raw_filename = "Demo_Pet.csv",
#   all_labels = labels,
#   num_question = 6,
#   num_item_per_question = 5,
#   export_maxdiff = "reformatted.csv"
# )
#
# # MaxDiff + Direct-Binary anchor:
# out2 <- maxdiff_prepare_files(
#   raw_filename = "Demo_Pet.csv",
#   all_labels = labels,
#   num_question = 6,
#   num_item_per_question = 5,
#   export_maxdiff = "reformatted.csv",
#   export_anchor = "reformatted_anchor.csv",
#   anchor_prefix = "Anchor_",      # raw columns Anchor_1..Anchor_10
#   anchor_total_item = 10,
#   num_anchor_item = 5
# )




