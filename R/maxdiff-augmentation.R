# Copyright 2019 Google LLC.
# SPDX-License-Identifier: Apache-2.0

# MaxDiff choice augmentation

#############################################################
#############################################################
#
#  md.augment(md.define, method)
#
#  augments the "md.block" object with inferred preferences for items that were
#  rated before selection into the MaxDiff.
#
#  md.define : the study object
#  method    : "threshold" (default) for augmenting via threshold method (see Chapman & Bahna, forthcoming)
#               .. adds "important" > threshold, and "unimportant" < threshold, not full expansion
#              "grid" to add all two-way full expansion for "important items" %*% "unimportant"


md.augment <- function(md.define, method="threshold") {

  # default to new threshold method (as of v0.60), with option for older "grid" expansion
  if (method=="grid") {
    return(md.augment.grid(md.define))

  } else {
    md.block <- md.define$md.block         # copy of the data so we can munge it and return

    md.block$threshold <- 0               # add or clear column to code threshold as "not shown"

    # set up blocks for basic CHO data before augmenting choice sets
    md.block$chid          <- ceiling(1:nrow(md.block)/md.define$md.item.pertask)
    md.block$choice.coded  <- md.block$win

    if (!md.define$md.adapt) {
      cat("Warning: Not augmenting. md.define is not set for augmentation.\n")

    } else {
      # load full CSV data
      cat("Reading full data set to get augmentation variables.\n\n")
      full.data <- read.csv(paste0(md.define$file.wd, md.define$file.all))

      cat("Importants:", md.define$md.adapt.Imp,"\n")
      print(names(full.data)[md.define$md.adapt.Imp])
      cat("Unimportants:", md.define$md.adapt.NotImp,"\n")
      print(names(full.data)[md.define$md.adapt.NotImp])
    }

    nrow.preadapt <- nrow(md.block)
    if (md.define$md.adapt) {

      cat("\nAugmenting choices per 'threshold augementation' method. \nRows before augementation:", nrow.preadapt, "\n")

      ## TO DO: some data quality tests and error recovery for full.data

      # set states for preallocation and placeholder
      block.new  <- TRUE

      # loop over all respondents and add data ...
      #
      chid <- max(md.block$chid)+1                          # counter for choice blocks as we add them
      md.supp                    <- md.block[1:4, ]   # a block we'll reuse for all the Imp x NotImp choices below

      # for all respondents ...
      for (i in unique(md.block$resp.id)) {
        i.data   <- full.data[full.data$sys_RespNum==i, ]
        itemsImp <- i.data[md.define$md.adapt.Imp]            # remove magic numbers
        itemsImp <- na.omit(as.numeric(itemsImp))
        itemsNotImp <- i.data[md.define$md.adapt.NotImp]       # remove magic numbers
        itemsNotImp <- na.omit(as.numeric(itemsNotImp))

        # first augment the Important items vs. threshold, if any
        if (length(itemsImp) > 0) {

          cat("\nAugmenting important items for respondent", i, ":", itemsImp)
          for (imp in itemsImp) {
            # set up an empty block of A vs. B choices to hold our augmented comparison
            # structure is: Rows 1/2 == IMP vs. threshold, IMP       == Best
            #               Rows 3/4 == IMP vs. threshold, threshold == Worst
            md.supp[ , 3:(md.define$md.item.k+2)] <- 0    # clear design matrix
            md.supp$win                <- 0    # " "
            # fill in our metadata
            md.supp$resp.id            <- i
            # set the best choices (item wins vs. threshold)
            md.supp[1, 2+imp]          <- 1
            md.supp[2, "threshold"]    <- 1
            md.supp[1, "win"]          <- 1
            md.supp[1:2, "chid"]       <- chid
            
            # set the worst choices (threshold wins as the "worst")
            md.supp[3, 2+imp]          <- -1
            md.supp[4, "threshold"]    <- -1
            md.supp[4, "win"]          <- 1
            md.supp[3:4, "chid"]       <- chid + 1
            

            # add the new choice set to the master choice data
            # first, need to preallocate block ?
            if (block.new) {                             # preallocate block
              # max comparisons would be md.define$md.item.k as either important or unimportant
              # so allocate that many for each respondent * 4 comparison rows
              nrow.prealloc      <- (md.define$md.item.k) * length(unique(md.block$resp.id)) * 4
              md.block.new       <- md.block[rep(1, nrow.prealloc), ]
              md.block.new[ , ]  <- 0                    # zero out the preallocated matrix
              block.line         <- 1                    # counter for which line we're on as we fill it
              block.new          <- FALSE                # don't preallocate again
            }

            # put the new data into the preallocated block
            md.block.new[block.line:(block.line+nrow(md.supp)-1), ] <- md.supp
            block.line                                              <- block.line + nrow(md.supp)

            # advance counter of choice blocks
            chid                       <- chid + 2
          }
        }


        ## Second, augment the unimportant items, if any
        if (length(itemsNotImp) > 0) {

          cat("\nAugmenting unimportant items for respondent", i, ":", itemsNotImp)
          for (notImp in itemsNotImp) {
            # set up an empty block of A vs. B choices to hold our augmented comparison
            # structure is: Rows 1/2 == notIMP vs. threshold, threshold  == Best
            #               Rows 3/4 == notIMP vs. threshold, item       == Worst
            md.supp[ , 3:(md.define$md.item.k+2)] <- 0    # clear design matrix
            md.supp$win                <- 0    # " "
            # fill in our metadata
            md.supp$resp.id            <- i
            # set the best choices (threshold wins vs item as the "best")
            md.supp[1, 2+notImp]          <- 1
            md.supp[2, "threshold"]    <- 1
            md.supp[2, "win"]          <- 1
            md.supp[1:2, "chid"]       <- chid
            
            # set the worst choices (item wins as the "worst")
            md.supp[3, 2+notImp]       <- -1
            md.supp[4, "threshold"]    <- -1
            md.supp[3, "win"]          <- 1
            md.supp[3:4, "chid"]       <- chid + 1
            

            # add the new choice set to the master choice data
            # first, need to preallocate block ?
            if (block.new) {                             # preallocate block
              # max comparisons would be md.define$md.item.k as either important or unimportant
              # so allocate that many for each respondent * 4 comparison rows
              nrow.prealloc      <- (md.define$md.item.k) * length(unique(md.block$resp.id)) * 4
              md.block.new       <- md.block[rep(1, nrow.prealloc), ]
              md.block.new[ , ]  <- 0                    # zero out the preallocated matrix
              block.line         <- 1                    # counter for which line we're on as we fill it
              block.new          <- FALSE                # don't preallocate again
            }

            # put the new data into the preallocated block
            md.block.new[block.line:(block.line+nrow(md.supp)-1), ] <- md.supp
            block.line                                              <- block.line + nrow(md.supp)

            # advance counter of choice blocks
            chid                       <- chid + 2
          }
        }
      }  # end FOR respondents
    }

    # did we add anything?
    if (block.line > 1) {
      # keep only the preallocated rows we actually used
      md.block.new <- md.block.new[1:(block.line-1), ]
      # md.block.new <- md.block.new[md.block.new$resp.id > 0, ]
      md.block <- rbind(md.block, md.block.new)
    }

    cat ("\n ... done!\n\n")
    cat("Rows after threshold augmentation: ", nrow(md.block), "\n")
  }

  # now cast the new blocks into conditional format
  md.block$choice.coded                         <- md.block$win
  md.block$choice.coded[which(md.block$win==1)] <- 'yes'   # recode 1's into yes
  md.block$choice.coded[which(md.block$win==0)] <- 'no'    # recode 0's into no
  md.block$choice.coded                         <- as.factor(md.block$choice.coded)
  # table(md.block$win, md.block$choice.coded)

  return(list(md.block=md.block, md.nrow.preadapt=nrow.preadapt, md.csvdata=full.data))
}


#############################################################
#############################################################
#
#  md.augment.grid(md.define)
#
#  called from md.augment() to implement the full-grid augmentation option
#

md.augment.grid <- function(md.define) {

  md.block <- md.define$md.block         # copy of the data so we can munge it and return

  # set up blocks for basic CHO data before augmenting choice sets
  md.block$chid          <- ceiling(1:nrow(md.block)/md.define$md.item.pertask)
  md.block$choice.coded  <- md.block$win

  if (!md.define$md.adapt) {
    cat("Warning: Not augmenting. md.define is not set for augmentation.\n")

  } else {
    # load full CSV data
    cat("Reading full data set to get augmentation variables.\n\n")
    full.data <- read.csv(paste0(md.define$file.wd, md.define$file.all))

    cat("Importants:", md.define$md.adapt.Imp,"\n")
    print(names(full.data)[md.define$md.adapt.Imp])
    cat("Unimportants:", md.define$md.adapt.NotImp,"\n")
    print(names(full.data)[md.define$md.adapt.NotImp])
  }

  nrow.preadapt <- nrow(md.block)
  if (md.define$md.adapt) {

    cat("\nAugmenting choices per 'adaptive grid', full expansion method. \nRows before adding:", nrow.preadapt, "\n")

    ## TO DO: some data quality tests and error recovery for full.data

    # set states for preallocation and placeholder
    block.new  <- TRUE

    # loop over all respondents and add data ...
    #
    chid <- max(md.block$chid)+1                          # counter for choice blocks as we add them
    cat ("\nAugmenting adaptive data for respondent:\n")

    md.supp                    <- md.block[1:4, ]   # a block we'll reuse for all the Imp x NotImp choices below

    for (i in unique(md.block$resp.id)) {
      i.data   <- full.data[full.data$sys_RespNum==i, ]
      itemsImp <- i.data[md.define$md.adapt.Imp]            # remove magic numbers
      itemsImp <- na.omit(as.numeric(itemsImp))
      itemsNotImp <- i.data[md.define$md.adapt.NotImp]       # remove magic numbers
      itemsNotImp <- na.omit(as.numeric(itemsNotImp))

      if (length(itemsImp) > 0 & length(itemsNotImp) > 0) {
        cat (i, " ")
        cat("augmenting:", itemsImp, "%*% ")
        cat(itemsNotImp, "\n")
        for (imp in itemsImp) {
          for (notimp in itemsNotImp) {
            # set up an empty block of A vs. B choices to hold our augmented comparison
            # structure is: Rows 1/2 == IMP vs. notIMP shown, IMP    == Best
            #               Rows 3/4 == IMP vs. notIMP shown, notIMP == Worst
            md.supp[ , 3:(md.define$md.item.k+2)] <- 0    # clear design matrix
            md.supp$win                <- 0    # " "
            # fill in our metadata
            md.supp$resp.id            <- i
            # set the best choices
            md.supp[1, 2+imp]          <- 1
            md.supp[2, 2+notimp]       <- 1
            md.supp[1, "win"]          <- 1
            md.supp[1:2, "chid"]       <- chid
            # set the worst choices
            md.supp[3, 2+imp]          <- -1
            md.supp[4, 2+notimp]       <- -1
            md.supp[4, "win"]          <- 1
            md.supp[3:4, "chid"]       <- chid + 1

            # add the new choice set to the master choice data
            # first, need to preallocate block ?
            if (block.new) {                             # preallocate block
              # max comparisons would be md.define$md.item.k/2 * (md.define$md.item.k-1)/2
              # so allocate that many (+1 for odd cases) for each respondent * 4 comparison rows
              nrow.prealloc      <- (md.define$md.item.k+1) %/% 2 * md.define$md.item.k %/% 2 * length(unique(md.block$resp.id)) * 4
              md.block.new       <- md.block[rep(1, nrow.prealloc), ]
              md.block.new[ , ]  <- 0                    # zero out the preallocated matrix
              block.line         <- 1                    # counter for which line we're on as we fill it
              block.new          <- FALSE                # don't preallocate again :)
            }

            # put the new data into the preallocated block
            md.block.new[block.line:(block.line+nrow(md.supp)-1), ] <- md.supp
            block.line                                              <- block.line + nrow(md.supp)

            # advance counter of choice blocks
            chid                       <- chid + 2
          }
        }
      }
    }
    # keep only the preallocated rows we actually used
    md.block.new <- md.block.new[1:(block.line-1), ]
    # md.block.new <- md.block.new[md.block.new$resp.id > 0, ]

    md.block <- rbind(md.block, md.block.new)
    cat ("done!\n\n")
    cat("Rows after augmenting data:", nrow(md.block), "\n")
  }

  # now cast the new blocks into conditional format
  md.block$choice.coded                         <- md.block$win
  md.block$choice.coded[which(md.block$win==1)] <- 'yes'   # recode 1's into yes
  md.block$choice.coded[which(md.block$win==0)] <- 'no'    # recode 0's into no
  md.block$choice.coded                         <- as.factor(md.block$choice.coded)
  # table(md.block$win, md.block$choice.coded)

  return(list(md.block=md.block, md.nrow.preadapt=nrow.preadapt, md.csvdata=full.data))

}
#' Convenience wrapper: read Qualtrics MaxDiff CSV, set adapt columns, augment, and return md.define
#'
#' @param filename Path to Qualtrics-exported CSV
#' @param Imp Integer vector of column indices containing the "Important" item IDs
#' @param NotImp Integer vector of column indices containing the "Not Important" item IDs
#' @param method "threshold" (default) or "grid" (passed to md.augment)
#' @param add_set_labels Whether to label augmented rows' Set as Best/Worst (default TRUE)
#' @param reorder_threshold If TRUE, move threshold column next to item columns for readability
#' @return md.define (study object) with md.block updated and (for threshold) threshold item appended
maxdiff_augment <- function(
  filename = NULL,
  data = NULL,
  headers = NULL,
  Imp = NULL,
  NotImp = NULL,
  Imp_prefix = "Imp_",
  NotImp_prefix = "NotImp_",
  num_anchor_items = 5,
  codeMDneg = 2,
  codeMDpos = 1,
  add_set_labels = TRUE,
  set_labels = c("Best","Worst"),
  reorder_threshold = TRUE,
  ...
) {
  dots <- list(...)
  if (is.null(headers) && !is.null(dots$headers)) {
    headers <- dots$headers
  }

  unknown_dots <- setdiff(names(dots), c("headers", ""))
  if (length(unknown_dots) > 0) {
    warning(
      "Ignoring unsupported argument(s): ",
      paste(unique(unknown_dots), collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(filename) && is.null(data)) {
    stop("Provide either `filename` or `data`.")
  }
  if (!is.null(filename) && !is.null(data)) {
    stop("Provide only one of `filename` or `data`, not both.")
  }

  if (!is.null(data)) {
    if (!is.data.frame(data)) stop("`data` must be a data.frame.")

    has_display_order_cols <- function(cols) {
      any(grepl("^DO[-.]Q[-.]Q", cols))
    }

    build_headers_from_data <- function(dat) {
      cols <- names(dat)
      list(
        first = cols,
        second = vapply(cols, function(col) {
          if (grepl("^DO-Q-Q", col)) return("Display Order: Imported MaxDiff Question")
          if (grepl("^(Imp|NotImp)_", col)) return(paste0("Anchor%-%", col))
          if (grepl("^Q[0-9]+_[0-9]+$", col)) return(paste0("Imported MaxDiff Question%-%", col))
          col
        }, character(1)),
        third = paste0("{'ImportId':", cols, "}")
      )
    }

    hdr <- headers
    if (is.null(hdr)) {
      hdr <- build_headers_from_data(data)
    }
    if (!all(c("first", "second", "third") %in% names(hdr))) {
      stop("`headers` must be a list with elements: first, second, third.")
    }

    # normalize/repair supplied headers to parser-safe vectors
    cols <- names(data)
    if (length(hdr$first) != length(cols)) hdr$first <- cols
    if (length(hdr$second) != length(cols)) hdr$second <- cols
    if (length(hdr$third) != length(cols)) hdr$third <- paste0("{'ImportId':", cols, "}")

    # Ensure display-order text exists for DO columns even if caller-provided
    do_idx <- grepl("^DO[-.]Q[-.]Q", cols)
    if (any(do_idx)) {
      hdr$second[do_idx] <- "Display Order: Imported MaxDiff Question"
    }

    # If no DO columns are present in data, fail early with actionable error
    if (!has_display_order_cols(cols)) {
      stop("`data` does not include any display-order columns (expected names like DO-Q-Q1).")
    }

    tmp_file <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp_file), add = TRUE)
    hdr_mat <- rbind(hdr$first, hdr$second, hdr$third)
    write.table(hdr_mat, file = tmp_file, sep = ",", row.names = FALSE, col.names = FALSE)
    write.table(data, file = tmp_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    input_file <- tmp_file
  } else {
    input_file <- filename
  }

  # ---- read + parse ----
  md.define <- parse.md.qualtrics(input_file, returnList = TRUE)
  md.define$q.codeMDneg <- codeMDneg
  md.define$q.codeMDpos <- codeMDpos
  md.define$md.block <- read.md.qualtrics(md.define)$md.block

  md.block <- md.define$md.block
  nrow.preadapt <- nrow(md.block)

  # ---- load full CSV (keep original names) ----
  full.data <- read.csv(input_file, check.names = FALSE)

  if (!("sys_RespNum" %in% names(full.data))) {
    stop("Expected column 'sys_RespNum' not found in CSV.")
  }

  # ---- helper: allow Imp/NotImp to be indices OR names ----
make_anchor_names <- function(prefix, n) {
  if (is.null(prefix) || is.null(n)) return(NULL)
  if (!is.character(prefix) || length(prefix) != 1) stop("prefix must be a single string.")
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) stop("num_anchor_items must be a positive integer.")
  n <- as.integer(n)

  # If user passes "Imp" instead of "Imp_", add underscore automatically
  sep <- if (grepl("_$", prefix)) "" else "_"
  paste0(prefix, sep, seq_len(n))
}

resolve_cols <- function(df, cols, label, prefix = NULL, n = NULL) {
  # If cols not provided, generate from prefix+n
  if (is.null(cols)) {
    cols <- make_anchor_names(prefix, n)
    if (is.null(cols)) {
      stop(label, ": provide either explicit cols OR (prefix and num_anchor_items).")
    }
  }

  # Numeric indices
  if (is.numeric(cols)) {
    cols <- as.integer(cols)
    if (any(cols < 1 | cols > ncol(df))) {
      stop(label, " contains out-of-range column indices. ncol(csv) = ", ncol(df))
    }
    return(cols)
  }

  # Explicit column names
  if (is.character(cols)) {
    missing <- setdiff(cols, names(df))
    if (length(missing) > 0) {
      stop(label, " contains unknown column names: ", paste(missing, collapse = ", "))
    }
    return(match(cols, names(df)))
  }

  stop(label, " must be numeric indices, character column names, or NULL (to use prefix + num_anchor_items).")
}

  # Resolve Imp/NotImp columns
  Imp_cols <- resolve_cols(full.data, Imp, "Imp",
                          prefix = Imp_prefix, n = num_anchor_items)
  NotImp_cols <- resolve_cols(full.data, NotImp, "NotImp",
                             prefix = NotImp_prefix, n = num_anchor_items)

  # ---- infer item columns from md.block (don’t depend on md.item.k) ----
  non_item <- c("resp.id","win","chid","choice.coded","Block","Set","sys.resp")
  candidate <- setdiff(names(md.block), non_item)
  candidate <- candidate[sapply(md.block[candidate], is.numeric)]

  is_item_like <- function(x) {
    u <- unique(x[!is.na(x)])
    length(u) > 0 && all(u %in% c(-1, 0, 1))
  }
  item_cols <- candidate[sapply(md.block[candidate], is_item_like)]

  if (length(item_cols) == 0) {
    stop("Could not infer item columns from md.block. Check your parsed md.block structure.")
  }

  # Ensure required metadata exists
  if (is.null(md.define$md.item.pertask)) {
    stop("md.define$md.item.pertask is NULL. parse/read did not populate task size.")
  }

  # ---- prepare md.block baseline ----
  md.block$threshold <- 0
  md.block$chid <- ceiling(seq_len(nrow(md.block)) / md.define$md.item.pertask)

  # Template reused for each inferred pair (4 rows)
  md.supp <- md.block[1:4, ]
  md.supp[, item_cols] <- 0
  md.supp$win <- 0
  md.supp$threshold <- 0

  # ---- build augmented rows efficiently ----
  blocks <- list()
  k <- 0
  chid <- max(md.block$chid, na.rm = TRUE) + 1

  for (rid in unique(md.block$resp.id)) {
    i.data <- full.data[full.data$sys_RespNum == rid, , drop = FALSE]
    if (nrow(i.data) == 0) next
    if (nrow(i.data) > 1) i.data <- i.data[1, , drop = FALSE]  # deterministic

    itemsImp <- na.omit(as.numeric(unlist(i.data[Imp_cols])))
    itemsNot <- na.omit(as.numeric(unlist(i.data[NotImp_cols])))

    # Important items: item > threshold
    for (imp in itemsImp) {
      s <- md.supp
      s[, item_cols] <- 0
      s$win <- 0
      s$resp.id <- rid

      # Best: item wins
      s[1, 2 + imp] <- 1
      s[2, "threshold"] <- 1
      s[1, "win"] <- 1
      s[1:2, "chid"] <- chid
      if (add_set_labels && "Set" %in% names(s)) s[1:2, "Set"] <- set_labels[1]

      # Worst: threshold wins
      s[3, 2 + imp] <- -1
      s[4, "threshold"] <- -1
      s[4, "win"] <- 1
      s[3:4, "chid"] <- chid + 1
      if (add_set_labels && "Set" %in% names(s)) s[3:4, "Set"] <- set_labels[2]

      k <- k + 1
      blocks[[k]] <- s
      chid <- chid + 2
    }

    # Not important items: threshold > item
    for (notImp in itemsNot) {
      s <- md.supp
      s[, item_cols] <- 0
      s$win <- 0
      s$resp.id <- rid

      # Best: threshold wins
      s[1, 2 + notImp] <- 1
      s[2, "threshold"] <- 1
      s[2, "win"] <- 1
      s[1:2, "chid"] <- chid
      if (add_set_labels && "Set" %in% names(s)) s[1:2, "Set"] <- set_labels[1]

      # Worst: item wins
      s[3, 2 + notImp] <- -1
      s[4, "threshold"] <- -1
      s[3, "win"] <- 1
      s[3:4, "chid"] <- chid + 1
      if (add_set_labels && "Set" %in% names(s)) s[3:4, "Set"] <- set_labels[2]

      k <- k + 1
      blocks[[k]] <- s
      chid <- chid + 2
    }
  }

  if (length(blocks) > 0) {
    md.block.new <- do.call(rbind, blocks)
    md.block <- rbind(md.block, md.block.new)
  }

  # ---- recode choice.coded (match old behavior exactly) ----
  md.block$choice.coded <- md.block$win
  md.block$choice.coded[md.block$win == 1] <- "yes"
  md.block$choice.coded[md.block$win == 0] <- "no"
  md.block$choice.coded <- factor(md.block$choice.coded, levels = c("no","yes"))

  # ---- update md.define ----
  md.define$md.block <- md.block
  md.define$md.nrow.preadapt <- nrow.preadapt
  md.define$md.csvdata <- full.data

  # Set md.item.k / md.item.names if missing (and append threshold once)
  if (is.null(md.define$md.item.names)) md.define$md.item.names <- item_cols
  if (is.null(md.define$md.item.k)) md.define$md.item.k <- length(md.define$md.item.names)

  if (!("threshold" %in% md.define$md.item.names)) {
    md.define$md.item.names <- c(md.define$md.item.names, "threshold")
    md.define$md.item.k <- md.define$md.item.k + 1
  }

  # Reorder like your old function (keeps modeling assumptions safe)
  if (reorder_threshold) {
    reorder_cols <- c("threshold", "Block", "Set", "sys.resp", "choice.coded", "chid")
    md.define$md.block <- md.define$md.block[
      , c(setdiff(names(md.define$md.block), reorder_cols), reorder_cols),
      drop = FALSE
    ]
  }

  return(md.define)
}
