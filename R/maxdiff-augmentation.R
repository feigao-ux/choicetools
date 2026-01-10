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


md.augment <- function(md.define, method="threshold", add_set_labels=FALSE, set_labels=c("Best","Worst")) {

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
            if (add_set_labels && "Set" %in% names(md.supp)) md.supp[1:2, "Set"] <- set_labels[1]
            # set the worst choices (threshold wins as the "worst")
            md.supp[3, 2+imp]          <- -1
            md.supp[4, "threshold"]    <- -1
            md.supp[4, "win"]          <- 1
            md.supp[3:4, "chid"]       <- chid + 1
            if (add_set_labels && "Set" %in% names(md.supp)) md.supp[3:4, "Set"] <- set_labels[2]

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
            if (add_set_labels && "Set" %in% names(md.supp)) md.supp[1:2, "Set"] <- set_labels[1]
            # set the worst choices (item wins as the "worst")
            md.supp[3, 2+notImp]       <- -1
            md.supp[4, "threshold"]    <- -1
            md.supp[3, "win"]          <- 1
            md.supp[3:4, "chid"]       <- chid + 1
            if (add_set_labels && "Set" %in% names(md.supp)) md.supp[3:4, "Set"] <- set_labels[2]

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
  filename,
  Imp,
  NotImp,
  method = "threshold",
  add_set_labels = TRUE,
  reorder_threshold = TRUE
) {
  # Parse + read Qualtrics MaxDiff structure
  md.define <- parse.md.qualtrics(filename, returnList = TRUE)  # :contentReference[oaicite:3]{index=3}
  md.define$q.codeMDneg <- 2
  md.define$q.codeMDpos <- 1
  md.define$md.block <- read.md.qualtrics(md.define)$md.block

  # Ensure md.item.k / md.item.names exist (some Qualtrics exports don't populate them)
  if (is.null(md.define$md.item.k) || is.null(md.define$md.item.names)) {
    b <- md.define$md.block
    non_item <- c("resp.id","win","chid","choice.coded","Block","Set","sys.resp")
    candidates <- setdiff(names(b), non_item)
    num_cols <- candidates[sapply(b[candidates], is.numeric)]
    is_item_like <- function(x) {
      u <- unique(x[!is.na(x)])
      all(u %in% c(-1, 0, 1))
    }
    item_cols <- num_cols[sapply(b[num_cols], is_item_like)]
    md.define$md.item.names <- item_cols
    md.define$md.item.k <- length(item_cols)
  }


  # Turn on augmentation and provide column locations for the anchor outputs
  md.define$md.adapt <- TRUE
  md.define$md.adapt.Imp <- Imp
  md.define$md.adapt.NotImp <- NotImp

  # Ensure md.augment can find the CSV (it uses file.wd + file.all in the original code) :contentReference[oaicite:4]{index=4}
  # If parse.md.qualtrics already populated these, keep them; otherwise set them from filename.
  if (is.null(md.define$file.wd) || is.null(md.define$file.all)) {
    # Basic split into directory + basename
    md.define$file.wd  <- paste0(dirname(filename), "/")
    md.define$file.all <- basename(filename)
  }

  # Run augmentation
  aug <- md.augment(md.define, method = method, add_set_labels = add_set_labels)

  # Stitch results back into md.define (your demo function returned md.define, not a list)
  md.define$md.block <- aug$md.block
  md.define$md.nrow.preadapt <- aug$md.nrow.preadapt
  md.define$md.csvdata <- aug$md.csvdata

  # If threshold method, add the synthetic threshold "item" to the study definition
  # (grid method doesn't create a threshold column)
  if (method == "threshold") {
    if (!("threshold" %in% md.define$md.item.names)) {
      md.define$md.item.names <- c(md.define$md.item.names, "threshold")
      md.define$md.item.k <- md.define$md.item.k + 1
    }
  
    if (reorder_threshold && "threshold" %in% names(md.define$md.block)) {
      reorder_cols <- c("threshold", "Block", "Set", "sys.resp", "choice.coded", "chid")
      md.define$md.block <- md.define$md.block[
        , c(setdiff(names(md.define$md.block), reorder_cols), reorder_cols),
        drop = FALSE
      ]
    }
  }
  return(md.define)
}
