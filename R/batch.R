#' Batch import and align
#'
#' Calls align2 (and thus various other functions) in order to import and align a series of.xyz files. Files currently must be 6 columns, though the final 3 columns are not used (this will be changed at a later date).
#' @param align Logical. If FALSE, the raw mandible models will be imported to the global environment but not aligned. Setting this to FALSE automatically sets "slice" to FALSE.
#' @param slice Logical. If TRUE, the aligned point cloud will be "sliced" to isolate the mandibular angles, posterior rami, and mental eminence, which will all be plotted in a subfolder and saved as separate .xlsx files.
#' @param folder The save folder. If not specified, the user can interactively choose a folder.
#' @export
#' @examples batch()

batch <- function(align = TRUE, slice = TRUE, folder = choose.dir(caption = "Select Save Folder")){
  require(stringr)

  if (align == FALSE){
    slice <- FALSE
  }
# Selecting the files
  file.filter <- matrix(c("xyz files", "*.xyz"), ncol = 2, byrow = TRUE)
  filelist <- choose.files(caption = "Select the files to import", filters = file.filter)
  nfiles <- length(filelist)
  startTime <- Sys.time()
  startmsg <- paste("WARNING: This could take a while. There are", nfiles, "files to process.", sep = " ")
  message(startmsg)

# Empty objects
  errorlist <- c()
  vertenv <- new.env()

# Filters for file import
  vertc <- c(rep(NA, 3), rep("NULL", 3))

# Importing files
  pb <- winProgressBar(title = "Importing...", max = nfiles)
  time1 <- Sys.time()
  for (i in 1:nfiles){
    pblab <- paste("Importing ", i, " of ", nfiles, ".", sep = "")
    setWinProgressBar(pb, i, label = pblab)
    noext <- sapply(strsplit(basename(filelist[i]), "\\."), function(x) paste(x[1:(length(x)-1)]))
    objname <- paste("m", gsub("[^[:alnum:]]", "", noext), sep = "")
    vertname <- paste(objname, "VERT", sep = "")

# Files are only imported if they have 6 columns.
    if ((ncol(read.table(filelist[i]))) == 6){

# For files with no header
      if (class(read.table(filelist[i], header = FALSE)[1,1]) == "numeric"){
        assign(objname, read.table(filelist[i], header = FALSE, col.names = c("x", "y", "z", "xn", "yn", "zn")), envir = .GlobalEnv)

# If models are to be aligned
        if (align == TRUE){
          assign(vertname, read.table(filelist[i], header = FALSE, colClasses = vertc, col.names = c("x", "y", "z", "xn", "yn", "zn")), envir = vertenv)
        }
      }

# For files with a header
      else {
        assign(objname, read.table(filelist[i], header = TRUE, col.names = c("x", "y", "z", "xn", "yn", "zn")), envir = .GlobalEnv)

# If models are to be aligned
        if (align == TRUE){
          assign(vertname, read.table(filelist[i], header = TRUE, colClasses = vertc, col.names = c("x", "y", "z", "xn", "yn", "zn")), envir = vertenv)
        }
      }
    }

# If it doesn't have 6 columns, it won't be read.
    else {
      error <- basename(filelist[i])
      errorlist <- c(errorlist, error)
    }
  time2 <- Sys.time()
  }

  close(pb)
  message("File import complete.")

# Printing the error list
  if (length(errorlist) != 0){
    print(data.frame("The following files were not imported:" = errorlist))
  }

  if (align == TRUE){

    savedir <- folder
    impfiles <- ls(vertenv)
    resFrame <- data.frame("saved.as" = numeric(length(impfiles)), "runtime" = numeric(length(impfiles)), "loops" = numeric(length(impfiles)), "x.diff" = numeric(length(impfiles)), "y.diff" = numeric(length(impfiles)), "z.diff" = numeric(length(impfiles)))
    errList <- data.frame()

    for (i in 1:length(impfiles)){
      obj <- get(impfiles[i], envir = vertenv)
      fn <- impfiles[i]
      shortname <- str_replace(fn, "VERT", "")
      tryCatch({
        align2(sample = obj, filename = fn, folder = savedir, slice = slice)
        resFrame$saved.as[i] <- as.character(returnlist$saved.as[1])
        resFrame$runtime[i] <- as.character(returnlist$runtime[1])
        resFrame$loops[i] <- as.integer(returnlist$loops[1])
        resFrame$x.diff[i] <- as.integer(returnlist$x.diff[1])
        resFrame$y.diff[i] <- as.integer(returnlist$y.diff[1])
        resFrame$z.diff[i] <- as.integer(returnlist$z.diff[1])
      }, error=function(e){
        message("ERROR IN MANDIBLE ", shortname, ": ", conditionMessage(e))
        }, finally = {
          errList <- rbind(errList, shortname)
        })
      pcdone <- round((i/length(impfiles))*100)
      pcdonemsg <- paste(pcdone, "% done.", sep = "")
      message(pcdonemsg)
    }
    resFrame <- subset(resFrame, saved.as != 0)
    if (sum(resFrame$saved.as == 0) > 0){
      print(resFrame)
    }

    if (length(errList) > 0){
      errList <- data.frame("INCOMPLETE" = errList[,1])
      print(errList)
    }
  }

  endTime <- Sys.time()
  message(paste("The process took about", round(difftime(endTime, startTime, units = "mins")), "minute(s).", sep = " "))
}
