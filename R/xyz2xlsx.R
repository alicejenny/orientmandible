#' xyz to xlsx File Converter
#'
#' Batch converts .xyz files containing 3 columns of point cloud data to .xlsx.
#' @param folder Save folder. User can choose interactively if left blank.
#' @seealso \code{\line{xlsx2xyz}} which performs the inverse function.
#' @export

xyz2xlsx <- function(folder = choose.dir(), saveplots = TRUE){
  require(openxlsx)
  require(stringr)
  file.filter <- matrix(c("xyz files", "*.xyz"), ncol = 2, byrow = TRUE)
  filelist <- choose.files(caption = "Select the files to import", filters = file.filter)
  nfiles <- length(filelist)

  for (i in 1:nfiles){
    noext <- basename(filelist[i])
    xlsxfile <- str_replace(noext, ".xyz", ".xlsx")
    fileandpath <- paste(folder, xlsxfile, sep = "\\")
    xyzfile <- read.table(filelist[i], header = FALSE, col.names = c("x", "y", "z"))
    if (file.exists(fileandpath) == TRUE){
      file.remove(fileandpath)
    }
    wb <- createWorkbook()
    addWorksheet(wb, noext)
    writeData(wb, noext, xyzfile, colNames = TRUE, rowNames = FALSE)
    saveWorkbook(wb, fileandpath, overwrite = TRUE)

    plotpath <- paste(folder, "plots\\", sep = "\\")
    if (dir.exists(plotpath) == FALSE){
      dir.create(plotpath)
    }

    plot.new()
    if (saveplots == TRUE){
      png(filename = paste(plotpath, noext, "2", ".png", sep = ""), width = 1000, height = 1000)
    }
    par(mfrow=c(2,3))
    plot(xyzfile$x, xyzfile$y, xlab = "x", ylab = "y", main = paste(noext, "xy", sep = " "), asp = 1)
    plot(xyzfile$y, xyzfile$z, xlab = "y", ylab = "z", main = paste(noext, "yz", sep = " "), asp = 1)
    plot(xyzfile$x, xyzfile$z, xlab = "x", ylab = "z", main = paste(noext, "xz", sep = " "), asp = 1)
    if (saveplots == TRUE){
      dev.off()
    }
  }
}
