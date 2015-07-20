#' xlsx to xyz File Converter
#'
#' Batch converts .xlsx files containing 3 columns of point cloud data to .xyz.
#' @param folder Save folder. User can choose interactively if left blank.
#' @seealso \code{\line{xyz2xlsx}} which performs the inverse function.
#' @export

xlsx2xyz <- function(folder = choose.dir()){
  require(openxlsx)
  require(stringr)
  file.filter <- matrix(c("excel files", "*.xlsx"), ncol = 2, byrow = TRUE)
  filelist <- choose.files(caption = "Select the files to import", filters = file.filter)
  nfiles <- length(filelist)

  for (i in 1:nfiles){
    noext <- basename(filelist[i])
    xyzfile <- str_replace(noext, ".xlsx", ".xyz")
    fileandpath <- paste(folder, xyzfile, sep = "\\")
    xlsxfile <- read.xlsx(filelist[i])
    if (file.exists(fileandpath) == TRUE){
      file.remove(fileandpath)
    }
    write.table(xlsxfile, file = fileandpath, row.names = FALSE, col.names = FALSE)
  }
}
