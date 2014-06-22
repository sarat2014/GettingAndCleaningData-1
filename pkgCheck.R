###############################################################################
## Script Name  : pkgCheck.R                                                 ##
## Author Name  : Vignesh Parameswaran                                       ##
## Date Created : 2014-06-22                                                 ##
## Version      : 1.0                                                        ##
## Description  : This script checks for the package                         ##
##                  If the package is present, load it                       ##
##                  Else install the package and load it                     ##
###############################################################################

pkgCheck <- function(x)
{
  if (!require(x, character.only = TRUE))
  {
    message("Installing package : ", x)
    install.packages(x, dep = TRUE, quiet = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}