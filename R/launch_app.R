##' launch_app
##' 
##' Visualise and interact with urban forest data of Cardiff
##' 
##' @return launch the gReeneRy app in internet browser window
##' @author Joe Peskett
##' @export 
launch_app <- function(){
        appDir <- system.file("shiny", "CityProfileR", package = "gRoot")
        if (appDir == "") {
            stop("Could not find example directory. Try re-installing `gRoot`.", call. = FALSE)
        }else{
        
        shiny::runApp(appDir, launch.browser = T,display.mode = "normal")
        }
    }