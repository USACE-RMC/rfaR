castle <- r"{
  _______________________________________
 /                                       \
/   _   _   _                 _   _   _   \
|  | |_| |_| |   _   _   _   | |_| |_| |  |
|   \   _   /   | |_| |_| |   \   _   /   |
|    | | | |     \       /     | | | |    |
|    | |_| |______|     |______| |_| |    |
|    |              ___              |    |
|    |  _    _    (     )    _    _  |    |
|    | | |  |_|  (       )  |_|  | | |    |
|    | |_|       |       |       |_| |    |
|   /            |_______|            \   |
|  |___________________________________|  |
\       USACE RISK MANAGEMENT CENTER      /
 \_______________________________________/
}"

.onAttach <- function(libname, pkgname) {
  if (runif(1) < 0.05) {
    utils::data(think, package = "rfaR", envir = environment())
    packageStartupMessage(castle)
    packageStartupMessage(think)
  } else {
    packageStartupMessage(castle)
  }
  packageStartupMessage("rfaR: RMC-Reservoir Frequency Analysis in R")
  packageStartupMessage("Version: ", utils::packageVersion("rfaR"))
  packageStartupMessage("US Army Corps of Engineers - Risk Management Center (USACE-RMC)")
}
