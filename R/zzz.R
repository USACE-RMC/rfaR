.onAttach <- function(libname, pkgname) {
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
  nickname <- utils::packageDescription(pkgname)$Nickname
  packageStartupMessage(castle)
  packageStartupMessage("rfaR: RMC-Reservoir Frequency Analysis in R")
  packageStartupMessage("Version: ", utils::packageVersion(pkgname), " - ", nickname)
  packageStartupMessage("US Army Corps of Engineers - Risk Management Center (USACE-RMC)")
}
