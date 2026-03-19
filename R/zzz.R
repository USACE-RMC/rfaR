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
  packageStartupMessage(castle)
  packageStartupMessage("rfaR: RMC-Reservoir Frequency Analysis in R")
  packageStartupMessage("Version: ", utils::packageVersion("rfaR"), ' - "Half Baked"')
  packageStartupMessage("US Army Corps of Engineers - Risk Management Center (USACE-RMC)")
}
