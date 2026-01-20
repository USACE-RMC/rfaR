## Next Steps for rfaR
install.packages("DiagrammeR")
library(DiagrammeR)

grViz("
digraph flood_analysis_workflow {

  # Graph attributes
  graph [rankdir = TB, fontname = Helvetica, fontsize = 12]

  # Node attributes
  node [shape = box, style = filled, fontname = Helvetica, fontsize = 11]

  # Main package node
  rfaR [label = 'rfaR', fillcolor = '#4682B4', color = '#000080', fontcolor = 'white',
        style = 'filled,bold', penwidth = 2]

  # Primary branches
  functionality [label = 'Functionality', fillcolor = '#E6E6FA', style = 'filled,rounded']
  documentation [label = 'Required Documentation', fillcolor = '#E6E6FA', style = 'filled,rounded']

  # Functionality components
  volfreq [label = 'Volume Frequency\nSampling', fillcolor = '#F39200FF', color = '#8B0000']
  season [label = 'Flood Seasonality\nSampling', fillcolor = '#F39200FF', color = '#8B0000']
  stage [label = 'Starting Stage\nSampling', fillcolor = '#F39200FF', color = '#8B0000']
  hydro [label = 'Hydrograph Shape\nSampling', fillcolor = '#F39200FF', color = '#8B0000']
  modpuls [label = 'Mod-Puls\nRouting', fillcolor = '#95C11FFF', color = '#006400']

  # Documentation branches
  internal [label = 'Internal', fillcolor = 'white', color = 'black']
  cran [label = 'CRAN', fillcolor = 'white', color = 'black']

  # Internal documentation items
  quickstart [label = 'Quick Start Guide', fillcolor = 'white', color = 'gray50']
  helpages [label = 'Function Help Pages/\nVignettes', fillcolor = 'white', color = 'gray50']
  validation [label = 'Validation &\nUnit Checking', fillcolor = 'white', color = 'gray50']
  examples [label = 'Example Walkthroughs', fillcolor = 'white', color = 'gray50']

  # CRAN documentation items
  cmdcheck [label = 'R CMD check\n--as-cran', fillcolor = 'white', color = 'gray50']
  standards [label = 'Package Standards', fillcolor = 'white', color = 'gray50']
  behavior [label = 'Code Behavior', fillcolor = 'white', color = 'gray50']
  updates [label = 'Periodic Updates', fillcolor = 'white', color = 'gray50']

  # Main structure
  rfaR -> functionality
  rfaR -> documentation

  # Functionality branches
  functionality -> volfreq
  functionality -> season
  functionality -> stage
  functionality -> hydro
  functionality -> modpuls

  # Documentation branches
  documentation -> internal
  documentation -> cran

  # Internal documentation items
  internal -> quickstart
  internal -> helpages
  internal -> validation
  internal -> examples

  # CRAN documentation items
  cran -> cmdcheck
  cran -> standards
  cran -> behavior
  cran -> updates
}
")

