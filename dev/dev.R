required_packages <- c(
    'tidyverse/ellmer',
    'tidyverse/glue',
    'ropensci/cld2'
)

devtools::install_github(required_packages, build = TRUE)

install.packages(
    'Unicode',
    type = 'source',
    INSTALL_opts = '--byte-compile --no-multiarch'
)
