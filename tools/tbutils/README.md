# tbutils


Internal helpers for the TROMBAT project.


## Install (development)
```r
# from your project root
renv::activate() # if you use renv
usethis::proj_set(".")
# build & install from local path
remotes::install_local("tools/tbutils", upgrade = "never")
# or devtools::load_all("tools/tbutils") during development