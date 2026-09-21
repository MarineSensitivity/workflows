# reload the Shiny apps on BOTH instances ----
#
# There are two Shiny Server blocks (server/rstudio/shiny-server.conf): the PUBLIC one (:3838,
# serving the apps_v8 checkout) and the PREVIEW one (:3839, serving 3-line wrapper apps from the
# server repo with MS_PREVIEW=1, behind the signed-in review host). Shiny Server watches
# `restart.txt` in the directory it SERVES, so each instance has its own file -- and a worker
# memoizes the version registry and the manifests it has read, so it must be rolled over whenever
# a release's data lands, not only when app code changes.
#
# Three notebooks touched only the public file. The symptom (2026-09-21, v7.1): v7b's tables,
# COGs, cell_model and manifest were all published, and the preview host answered HTTP 500 for
# /v7b/scores/ until DEPLOY_APPS=1 happened to reload it -- a reviewer-facing release that did not
# open, behind a gate the public checks cannot see past. One definition, used everywhere.
#
# paths are the SERVER's; /share is mounted at the same path inside the rstudio container, so the
# command is valid both over `ssh msens` and from a render running in the container.
app_reload_cmd <- function(
    apps     = c("species", "scores"),
    apps_co  = "/share/github/MarineSensitivity/apps_v8",
    srv_repo = "/share/github/MarineSensitivity/server") {
  paste(c(
    sprintf("touch %s/%s/restart.txt", apps_co, apps),
    sprintf("touch %s/rstudio/shiny_apps_preview/%s/restart.txt", srv_repo, apps)),
    collapse = "; ")
}
