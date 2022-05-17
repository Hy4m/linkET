.onAttach <- function(...) {
  if (interactive() && ("ggcor" %in% loadedNamespaces())) {
    msg <- paste0("The `ggcor` package has been loaded, and it's some functions may conflict\n",
                  "with `linkET`, please detach or uninstall the `ggcor` package first.")
    packageStartupMessage(msg)
  }

  if (!("ggcor" %in% loadedNamespaces()) &&
      ("ggcor" %in% row.names(utils::installed.packages()))) {
    if (stats::runif(1) > 0.5) {
      msg <- paste0("`linkET` has almost all of `ggcor`'s features and includes\n",
                    "many new features, so switching to `linkET` is recommended.")
      packageStartupMessage(msg)
    }
  }
}

