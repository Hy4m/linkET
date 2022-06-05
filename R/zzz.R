.onAttach <- function(...) {
  if (interactive() && ("ggcor" %in% loadedNamespaces())) {
    msg <- paste0("The `ggcor` package has been loaded, and it's some functions may conflict\n",
                  "with `linkET`, please detach or uninstall the `ggcor` package first.")
    packageStartupMessage(msg)
  }
}

.onLoad <- function(...) {
  register_pairs_plot()
}
