.onAttach <- function(...) {
  if (interactive() && ("ggcor" %in% loadedNamespaces())) {
    msg <- paste0("The `ggcor` package has been loaded, and it's some functions may conflict\n",
                  "with `linkET`, please detach or uninstall the `ggcor` package first.")
    packageStartupMessage(msg)
  }
}

.onLoad <- function(...) {
  register_pairs_plot(reset = TRUE)
  for(g in c("cor_md_tbl", "correlate", "mantel_tbl", "pro_tbl",
             "easycorrelation", "rcorr", "corr.test")) {
    register_s3_method("igraph", "as.igraph", g)
    register_s3_method("tidygraph", "as_tbl_graph", g)
  }
}

register_s3_method <- function (pkg, generic, class, fun = NULL)
{
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  else {
    stopifnot(is.function(fun))
  }
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  setHook(packageEvent(pkg, "onLoad"), function(...) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  })
}
