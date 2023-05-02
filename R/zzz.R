.onAttach <- function(...) {
  if (interactive() && ("ggcor" %in% loadedNamespaces())) {
    msg <- paste0("The `ggcor` package has been loaded, and it's some functions may conflict\n",
                  "with `linkET`, please detach or uninstall the `ggcor` package first.")
    packageStartupMessage(msg)
  }
}

.onLoad <- function(...) {
  ### register pairs plot parameters
  register_pairs_plot(reset = TRUE)

  ### register S3 method about network
  # for(g in c("cor_md_tbl", "correlate", "mantel_tbl",
  #            "easycorrelation", "rcorr", "corr.test")) {
  #   register_s3_method("igraph", "as.igraph", g)
  #   register_s3_method("tidygraph", "as_tbl_graph", g)
  # }

  ### register S3 method about qcorrplot
  for (g in c("cor_md_tbl", "default")) {
    register_s3_method("linkET", "qcorrplot", g)
  }

  ### register S3 method about as_matrix_data
  for (g in c("matrix", "data.frame", "correlate", "grouped_correlate",
              "rcorr", "corr.test", "default")) {
    register_s3_method("linkET", "as_matrix_data", g)
  }

  ### register S3 method about as_md_tbl
  for (g in c("matrix_data", "grouped_matrix_data", "mantel_tbl",
              "easycorrelation", "correlate", "grouped_correlate",
              "rcorr", "corr.test", "data.frame", "matrix", "md_tbl",
              "default")) {
    register_s3_method("linkET", "as_md_tbl", g)
  }

  ### register S3 method about marker
  for (g in c("grob", "gList", "ggplot", "raster", "magick-image", "formula",
              "character", "list", "marker")) {
    register_s3_method("linkET", "marker", g)
  }

  ### register S3 method about as_correlate
  for (g in c("matrix", "data.frame", "rcorr", "corr.test", "easycorrelation")) {
    register_s3_method("linkET", "as_correlate", g)
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
