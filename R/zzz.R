.onAttach <- function(libname, pkgname) {
    if (!isTRUE(getOption("gnafr.verbose", TRUE))) return(invisible())
    version <- unname(getNamespaceVersion(pkgname))
    packageStartupMessage(paste(.index_banner(version), collapse = "\n"))
}
