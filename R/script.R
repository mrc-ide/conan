conan_scripts <- function(path) {
  for (name in c("bootstrap", "install")) {
    conan_script(name, path)
  }
}


## I wonder if we might do a different version that copes with conan
## being in the library already?
conan_script <- function(name, path) {
  dir.create(path, FALSE, TRUE)

  target <- switch(
    name,
    install = "main_install",
    bootstrap = "main_bootstrap",
    stop(sprintf("Unknown script '%s'", name)))

  env <- environment(conan_script)
  fns <- find_functions(target, env)

  code <- c(
    "#!/usr/bin/env Rscript",
    unlist(lapply(fns, deparse_fn)),
    sprintf("%s()", target))

  dest <- file.path(path, paste0("conan-", name))
  writeLines(code, dest)
  Sys.chmod(dest, "755")
  invisible(dest)
}


deparse_fn <- function(nm, ...) {
  value <- trimws(deparse(get(nm, ...)), "right")
  value[[1]] <- sprintf("%s <- %s", nm, value[[1]])
  value
}
