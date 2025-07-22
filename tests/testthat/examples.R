installations <- function() {
  path <- tempfile()
  dir.create(path)
  path_lib <- "lib"
  path_bootstrap <- bootstrap_library("remotes")

  writeLines('install.packages("R6")', file.path(path, "provision.R"))
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap)
  withr::with_dir(path, conan_run(cfg, show_log = TRUE))

  writeLines('install.packages("ids")', file.path(path, "provision.R"))
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap)
  withr::with_dir(path, conan_run(cfg, show_log = TRUE))

  writeLines('remotes::install_github("richfitz/ids@tidy")',
             file.path(path, "provision.R"))
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap)
  withr::with_dir(path, conan_run(cfg, show_log = TRUE))

  writeLines('remotes::install_github("richfitz/ids")',
             file.path(path, "provision.R"))
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap)
  withr::with_dir(path, conan_run(cfg, show_log = TRUE))

  writeLines('remove.packages(c("askpass", "openssl"))',
             file.path(path, "provision.R"))
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap)
  withr::with_dir(path, conan_run(cfg, show_log = TRUE))

  nms <- dir(file.path(path, "lib/.conan"))
  res <- lapply(file.path(path, "lib/.conan", nms), readRDS)
  names(res) <- nms
  res
}

fs::dir_create("examples")
saveRDS(installations(), file.path("examples/installations.rds"),
        version = 2)
