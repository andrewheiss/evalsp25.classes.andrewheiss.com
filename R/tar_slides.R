slides <- tibble::tibble(
  path = list.files(here_rel("slides"), pattern = "\\.Rmd", full.names = TRUE)
) %>%
  mutate(
    name = tools::file_path_sans_ext(basename(path)),
    sym = syms(janitor::make_clean_names(paste0("slide_rmd_", name))),
    sym_html = syms(janitor::make_clean_names(paste0("slide_html_", name))),
    sym_pdf = syms(janitor::make_clean_names(paste0("slide_pdf_", name)))
  )

build_slides <- list(
  tar_eval(
    tar_files_input(target_name, rmd_file),
    values = list(
      target_name = slides$sym,
      rmd_file = slides$path
    )
  ),

  tar_eval(
    tar_target(target_name, render_xaringan(rmd_file), format = "file"),
    values = list(
      target_name = slides$sym_html,
      rmd_file = slides$sym
    )
  ),

  tar_eval(
    tar_target(target_name, xaringan_to_pdf(html_file), format = "file"),
    values = list(
      target_name = slides$sym_pdf,
      html_file = slides$sym_html
    )
  )
)


# We need to return the path to the rendered HTML file. In this case,
# rmarkdown::render() *does* return a path, but it returns an absolute path,
# which makes the targets pipeline less portable. So we return our own path to
# the HTML file instead.
render_xaringan <- function(slide_path) {
  # crayon does weird things to R Markdown and xaringan output, so we need to
  # disable it here. This is the same thing that tarchetypes::tar_render() does
  # behind the scenes too.
  withr::local_options(list(crayon.enabled = NULL))
  rmarkdown::render(slide_path, quiet = TRUE)
  return(paste0(tools::file_path_sans_ext(slide_path), ".html"))
}


# Use pagedown to convert xaringan HTML slides to PDF. Return a relative path to
# the PDF to keep targets happy.
#
# Slides for sessions 10 and 14 are huge, so use chromote to convert them
# outside of this targets pipeline instead
#
# But that doesn't work inside renv or targets for whatever reasons, so this has
# to be done manually from a different non-renv session:
#
# renderthis::to_pdf(
#   from = "~/Sites/courses/evalsp25/slides/10-slides.html", 
#   to = "~/Sites/courses/evalsp25/slides/10-slides.pdf", 
#   complex_slides = TRUE
# )

# BUT WAIT, complex_slides is temporarily broken in early 2025 because of
# changes in headless Chrome (https://github.com/rstudio/chromote/issues/193)

# A (slow) workaround is to use decktape
# (https://github.com/astefanutti/decktape) through Docker with this cryptic
# command (after serving the slides with a live server in VS Code or Positron or
# whatever)

# docker run --rm -t --net=host -v `pwd`:/slides astefanutti/decktape http://host.docker.internal:5500/05-class.html slides.pdf

xaringan_to_pdf <- function(slide_path) {
  path_sans_ext <- tools::file_path_sans_ext(slide_path)

  # if (path_sans_ext == "slides/03-class") {
  #   return(here::here("slides/03-class.pdf"))
  # }

  if (path_sans_ext == "slides/04-class") {
    return(here::here("slides/04-class.pdf"))
  }

  if (path_sans_ext == "slides/05-class") {
    return(here::here("slides/05-class.pdf"))
  }

  if (path_sans_ext == "slides/06-class") {
    return(here::here("slides/06-class.pdf"))
  }

  if (path_sans_ext == "slides/07-class") {
    return(here::here("slides/07-class.pdf"))
  }

  if (path_sans_ext %in% c("slides/10-slides",
                           "slides/14-slides", 
                           "slides/07-slides",
                           "slides/08-slides")) {
    complex <- FALSE
  } else {
    renderthis::to_pdf(
      slide_path,
      to = paste0(path_sans_ext, ".pdf")
    )
  }

  return(paste0(tools::file_path_sans_ext(slide_path), ".pdf"))
}
