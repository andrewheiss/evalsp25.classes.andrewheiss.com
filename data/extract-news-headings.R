#' Convert Markdown snippets to HTML
#' 
#' This converts small snippets of Markdown to HTML.
#' 
#' @param x Markdown text.
#' @return A character string of HTML text.
#' @examples
#' md_to_html("This *is* a **test**.")
#' #> [1] "This <em>is</em> a <strong>test</strong>."
#' 
md_to_html <- function(x) {
  markdown::mark_html(text = x, template = FALSE) |>
    trimws() |>
    stringr::str_remove_all("<p>") |>
    stringr::str_remove_all("</p>")
}


#' Convert Pandoc inline elements to Markdown
#'
#' This converts Pandoc inline elements to Markdown/HTML. It handles a handful 
#' of important elements like italics, bold, code, and math.
#'
#' @param inline A list representing a Pandoc inline element.
#' @return A character string of Markdown text.
#' @examples
#' # Example inline structure for "A *nested* example"
#' inline_example <- list(
#'   list(t = "Str", c = "A"),
#'   list(t = "Space"),
#'   list(t = "Emph", c = list(
#'     list(t = "Str", c = "nested")
#'   )),
#'   list(t = "Space"),
#'   list(t = "Str", c = "example")
#' )
#' 
#' paste(sapply(inline_example, convert_to_markdown), collapse = "")
#' #> [1] "A <em>nested</em> example"
convert_to_markdown <- function(inline) {
  switch(inline$t,
    "Str" = inline$c,
    "Space" = " ",
    "Emph" = md_to_html(
      paste0("*", paste(sapply(inline$c, convert_to_markdown), collapse = ""), "*")
    ),
    "Strong" = md_to_html(
      paste0("**", paste(sapply(inline$c, convert_to_markdown), collapse = ""), "**")
    ),
    "Code" = md_to_html(paste0("`", inline$c[[2]], "`")),
    "Math" = paste0("\\(", inline$c[[2]], "\\)"),
    ""  # Default case for unhandled types
  )
}

#' Extract metadata and headings from a Quarto file
#'
#' @param post_path Path to a .qmd file.
#' @return A list containing the metadata and headings.
#' @examples
#' details <- extract_details("path/to/file.qmd")
extract_details <- function(post_path) {
  # Get YAML frontmatter
  metadata <- rmarkdown::yaml_front_matter(post_path)

  # Deal with the title
  title <- metadata$title |> structure(quoted = TRUE)

  # Deal with the date
  date_actual <- lubridate::parse_date_time(
    metadata$date, orders = c("ymd", "ymd HM", "ymd HMS")
  )
  date_iso <- format(date_actual, "%Y-%m-%dT%H:%M:%S") |> 
    structure(quoted = TRUE)

  # Deal with the categories
  categories <- metadata$categories |> 
    purrr::map(\(x) structure(x, quoted = TRUE))

  # Convert the path to a URL
  href <- paste0("/", fs::path_rel(post_path, start = ".")) |>
    fs::path_ext_set("html") |> 
    structure(quoted = TRUE)

  # Convert .qmd to JSON AST
  # This is tricky because Quarto files aren't fully readable by pandoc 
  # (https://github.com/quarto-dev/quarto-cli/discussions/541#discussioncomment-2512284)
  #
  # Code chunks that start with ```{r} are nonstandard Markdown, and pandoc
  # can't read them correctly. When it reads the file, it lumps all the text
  # after a code chunk into a CodeBlock block in the AST.
  #
  # So to work around this, I remove all computational code chunks (```{r},
  # {python}, {sql}, etc.) with some gross regex, then write the chunk-less
  # version of the file to a temporary file, *then* process that with pandoc
  temp_ast <- tempfile(fileext = ".json")
  temp_md <- tempfile(fileext = ".md")
  on.exit(unlink(c(temp_ast, temp_md)), add = TRUE)

  qmd_content <- readLines(post_path, warn = FALSE) |> 
    paste(collapse = "\n")

  qmd_sans_chunks <- qmd_content |>
    stringr::str_remove_all("(?s)```\\{[^}]*\\}.*?```")

  writeLines(qmd_sans_chunks, temp_md)

  rmarkdown::pandoc_convert(
    temp_md, 
    to = "json", from = "markdown", 
    output = temp_ast, wd = here::here()
  )

  ast <- jsonlite::fromJSON(
    readLines(temp_ast, warn = FALSE), 
    simplifyDataFrame = FALSE
  )

  # Iterate through the gnarly list of AST blocks and extract all third-level
  # headings and their IDs (or slugs)
  headings <- ast$blocks |> 
    purrr::map(function(block) {
      if (block$t == "Header" && block$c[[1]] == 3) {
        list(
          id = block$c[[2]][[1]] |> 
            structure(quoted = TRUE),
          text = paste(purrr::map_chr(block$c[[3]], convert_to_markdown), collapse = "") |> 
            structure(quoted = TRUE)
        )
      }
    }) |> 
    purrr::compact()

  # Return stuff
  list(
    title = title,
    categories = categories,
    date_actual = format(as.numeric(date_actual), scientific = FALSE),
    date = date_iso,
    href = href,
    headings = headings
  )
}

# Find all the .qmd files in news/
posts <- fs::dir_ls(here::here("news"), regexp = "\\.qmd$") |> 
  purrr::discard(\(x) x == here::here("news/index.qmd"))

# Extract the details from each .qmd file
posts_details <- purrr::map(posts, extract_details) |> unname()

# Sort the posts by date
posts_details <- posts_details[
  order(
    purrr::map_dbl(posts_details, ~ as.numeric(.x$date_actual)),
    decreasing = TRUE
  )
]

# Save the details as YAML so the listing page can read it
writeLines(
  c(
    "# This file is generated with data/extract-news-headings.R; do not edit by hand\n", 
    yaml::as.yaml(posts_details)
  ), 
  con = here::here("data/news.yml")
)
