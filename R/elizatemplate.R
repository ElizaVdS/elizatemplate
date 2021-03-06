#' elizatemplate
#'
#' Package with template for R Markdown pdf
#'
#' @name elizatemplate
#' @docType package

NULL


#  Internal functions
#  Utilities for article() and memo()
#  Largely inspired from the rticles package
#' @keywords internal
find_file <- function (template, file) {
  template <- system.file("rmarkdown", "templates", template, file, package="elizatemplate")
  if (template == "") {
    stop("Couldn't find template file ", template, "/", file, call.=FALSE)
  }
  return(template)
}
#' @keywords internal
find_resource <- function (template, file) {
  return(find_file(template, file.path("resources", file)))
}
#' @keywords internal
inherit_pdf_document <- function (...) {
  fmt <- rmarkdown::pdf_document(...)
  fmt$inherits <- "pdf_document"
  return(fmt)
}

#' Article
#'
#' Formatting an article for self-archiving
#'
#' The fonction is called by the Markdown model Basic article
#'
#' @param ... Optional arguments passed to \code{\link{pdf_document}}
#' @param md_extensions Markdodown extensions, cf. \code{\link{pdf_document}}
#'
#' @export
article <- function (..., md_extensions=c("-autolink_bare_uris")) {
  inherit_pdf_document(..., template=find_resource("article", "template.tex"), md_extensions=md_extensions, citation_package="natbib")
}



#' Tricoter
#'
#' Creating all documents from the models
#'
#' Used to verify that the models function well
#'
#' @param destination Destination file of the documents
#'
#' @name Tricoter
NULL


#' @rdname Tricoter
#' @export
TricoterTout <- function (destination="docs") {
  TricoterArticle(destination=destination)
  TricoterPresentation(destination=destination)
  TricoterOuvrage(destination=destination)
  TricoterMemo(destination=destination)
}

#' @rdname Tricoter
#' @export
TricoterArticle <- function (destination="docs") {
  # Preparation
  knitr_table_format <- options("knitr.table.format")
  OriginalWD <- getwd()
  tmpdir <- tempdir()
  # Article
  setwd(tmpdir)
  unlink("article", recursive=TRUE)
  rmarkdown::draft("article", template="article", package="elizatemplate", edit=FALSE)
  setwd("article")
  # Knit to HTML
  options(knitr.table.format='html')
  rmarkdown::render(input="article.Rmd",
                    output_format=bookdown::html_document2(theme="sandstone", toc=TRUE, toc_float=TRUE),
                    output_dir=destination)
  # Knit to pdf
  options(knitr.table.format='latex')
  rmarkdown::render(input="article.Rmd",
                    output_format=bookdown::pdf_book(base_format=elizatemplate::article),
                    output_dir=destination)
  # Copy to destination
  docsDirs <- list.dirs(path=destination, full.names=TRUE, recursive=TRUE)
  dir.create(paste(OriginalWD, "/", destination, sep=""), showWarnings=FALSE)
  dir.create(paste(OriginalWD, "/", destination, "/article", sep=""), showWarnings=FALSE)
  if (length(docsDirs) > 0) {
    sapply(paste(OriginalWD, "/", destination, "/article/", docsDirs, sep=""), dir.create, showWarnings=FALSE)
    docsFiles <- list.files(destination, full.names=TRUE, recursive=TRUE)
    file.copy(from=docsFiles, to=paste(OriginalWD, "/", destination, "/article/", docsFiles, sep=""), overwrite=TRUE)
  }
  # Clean up
  setwd(OriginalWD)
  unlink(paste(tmpdir, "/article", sep=""), recursive=TRUE)
  options(knitr.table.format=knitr_table_format)
}
