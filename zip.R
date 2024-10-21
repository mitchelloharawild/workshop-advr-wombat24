zip(
  "slides.zip",
  list.files("_site/slides", pattern = "\\.pdf$", full.names = TRUE),
  flags = "-j"
)
