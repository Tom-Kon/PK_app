# small helper for distinct colors
palette_hcl <- function(n, h = c(15, 375), c = 100, l = 60) {
  if (n <= 0) return(character(0))
  grDevices::hcl(h = seq(h[1], h[2], length.out = n + 1)[1:n], c = c, l = l)
}