# Format-Fehlermeldungen -------------------------------------------------------

assertthat::on_failure(is_temporal) <- function(call, env) {
  
  paste(deparse(call$x), "does not contain temporal information")
}

assertthat::on_failure(is_long) <- function(call, env) {
  
  paste(deparse(call$x), "is not a long tiqqle")
}

assertthat::on_failure(is_tiqqle) <- function(call, env) {
  
  paste(deparse(call$x), "is not a tiqqle")
}

assertthat::on_failure(is_valid) <- function(call, env) {
  
  paste(deparse(call$x), "is not a valid tiqqle (neither long nor wide)")
}

assertthat::on_failure(is_wide) <- function(call, env) {
  
  paste(deparse(call$x), "is not a wide tiqqle")
}
