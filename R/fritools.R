is_character_zero <- function(x) {
    is <- identical(length(x), 0L) && is(x, "character")
    return(is)
}
