#' require_class
#'
#' Require an object to have the specified class.  If it doesn't, an error is thrown.
#'
#' @param req_class `(character)` : the required class name.
#' @param obj       `(R object)` :  the object.
#' @param obj_name  `(character)` : the name of the object in memory.
#'
#' @return
#' @export
#'
#' @examples
require_class <- function(req_class, obj, obj_name) {
  if (obj$class != req_class) stop(
    'Invalid class for object: ', obj_name,
    '. Requires class: ', req_class,
    ', but object is class: ', obj$class, '.'
  )
}
