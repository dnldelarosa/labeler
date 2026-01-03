#' Convert a Dict Object to JSON
#' `r lifecycle::badge("experimental")`
#'
#' Serializes a Dict object to JSON format, either as a string or written to a file.
#'
#' @param dict A Dict object.
#' @param path Optional file path to write the JSON. If NULL, returns a JSON string.
#' @param pretty Logical, whether to format JSON with indentation (default TRUE).
#'
#' @return If `path` is NULL, returns a JSON string. Otherwise, writes to file and returns the path invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#'     # Get JSON string
#'     json_str <- to_json(lbl_df_dict)
#'     cat(json_str)
#'
#'     # Write to file
#'     to_json(lbl_df_dict, path = "my_dictionary.json")
#' }
to_json <- function(dict, path = NULL, pretty = TRUE) {
  if (!is.Dict(dict)) {
    stop("dict must be a Dict object. See ?Dict for more details.")
  }

  json_list <- .dict_to_json_list(dict)

  json_str <- jsonlite::toJSON(
    json_list,
    pretty = pretty,
    auto_unbox = TRUE,
    null = "null"
  )

  if (is.null(path)) {
    return(json_str)
  } else {
    writeLines(json_str, path)
    return(invisible(path))
  }
}


#' Read a Dict Object from JSON
#' `r lifecycle::badge("experimental")`
#'
#' Reads a JSON string or file and converts it to a Dict object.
#'
#' @param x Either a JSON string or a file path to a JSON file.
#'
#' @return A Dict object.
#' @export
#'
#' @examples
#' \dontrun{
#'     # From file
#'     my_dict <- from_json("my_dictionary.json")
#'
#'     # From string
#'     json_str <- '{"metadata": {"name": "Test"}, "variables": {"x": {"label": "X"}}}'
#'     my_dict <- from_json(json_str)
#' }
from_json <- function(x) {
  if (file.exists(x)) {
    json_str <- paste(readLines(x, warn = FALSE), collapse = "\n")
  } else {
    json_str <- x
  }

  json_list <- jsonlite::fromJSON(json_str, simplifyVector = TRUE)

  .json_list_to_dict(json_list)
}


# Internal: Convert Dict to JSON-compatible list structure
.dict_to_json_list <- function(dict) {
  # Extract metadata
  metadata <- dict$metadata

  # Extract variables (everything except metadata)
  var_names <- names(dict)[names(dict) != "metadata"]
  variables <- list()

  for (var_name in var_names) {
    var_def <- dict[[var_name]]
    # Convert named vector labels to list for proper JSON serialization
    if (!is.null(var_def$labels)) {
      var_def$labels <- as.list(var_def$labels)
    }
    variables[[var_name]] <- var_def
  }

  list(
    `$schema` = "https://labeler.adatar.do/dict-schema/v1",
    metadata = metadata,
    variables = variables
  )
}


# Internal: Convert JSON list to Dict
.json_list_to_dict <- function(json_list) {
  # Validate structure
  if (is.null(json_list$metadata)) {
    stop("Invalid JSON: missing 'metadata' field.")
  }
  if (is.null(json_list$metadata$name)) {
    stop("Invalid JSON: metadata must contain a 'name' field.")
  }
  if (is.null(json_list$variables)) {
    stop("Invalid JSON: missing 'variables' field.")
  }

  # Build Dict arguments
  dict_args <- list(metadata = as.list(json_list$metadata))

  for (var_name in names(json_list$variables)) {
    var_def <- json_list$variables[[var_name]]

    # Convert labels back to named vector if present
    if (!is.null(var_def$labels) && length(var_def$labels) > 0) {
      labels_list <- var_def$labels
      if (is.list(labels_list)) {
        var_def$labels <- unlist(labels_list)
      }
    }

    dict_args[[var_name]] <- as.list(var_def)
  }

  do.call(Dict, dict_args)
}
