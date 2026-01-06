#' Apply a Dictionary to a Dataset
#' `r lifecycle::badge("experimental")`
#'
#' Applies labels and data types defined in a Dict object to a dataset.
#'
#' @param tbl A data.frame or tibble.
#' @param dict A Dict object.
#' @param subset (Optional) A character vector specifying the variables to which the dictionary should be applied.
#'   If NULL (default), the dictionary is applied to all variables in the dataset that are also present in the dictionary.
#' @param dtypes Logical, indicating whether to apply data types defined in the dictionary (default is TRUE).
#' @param label_type Character string specifying how variable labels should be applied.
#'   Options are "full" (default, uses the entire label), "label" (uses the part after "||"), or "name" (uses the part before "||").
#'   See \code{Dict} for more details.
#'
#' @return The dataset with labels and data types applied according to the dictionary.
#' @export
#'
#' @examples
#' \dontrun{
#' labeled_data <- set_Dict(lbl_df, lbl_df_dict)
#' }
set_Dict <- function(tbl, dict, subset = NULL, dtypes = TRUE, label_type = c("full", "label", "name")) {
  # browser()
  if (!is.Dict(dict)) stop("dict must be a Dict object. See ?Dict for more details.")
  if (!is.data.frame(tbl)) stop("tbl must be a data.frame. See ?data.frame for more details.")

  label_type <- ifelse(length(label_type) > 1, label_type[1], label_type)
  label_type <- match.arg(label_type, c("full", "label", "name"))

  attr(tbl, "dict_metadata") <- dict$metadata

  if (!is.null(subset)) {
    if (!is.character(subset)) stop("subset must be a character vector of variable names.")
    # Filter to only variables that exist in both tbl and dict
    subset <- intersect(subset, names(tbl))
  } else {
    # Only apply to variables that exist in both dict and tbl
    dict_vars <- names(dict[!names(dict) %in% c("metadata")])
    subset <- intersect(dict_vars, names(tbl))
  }

  for (variable in subset) {
    if (dtypes) {
      tbl <- .apply_Dict_types(tbl, dict, variable)
    }
    .label <- dict[[variable]][["label"]]
    .label <- switch(label_type,
      full = .label,
      name = trimws(gsub("\\|\\|.*", "", .label)),
      label = trimws(gsub(".*\\|\\|", "", .label))
    )
    .labels <- dict[[variable]][["labels"]]
    tbl[[variable]] <- haven::labelled(
      tbl[[variable]],
      labels = .labels,
      label = .label
    )
  }

  tbl
}



.apply_Dict_types <- function(tbl, dict, subset = NULL) {
  for (variable in subset) {
    if (!variable %in% names(tbl)) {
      next
    }

    if (!is.null(dict[[variable]][["dtype"]])) {
      class(tbl[[variable]]) <- dict[[variable]][["dtype"]]
    }
  }

  return(tbl)
}




#   lbl_df |> set_Dict(lbl_df_dict) |> dplyr::count(sex, haven::as_factor(sex)) |> haven::as_factor()
#   lbl_df |> set_Dict(lbl_df_dict) |> dplyr::pull(sex) |> str()
#   lbl_df |> set_Dict(lbl_df_dict) |> dplyr::select(sex)
#   lbl_df |> set_Dict(lbl_df_dict) |> dplyr::count(sex) |> sjlabelled::label_to_colnames() |> sjlabelled::as_label()
