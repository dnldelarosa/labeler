#' Save a Dict to Database
#' `r lifecycle::badge("experimental")`
#'
#' Saves a Dict object to a database table. The table `labeler_dicts` is created
#' automatically if it doesn't exist.
#'
#' @param con A DBI database connection.
#' @param dict A Dict object.
#' @param overwrite Logical. If TRUE, overwrites existing dict with same name.
#'   If FALSE (default), throws an error if dict already exists.
#'
#' @return Invisibly returns the dict name.
#' @export
#'
#' @examples
#' \dontrun{
#'     con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'     db_save_dict(con, my_dict)
#'     db_save_dict(con, my_dict, overwrite = TRUE)  
#'     DBI::dbDisconnect(con)
#' }
db_save_dict <- function(con, dict, overwrite = FALSE) {
  rlang::check_installed("DBI")
  if (!is.Dict(dict)) {
    stop("dict must be a Dict object. See ?Dict for more details.")

  }

  .ensure_dict_table(con)

  dict_name <- dict$metadata$name
  json_content <- as.character(to_json(dict, pretty = FALSE))
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")


  # Check if dict already exists
  existing <- DBI::dbGetQuery(
    con,
    "SELECT name FROM labeler_dicts WHERE name = ?",
    params = list(dict_name)
  )

  if (nrow(existing) > 0) {
    if (!overwrite) {
      stop(
        "Dict '", dict_name, "' already exists. ",
        "Use overwrite = TRUE to replace it."
      )
    }
    # Update existing
    DBI::dbExecute(
      con,
      "UPDATE labeler_dicts SET json_content = ?, updated_at = ? WHERE name = ?",
      params = list(json_content, now, dict_name)
    )
  } else {
    # Insert new
    DBI::dbExecute(
      con,
      "INSERT INTO labeler_dicts (name, json_content, created_at, updated_at) VALUES (?, ?, ?, ?)",
      params = list(dict_name, json_content, now, now)
    )
  }

  invisible(dict_name)
}


#' Load a Dict from Database
#' `r lifecycle::badge("experimental")`
#'
#' Loads a Dict object from the database by name.
#'
#' @param con A DBI database connection.
#' @param name Character string. The name of the dict to load.
#'
#' @return A Dict object.
#' @export
#'
#' @examples
#' \dontrun{
#'     con <- DBI::dbConnect(RSQLite::SQLite(), "my_db.sqlite")
#'     my_dict <- db_load_dict(con, "survey_2024")
#'     DBI::dbDisconnect(con)
#' }
db_load_dict <- function(con, name) {
  rlang::check_installed("DBI")
  .ensure_dict_table(con)

  result <- DBI::dbGetQuery(
    con,
    "SELECT json_content FROM labeler_dicts WHERE name = ?",
    params = list(name)
  )

  if (nrow(result) == 0) {
    stop("Dict '", name, "' not found in database.")
  }

  from_json(result$json_content[1])
}


#' List Dicts in Database
#' `r lifecycle::badge("experimental")`
#'
#' Lists all dictionary names stored in the database.
#'
#' @param con A DBI database connection.
#'
#' @return A data.frame with columns: name, created_at, updated_at.
#' @export
#'
#' @examples
#' \dontrun{
#'     con <- DBI::dbConnect(RSQLite::SQLite(), "my_db.sqlite")
#'     db_list_dicts(con)
#'     DBI::dbDisconnect(con)
#' }
db_list_dicts <- function(con) {
  rlang::check_installed("DBI")
  .ensure_dict_table(con)

  DBI::dbGetQuery(
    con,
    "SELECT name, created_at, updated_at FROM labeler_dicts ORDER BY name"
  )
}


#' Delete a Dict from Database
#' `r lifecycle::badge("experimental")`
#'
#' Deletes a dictionary from the database by name.
#'
#' @param con A DBI database connection.
#' @param name Character string. The name of the dict to delete.
#'
#' @return Invisibly returns TRUE if deleted, FALSE if not found.
#' @export
#'
#' @examples
#' \dontrun{
#'     con <- DBI::dbConnect(RSQLite::SQLite(), "my_db.sqlite")
#'     db_delete_dict(con, "old_survey")
#'     DBI::dbDisconnect(con)
#' }
db_delete_dict <- function(con, name) {
  rlang::check_installed("DBI")
  .ensure_dict_table(con)

  rows_affected <- DBI::dbExecute(
    con,
    "DELETE FROM labeler_dicts WHERE name = ?",
    params = list(name)
  )

  invisible(rows_affected > 0)
}


# Internal: Ensure the labeler_dicts table exists
.ensure_dict_table <- function(con) {
  if (!DBI::dbExistsTable(con, "labeler_dicts")) {
    DBI::dbExecute(con, "
      CREATE TABLE labeler_dicts (
        name TEXT PRIMARY KEY,
        json_content TEXT NOT NULL,
        created_at TEXT,
        updated_at TEXT
      )
    ")
  }
  invisible(TRUE)
}
