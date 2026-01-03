test_that("db_save_dict and db_load_dict work with in-memory SQLite", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  my_dict <- Dict(
    metadata = list(name = "test_dict"),
    var1 = list(label = "Variable 1", labels = c("A" = 1, "B" = 2))
  )

  # Save
  result <- db_save_dict(con, my_dict)
  expect_equal(result, "test_dict")

  # Table should exist
  expect_true(DBI::dbExistsTable(con, "labeler_dicts"))

  # Load back
  loaded <- db_load_dict(con, "test_dict")
  expect_true(is.Dict(loaded))
  expect_equal(loaded$metadata$name, "test_dict")
  expect_equal(loaded$var1$label, "Variable 1")
  expect_equal(loaded$var1$labels, c("A" = 1, "B" = 2))
})


test_that("db_save_dict respects overwrite argument", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  dict1 <- Dict(
    metadata = list(name = "my_dict"),
    var1 = list(label = "Original")
  )

  dict2 <- Dict(
    metadata = list(name = "my_dict"),
    var1 = list(label = "Updated")
  )

  # Save first
  db_save_dict(con, dict1)

  # Should error without overwrite
  expect_error(
    db_save_dict(con, dict2, overwrite = FALSE),
    "already exists"
  )

  # Should work with overwrite
  db_save_dict(con, dict2, overwrite = TRUE)

  # Verify update
  loaded <- db_load_dict(con, "my_dict")
  expect_equal(loaded$var1$label, "Updated")
})


test_that("db_list_dicts returns correct info", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  # Empty at start
  result <- db_list_dicts(con)
  expect_equal(nrow(result), 0)

  # Add some dicts
  db_save_dict(con, Dict(metadata = list(name = "dict_a"), x = list(label = "X")))
  db_save_dict(con, Dict(metadata = list(name = "dict_b"), y = list(label = "Y")))

  result <- db_list_dicts(con)
  expect_equal(nrow(result), 2)
  expect_true("dict_a" %in% result$name)
  expect_true("dict_b" %in% result$name)
  expect_true("created_at" %in% names(result))
  expect_true("updated_at" %in% names(result))
})


test_that("db_delete_dict removes dict", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  db_save_dict(con, Dict(metadata = list(name = "to_delete"), x = list(label = "X")))

  # Verify exists
  expect_equal(nrow(db_list_dicts(con)), 1)

  # Delete
  result <- db_delete_dict(con, "to_delete")
  expect_true(result)

  # Verify gone
  expect_equal(nrow(db_list_dicts(con)), 0)

  # Delete non-existent returns FALSE
  result <- db_delete_dict(con, "non_existent")
  expect_false(result)
})


test_that("db_load_dict errors on missing dict", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  expect_error(
    db_load_dict(con, "does_not_exist"),
    "not found"
  )
})


test_that("table is auto-created on first use", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  # Table doesn't exist yet
  expect_false(DBI::dbExistsTable(con, "labeler_dicts"))

  # Call list (should create table)
  db_list_dicts(con)

  # Now it exists
  expect_true(DBI::dbExistsTable(con, "labeler_dicts"))
})
