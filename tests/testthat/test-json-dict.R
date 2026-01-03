test_that("to_json produces valid JSON string", {
  my_dict <- Dict(
    metadata = list(name = "Test Dict"),
    sex = list(
      label = "Sex || What is your sex?",
      labels = c("Male" = 1, "Female" = 2),
      dtype = "integer"
    ),
    age = list(
      label = "Age || How old are you?",
      dtype = "integer"
    )
  )

  json_str <- to_json(my_dict)

  # Should be a character string

  expect_type(json_str, "character")

  # Should contain expected fields
  expect_match(json_str, '"\\$schema"')
  expect_match(json_str, '"metadata"')
  expect_match(json_str, '"variables"')
  expect_match(json_str, '"sex"')
  expect_match(json_str, '"age"')
})


test_that("from_json from string recreates Dict", {
  json_str <- '{
    "$schema": "https://labeler.adatar.do/dict-schema/v1",
    "metadata": {"name": "Test"},
    "variables": {
      "x": {"label": "Variable X", "labels": {"A": 1, "B": 2}}
    }
  }'

  dict <- from_json(json_str)

  expect_true(is.Dict(dict))
  expect_equal(dict$metadata$name, "Test")
  expect_equal(dict$x$label, "Variable X")
  expect_equal(dict$x$labels, c("A" = 1, "B" = 2))
})


test_that("round-trip Dict -> JSON -> Dict preserves data", {
  original <- Dict(
    metadata = list(name = "Round Trip Test", version = "1.0"),
    gender = list(
      label = "Gender || What is your gender?",
      labels = c("Male" = 1, "Female" = 2, "Other" = 3),
      dtype = "integer"
    ),
    income = list(
      label = "Income || Annual income",
      dtype = "numeric"
    )
  )

  json_str <- to_json(original)
  restored <- from_json(json_str)

  # Structure should match
  expect_true(is.Dict(restored))
  expect_equal(restored$metadata$name, original$metadata$name)
  expect_equal(restored$metadata$version, original$metadata$version)
  expect_equal(restored$gender$label, original$gender$label)
  expect_equal(restored$gender$labels, original$gender$labels)
  expect_equal(restored$gender$dtype, original$gender$dtype)
  expect_equal(restored$income$label, original$income$label)
  expect_equal(restored$income$dtype, original$income$dtype)
})


test_that("file I/O works correctly", {
  my_dict <- Dict(
    metadata = list(name = "File Test"),
    var1 = list(label = "Variable 1")
  )

  # Write to temp file
  temp_file <- tempfile(fileext = ".json")
  result_path <- to_json(my_dict, path = temp_file)

  # Check file was created
  expect_true(file.exists(temp_file))
  expect_equal(result_path, temp_file)

  # Read back
  restored <- from_json(temp_file)

  expect_true(is.Dict(restored))
  expect_equal(restored$metadata$name, "File Test")
  expect_equal(restored$var1$label, "Variable 1")

  # Cleanup
  unlink(temp_file)
})


test_that("from_json errors on invalid JSON", {
  # Missing metadata
  expect_error(
    from_json('{"variables": {"x": {"label": "X"}}}'),
    "missing 'metadata'"
  )

  # Missing name in metadata
  expect_error(
    from_json('{"metadata": {}, "variables": {"x": {"label": "X"}}}'),
    "must contain a 'name'"
  )

  # Missing variables
  expect_error(
    from_json('{"metadata": {"name": "Test"}}'),
    "missing 'variables'"
  )
})


test_that("to_json errors on non-Dict input", {
  expect_error(
    to_json(list(a = 1)),
    "must be a Dict object"
  )

  expect_error(
    to_json("not a dict"),
    "must be a Dict object"
  )
})
