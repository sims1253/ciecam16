library(testthat)

test_that("CAM16_Specification creation works", {
  # Test basic creation with all properties
  spec <- CAM16_Specification(
    J = 50,
    C = 30,
    h = 180,
    s = 2.5,
    Q = 75,
    M = 25,
    H = 200,
    e = 0.5
  )

  # expect_s7_class(spec, "CAM16_Specification")
  expect_equal(spec@J, 50)
  expect_equal(spec@C, 30)
  expect_equal(spec@h, 180)
  expect_equal(spec@s, 2.5)
  expect_equal(spec@Q, 75)
  expect_equal(spec@M, 25)
  expect_equal(spec@H, 200)
  expect_equal(spec@e, 0.5)

  # Test creation with partial properties
  spec_partial <- CAM16_Specification(J = 50, C = 30)
  expect_equal(spec_partial@J, 50)
  expect_equal(spec_partial@C, 30)
  expect_equal(spec_partial@h, numeric())

  # Test creation with no properties
  spec_empty <- CAM16_Specification()
  # expect_s7_class(spec_empty, "CAM16_Specification")
  expect_true(length(as.list(spec_empty)) == 0)
})

test_that("CAM16_Specification validates numeric ranges", {
  # Test J (Lightness) validation
  expect_error(CAM16_Specification(J = -1), "Invalid J: Value must be >= 0")
  expect_error(CAM16_Specification(J = 101), "Invalid J: Value must be <= 100")
  expect_error(CAM16_Specification(C = -1), "Invalid C: Value must be >= 0")
  expect_error(CAM16_Specification(C = 101), "Invalid C: Value must be <= 100")
  expect_error(CAM16_Specification(h = -1), "Invalid h: Value must be >= 0")
  expect_error(CAM16_Specification(h = 361), "Invalid h: Value must be <= 360")
  expect_error(CAM16_Specification(s = -1), "Invalid s: Value must be >= 0")
  expect_error(CAM16_Specification(s = 101), "Invalid s: Value must be <= 100")
  expect_error(CAM16_Specification(Q = -1), "Invalid Q: Value must be >= 0")
  expect_error(CAM16_Specification(Q = 101), "Invalid Q: Value must be <= 100")
  expect_error(CAM16_Specification(M = -1), "Invalid M: Value must be >= 0")
  expect_error(CAM16_Specification(M = 101), "Invalid M: Value must be <= 100")
  expect_error(CAM16_Specification(H = -1), "Invalid H: Value must be >= 0")
  expect_error(CAM16_Specification(H = 401), "Invalid H: Value must be <= 400")
})

test_that("CAM16_Specification validates types", {
  # Test numeric type validation
  expect_error(
    CAM16_Specification(J = "50"),
    "<huerd::CAM16_Specification> object properties are invalid:
- @J must be <integer> or <double>, not <character>"
  )
  expect_error(
    CAM16_Specification(C = "30"),
    "<huerd::CAM16_Specification> object properties are invalid:
- @C must be <integer> or <double>, not <character>"
  )
  expect_error(
    CAM16_Specification(h = "180"),
    "<huerd::CAM16_Specification> object properties are invalid:
- @h must be <integer> or <double>, not <character>"
  )
})

test_that("CAM16_Specification handles NA values", {
  # Test NA validation
  expect_error(
    CAM16_Specification(J = NA),
    "<huerd::CAM16_Specification> object properties are invalid:
- @J must be <integer> or <double>, not <logical>"
  )
  expect_error(
    CAM16_Specification(C = NA),
    "<huerd::CAM16_Specification> object properties are invalid:
- @C must be <integer> or <double>, not <logical>"
  )
  expect_error(
    CAM16_Specification(h = NA),
    "<huerd::CAM16_Specification> object properties are invalid:
- @h must be <integer> or <double>, not <logical>"
  )

  # Test NA in vectors
  expect_error(
    CAM16_Specification(J = c(50, NA)), "Invalid J: Value cannot be NA"
  )
})

# Test Constructor with Vectorized Inputs
test_that("CAM16_Specification constructor handles vectorized inputs", {
  multiple_colors <- CAM16_Specification(
    J = c(50, 60, 70),
    C = c(30, 40, 50),
    h = c(180, 200, 220)
  )

  # Check that a list is returned
  expect_true(is.list(multiple_colors))

  # Check the length of the list
  expect_equal(length(multiple_colors), 3)

  # Check the properties of individual objects in the list
  expect_equal(multiple_colors[[1]]@J, 50)
  expect_equal(multiple_colors[[2]]@C, 40)
  expect_equal(multiple_colors[[3]]@h, 220)

  # Check that uninitialized properties have length 0
  expect_equal(length(multiple_colors[[1]]@s), 0)
})

# Test Constructor with a Single Input
test_that("CAM16_Specification constructor handles single inputs", {
  single_color <- CAM16_Specification(J = 50, C = 30, h = 180)

  # Check that a single object is returned
  expect_false(is.list(single_color))

  # Check the properties of the object
  expect_equal(single_color@J, 50)
  expect_equal(single_color@C, 30)
  expect_equal(single_color@h, 180)

  # Check that uninitialized properties have length 0
  expect_equal(length(single_color@s), 0)
})

# Test as.list with Single Object
test_that("as.list works with a single CAM16_Specification object", {
  single_color <- CAM16_Specification(J = 50, C = 30, h = 180)
  color_list <- as.list(single_color)

  # Check the type of the output
  expect_true(is.list(color_list))

  # Check that initialized properties are present
  expect_equal(color_list$J, 50)
  expect_equal(color_list$C, 30)
  expect_equal(color_list$h, 180)

  # Check that uninitialized properties are not present
  expect_false(hasName(color_list, "s"))
  expect_false(hasName(color_list, "Q"))
  expect_false(hasName(color_list, "M"))
  expect_false(hasName(color_list, "H"))
  expect_false(hasName(color_list, "HC"))
})

# Test as.list with Multiple Objects
test_that("as.list works with multiple CAM16_Specification objects", {
  multiple_colors <- CAM16_Specification(
    J = c(50, 60, 70),
    C = c(30, 40, 50),
    h = c(180, 200, 220)
  )
  list_of_lists <- lapply(multiple_colors, as.list)

  # Check the type of the output
  expect_true(is.list(list_of_lists))

  # Check the length of the list
  expect_equal(length(list_of_lists), 3)

  # Check the properties of individual lists
  expect_equal(list_of_lists[[1]]$J, 50)
  expect_equal(list_of_lists[[2]]$C, 40)
  expect_equal(list_of_lists[[3]]$h, 220)

  # Check that uninitialized properties are not present
  expect_false(hasName(list_of_lists[[1]], "s"))
  expect_false(hasName(list_of_lists[[2]], "Q"))
  expect_false(hasName(list_of_lists[[3]], "M"))
})

# Test as.data.frame with Single Object
test_that("as.data.frame works with a single CAM16_Specification object", {
  single_color <- CAM16_Specification(J = 50, C = 30, h = 180)
  color_df <- as.data.frame(single_color)

  # Check the type of the output
  expect_true(is.data.frame(color_df))

  # Check the dimensions of the data frame
  expect_equal(nrow(color_df), 1)
  expect_equal(ncol(color_df), 3)

  # Check the values in the data frame
  expect_equal(color_df$J, 50)
  expect_equal(color_df$C, 30)
  expect_equal(color_df$h, 180)

  # Check that uninitialized properties are not present
  expect_false(hasName(color_df, "s"))
  expect_false(hasName(color_df, "Q"))
  expect_false(hasName(color_df, "M"))
  expect_false(hasName(color_df, "H"))
  expect_false(hasName(color_df, "HC"))
})

# Test as.data.frame with Multiple Objects
test_that("as.data.frame works with multiple CAM16_Specification objects", {
  multiple_colors <- CAM16_Specification(
    J = c(50, 60, 70),
    C = c(30, 40, 50),
    h = c(180, 200, 220)
  )
  color_df <- do.call(rbind, lapply(multiple_colors, as.data.frame))
  # Check the type of the output
  expect_true(is.data.frame(color_df))

  # Check the dimensions of the data frame
  expect_equal(nrow(color_df), 3)
  expect_equal(ncol(color_df), 3)

  # Check the values in the data frame
  expect_equal(color_df$J, c(50, 60, 70))
  expect_equal(color_df$C, c(30, 40, 50))
  expect_equal(color_df$h, c(180, 200, 220))

  # Check that uninitialized properties are not present
  expect_false(hasName(color_df, "s"))
  expect_false(hasName(color_df, "Q"))
  expect_false(hasName(color_df, "M"))
  expect_false(hasName(color_df, "H"))
  expect_false(hasName(color_df, "HC"))
})

# Test '==' Operator
test_that("'==' operator works correctly", {
  color1 <- CAM16_Specification(J = 50, C = 30, h = 180)
  color2 <- CAM16_Specification(J = 50, C = 30, h = 180)
  color3 <- CAM16_Specification(J = 60, C = 40, h = 200)
  color4 <- CAM16_Specification(C = 30, h = 180)

  # Test equality with initialized properties
  expect_true(color1 == color2)

  # Test inequality
  expect_false(color1 == color3)

  # Test inequality with uninitialized properties
  expect_false(color1 == color4) # Both have J, C, and h initialized, and they are equal
})

test_that("format.CAM16_Specification formats single object correctly", {
  color <- CAM16_Specification(J = 50, C = 30, h = 180, s = 85)

  expected_output <- paste0(
    "<CAM16_Specification:\n",
    "  Lightness (J) = 50,\n",
    "  Chroma (C) = 30,\n",
    "  Hue Angle (h) = 180,\n",
    "  Saturation (s) = 85\n",
    ">"
  )

  expect_equal(format(color), expected_output)
})

test_that("format.CAM16_Specification handles uninitialized properties", {
  color <- CAM16_Specification(J = 50, h = 180)

  expected_output <- paste0(
    "<CAM16_Specification:\n",
    "  Lightness (J) = 50,\n",
    "  Hue Angle (h) = 180\n",
    ">"
  )

  expect_equal(format(color), expected_output)
})
