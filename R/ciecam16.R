#' CIECAM16 Color Appearance Model
#'
#' @description
#' Implementation of CIECAM16 color appearance model for color space conversions and
#' color difference calculations. Supports:
#' - Transformations between CIE XYZ and CIECAM16
#' - Color difference calculations in CAM16-UCS space
#' - Multiple viewing conditions
#'
#' @references
#' CIE 248:2022. The CIE 2016 Colour Appearance Model for Colour Management Systems:
#' CIECAM16. Vienna: CIE Central Bureau.
#'
#' @name CIECAM16
NULL

#' CAM16 Color Specification
#'
#' @description
#' Represents colors in CIECAM16 color appearance model space with perceptual attributes.
#' Handles both single values and vectors of color properties.
#'
#' @slot J Lightness (0-100)
#' @slot C Chroma (0-100)
#' @slot h Hue angle in degrees (0-360)
#' @slot s Saturation (0-100)
#' @slot Q Brightness (0-100)
#' @slot M Colorfulness (0-100)
#' @slot H Hue quadrature (0-400)
#' @slot e Eccentricity factor (0-1)
#'
#' @param J,C,h,s,Q,M,H,e Numeric vectors or single values for respective properties
#' @return `CAM16_Specification` object or list of objects for vector inputs
#'
#' @import S7
#' @export
#' @examples
#' # Single color
#' CAM16_Specification(J = 50, C = 30, h = 180)
#'
#' # Multiple colors
#' CAM16_Specification(J = c(50, 60), C = c(30, 40), h = c(180, 200))
CAM16_Specification <- S7::new_class(
  "CAM16_Specification",
  properties = list(
    J = S7::new_property(class = S7::class_numeric), # Lightness
    C = S7::new_property(class = S7::class_numeric), # Chroma
    h = S7::new_property(class = S7::class_numeric), # Hue Angle
    s = S7::new_property(class = S7::class_numeric), # Saturation
    Q = S7::new_property(class = S7::class_numeric), # Brightness
    M = S7::new_property(class = S7::class_numeric), # Colorfulness
    H = S7::new_property(class = S7::class_numeric), # Hue Quadrature
    e = S7::new_property(class = S7::class_numeric) # Eccentricity Factor
  ),
  constructor = function(J = numeric(),
                         C = numeric(),
                         h = numeric(),
                         s = numeric(),
                         Q = numeric(),
                         M = numeric(),
                         H = numeric(),
                         e = numeric()) {
    # Find maximum length of input vectors
    max_len <- max(
      length(J),
      length(C),
      length(h),
      length(s),
      length(Q),
      length(M),
      length(H),
      length(e)
    )

    # Single object case (all arguments have length 0 or 1)
    if (max_len <= 1) {
      # Create a new object and set properties
      obj <- new_object(
        .parent = CAM16_Specification,
        J = J,
        C = C,
        h = h,
        s = s,
        Q = Q,
        M = M,
        H = H,
        e = e
      )
      return(obj)
    } else {
      # Vectorized case

      # Create a list of objects
      results <- vector(mode = "list", length = max_len)
      for (i in seq_len(max_len)) {
        results[[i]] <- new_object(
          .parent = CAM16_Specification,
          J = if (length(J) > 0) {
            J[i]
          } else {
            J # Use the default empty vector
          },
          C = if (length(C) > 0) {
            C[i]
          } else {
            C # Use the default empty vector
          },
          h = if (length(h) > 0) {
            h[i]
          } else {
            h # Use the default empty vector
          },
          s = if (length(s) > 0) {
            s[i]
          } else {
            s # Use the default empty vector
          },
          Q = if (length(Q) > 0) {
            Q[i]
          } else {
            Q # Use the default empty vector
          },
          M = if (length(M) > 0) {
            M[i]
          } else {
            M # Use the default empty vector
          },
          H = if (length(H) > 0) {
            H[i]
          } else {
            H # Use the default empty vector
          },
          e = if (length(e) > 0) {
            e[i]
          } else {
            e # Use the default empty vector
          }
        )
      }
      return(results)
    }
  },
  # Validator (simplified since HC is removed)
  validator = function(self) {
    errors <- character()

    # Helper function for numeric range validation
    validate_range <- function(value, name, min = NULL, max = NULL) {
      if (!is.null(value)) {
        if (any(is.na(value))) {
          return(sprintf("Invalid %s: Value cannot be NA", name))
        }
        if (!is.null(min) && any(value < min)) {
          return(sprintf("Invalid %s: Value must be >= %s", name, min))
        }
        if (!is.null(max) && any(value > max)) {
          return(sprintf("Invalid %s: Value must be <= %s", name, max))
        }
      }
      return(NULL)
    }

    # Validate numeric ranges
    errors <- c(
      errors,
      validate_range(self@J, "J", 0, 100),
      validate_range(self@C, "C", 0, 100),
      validate_range(self@h, "h", 0, 360),
      validate_range(self@s, "s", 0, 100),
      validate_range(self@Q, "Q", 0, 100),
      validate_range(self@M, "M", 0, 100),
      validate_range(self@H, "H", 0, 400),
      validate_range(self@e, "e", 0, 1)
    )

    # Return the errors
    errors <- errors[!sapply(errors, is.null)]
    if (length(errors) > 0) {
      return(errors)
    }

    return(NULL)
  }
)



#' @export
local({
  S7::method(as.list, CAM16_Specification) <- function(x, ...) {
    result <- list()
    if (length(x@J) > 0) result$J <- x@J
    if (length(x@C) > 0) result$C <- x@C
    if (length(x@h) > 0) result$h <- x@h
    if (length(x@s) > 0) result$s <- x@s
    if (length(x@Q) > 0) result$Q <- x@Q
    if (length(x@M) > 0) result$M <- x@M
    if (length(x@H) > 0) result$H <- x@H
    if (length(x@e) > 0) result$e <- x@e
    result
  }

  S7::method(as.data.frame, CAM16_Specification) <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...) {
    if (is.list(x)) {
      # Handle list of objects
      df_list <- lapply(x, function(obj) {
        as.data.frame(as.list(obj))
      })
      do.call(rbind, df_list)
    } else {
      # Handle single object
      as.data.frame(as.list(x))
    }
  }


  S7::method(format, CAM16_Specification) <- function(x, ...) {
    # Create a string representation of the CAM16_Specification object
    # Only include properties that have values (length > 0)

    parts <- character()
    if (length(x@J) > 0) {
      parts <- c(parts, paste0("  Lightness (J) = ", x@J))
    }
    if (length(x@C) > 0) {
      parts <- c(parts, paste0("  Chroma (C) = ", x@C))
    }
    if (length(x@h) > 0) {
      parts <- c(parts, paste0("  Hue Angle (h) = ", x@h))
    }
    if (length(x@s) > 0) {
      parts <- c(parts, paste0("  Saturation (s) = ", x@s))
    }
    if (length(x@Q) > 0) {
      parts <- c(parts, paste0("  Brightness (Q) = ", x@Q))
    }
    if (length(x@M) > 0) {
      parts <- c(parts, paste0("  Colorfulness (M) = ", x@M))
    }
    if (length(x@H) > 0) {
      parts <- c(parts, paste0("  Hue Quadrature (H) = ", x@H))
    }
    if (length(x@e) > 0) {
      parts <- c(parts, paste0("  Eccentricity Factor (e) = ", x@e))
    }

    # Combine the parts into a single string with newlines
    paste0(
      "<CAM16_Specification:\n",
      paste(parts, collapse = ",\n"),
      "\n>"
    )
  }

  S7::method(print, CAM16_Specification) <- function(x, ...) {
    if (is.list(x)) {
      # Handle list of objects
      for (i in seq_along(x)) {
        cat("[[", i, "]]\n", sep = "")
        cat(format(x[[i]]), "\n\n") # Add extra newline between list elements
      }
    } else {
      # Handle single object
      cat(format(x), "\n")
    }
  }

  S7::method(`==`, list(CAM16_Specification, CAM16_Specification)) <- function(e1, e2) {
    # Check if all corresponding properties are equal (or both empty)
    all(
      (length(e1@J) > 0 && length(e2@J) > 0 && e1@J == e2@J) || (length(e1@J) == 0 && length(e2@J) == 0),
      (length(e1@C) > 0 && length(e2@C) > 0 && e1@C == e2@C) || (length(e1@C) == 0 && length(e2@C) == 0),
      (length(e1@h) > 0 && length(e2@h) > 0 && e1@h == e2@h) || (length(e1@h) == 0 && length(e2@h) == 0),
      (length(e1@s) > 0 && length(e2@s) > 0 && e1@s == e2@s) || (length(e1@s) == 0 && length(e2@s) == 0),
      (length(e1@Q) > 0 && length(e2@Q) > 0 && e1@Q == e2@Q) || (length(e1@Q) == 0 && length(e2@Q) == 0),
      (length(e1@M) > 0 && length(e2@M) > 0 && e1@M == e2@M) || (length(e1@M) == 0 && length(e2@M) == 0),
      (length(e1@H) > 0 && length(e2@H) > 0 && e1@H == e2@H) || (length(e1@H) == 0 && length(e2@H) == 0),
      (length(e1@e) > 0 && length(e2@e) > 0 && e1@e == e2@e) || (length(e1@e) == 0 && length(e2@e) == 0)
    )
  }

})
