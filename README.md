# CIECAM16 - R Implementation

An R package implementation of the CIECAM16 color appearance model.

## Installation

```r
# Install from GitHub
devtools::install_github("sims1253/ciecam16")
```

## Usage

```r
library(ciecam16)

  color1 <- CAM16_Specification(J = 50, C = 30, h = 180, H = 200, e = 0.9)
  color2 <- CAM16_Specification(J = 60, C = 40, h = 200, H = 220, e = 0.8)
  color1 == color2
  as.list(color1)
  as.data.frame(color2)
```

## References

1. CIE 224:2017 - CIE Color Appearance Model for Color Management Systems: CIECAM16
2. Li, C., Li, Z., Wang, Z., et al. (2017). Comprehensive color solutions: CAM16, CAT16, and CAM16-UCS. Color Research & Application, 42(6), 703-718.
3. Luo, M. R. (2016). CIECAM02 and its recent developments. In Advanced Color Image Processing and Analysis (pp. 19-58). Springer.

