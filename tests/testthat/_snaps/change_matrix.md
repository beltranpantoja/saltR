# Base arguments work

    Code
      change_matrix(qmatrix, fun = function(x) x * 0)
    Output
            Attr1 Attr2 Attr3
      Item1     0     0     0
      Item2     0     0     0
      Item3     0     0     0
      Item4     0     0     0
      Item5     0     0     0
      Item6     0     0     0

---

    Code
      change_matrix(qmatrix, fun = function(x) x * 0, direction = "col")
    Output
            Attr1 Attr2 Attr3
      Item1     0     0     0
      Item2     0     0     0
      Item3     0     0     0
      Item4     0     0     0
      Item5     0     0     0
      Item6     0     0     0

---

    Code
      change_matrix(qmatrix, fun = function(x) x * 0, at = 2)
    Output
            Attr1 Attr2 Attr3
      Item1     1     0     0
      Item2     0     0     0
      Item3     0     0     1
      Item4     1     1     0
      Item5     1     0     1
      Item6     0     1     1

---

    Code
      change_matrix(qmatrix, fun = function(x) x * 0, at = 2, direction = "col")
    Output
            Attr1 Attr2 Attr3
      Item1     1     0     0
      Item2     0     0     0
      Item3     0     0     1
      Item4     1     0     0
      Item5     1     0     1
      Item6     0     0     1

---

    Code
      change_matrix(qmatrix, fun = function(x) x * 0, at = 1:4)
    Output
            Attr1 Attr2 Attr3
      Item1     0     0     0
      Item2     0     0     0
      Item3     0     0     0
      Item4     0     0     0
      Item5     1     0     1
      Item6     0     1     1

---

    Code
      change_matrix(qmatrix, fun = function(x) x * 0, at = function(row) row[1] == 1)
    Output
            Attr1 Attr2 Attr3
      Item1     0     0     0
      Item2     0     1     0
      Item3     0     0     1
      Item4     0     0     0
      Item5     0     0     0
      Item6     0     1     1

# Utility functions work

    Code
      change_matrix(test, fun = function(x) x * 0, at = mat_is_complex(2))
    Output
             0  1  2  3 1-2 1-3 2-3 1-2-3
      Item1 -2  2 NA NA  NA  NA  NA    NA
      Item2 -2 NA  2 NA  NA  NA  NA    NA
      Item3 -2 NA NA  2  NA  NA  NA    NA
      Item4  0  0  0 NA   0  NA  NA    NA
      Item5  0  0 NA  0  NA   0  NA    NA
      Item6  0 NA  0  0  NA  NA   0    NA

---

    Code
      change_matrix(qmatrix, fun = function(x) x * 0, at = mat_is_complex(1, 2))
    Output
            Attr1 Attr2 Attr3
      Item1     0     0     0
      Item2     0     0     0
      Item3     0     0     0
      Item4     0     0     0
      Item5     0     0     0
      Item6     0     0     0

---

    Code
      change_matrix(qmatrix, fun = function(x) x * 0, at = mat_measures_attr(1))
    Output
            Attr1 Attr2 Attr3
      Item1     0     0     0
      Item2     0     1     0
      Item3     0     0     1
      Item4     0     0     0
      Item5     0     0     0
      Item6     0     1     1

---

    Code
      change_matrix(test, fun = function(x) x * 0, at = mat_measures_attr(1))
    Output
             0  1  2  3 1-2 1-3 2-3 1-2-3
      Item1  0  0 NA NA  NA  NA  NA    NA
      Item2 -2 NA  2 NA  NA  NA  NA    NA
      Item3 -2 NA NA  2  NA  NA  NA    NA
      Item4  0  0  0 NA   0  NA  NA    NA
      Item5  0  0 NA  0  NA   0  NA    NA
      Item6 -2 NA  2  2  NA  NA 0.5    NA

