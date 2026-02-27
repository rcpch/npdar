# get_ultimate produces appropriate warnings

    Code
      get_ultimate(c(1, 2, 3), find = "uptodate_mode")
    Condition
      Warning in `get_ultimate()`:
      NAs are excluded by default, but it's still good practice to specify the invalid values to exclude.
      Warning in `get_ultimate()`:
      There are multiple modes, the remaining modes are: 1, 2
    Output
      [1] 3

