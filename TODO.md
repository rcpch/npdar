# Planned Improvements

## get_ultimate()

- [ ] Allow vector with POSIXct Date Time values

## mask_numerators()

- [ ] for rows with (2, 3, 3, 3, 3) with threshold being <3, all values are currently masked.
- [ ] instead, new rules can be implemented in the future:
  1) randomly mask only one of the second smallest value if specify set.seed(),
  2) can give weights to different options, so a certain option may be more likely to be masked


## BP warpper functions

- [ ] Check unit testing.

- [ ] Add new references apart from Fourth Report and NICE/BHF.

## AOB

- [ ] Better documentation and reference.

- [ ] Add get_audit() function