# domR

**PRIVATE BRANCH OF THE pkg01_domR PACKAGE**

## Overview
The domR package is a series of personal functions that I use in my own project.
These are pretty fundamental functions, and follow my own personal logic of using R.

## Public Version
The public version of this repo can be accessed at: 
  https://github.com/polarSaunderson/domR

## To-Do
### 2023-09-05
- [ ] Change add_metadata; it should probably be split into get_metadata, and 
      then add_metadata, which formats it; choice of where it is going (e.g.
      as PDF keywords via exifTools, as NetCDF atts, or as part of json / yaml).

### 2023-09-04
- [ ] Figure out lists and saving data in relation to version control 
- [ ] Unify the 'cat' functions - it should handle if a vector goes to cat_list,
      or a list goes to cat2. But bring these together, so there isn't a wrong 
      function to go to. Probably keep a cat_list / cat2 combined, and then also
      a cat4 with the location & traceback part. cat3... hmmmm.

### 2023-09-03
- [ ] In `which_line()`, figure out identity within chunk of existing notebook
- [ ] Rename `cat` functions? Currently, they are:
    - cat2 : name & variable
    - cat3 : x with \n
    - cat4 : name, variable & location / traceback
- [x] ASCII decision on cat_line
- [?] Check doesn't like undefined "params" or "whichLine_info" 

### 2023-07-30 
- [X] Consider the codes in `now()`
- [X] Add a good example to `set_if_null()` - see `figuR::add_axis()`
- [ ] Test `create_lean_bib()` with phd02

