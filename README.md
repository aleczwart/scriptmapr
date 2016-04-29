R Package **scriptmapr**
=======================


Package **scriptmapr** provides a function `scriptmap`, which uses
`getParseData(parse())` on an R script file to extract the locations
of symbols (object names) in the script.  It then plots these
locations for each object name, providing a visual display of the
locations of symbols in a script.

This 'scriptmap' plot can be helpful when developing a script
analysing multiple responses - in these situations, one often adopts a
'copy/paste/edit' cycle, re-using code from the previous analysis on
the current one.  With such a workflow, there is a risk of neglecting
to update an object name to reflect the current response analysed,
potentially leading to runtime errors, or (much worse) misleading
results.

The 'scriptmap' provides a simple way to checking for symbol names
that are out of place, in the script file.  *scriptmap* should NOT be
regarded as provided a definitive tool for finding out-of-place names
in a file!  The author accepts no reposibility for failure to find
errors in scripts using `scriptmapr` and `scriptmap`.

Note that according to the data provided by `getParseData`, the
'location' of a symbol is defined as the line number in the script of
the *first line of the statement containing the object*. For a
multi-line statement, this may not be the line that the symbol appears
in.

R function `parse` will generate an error if there is a syntax error
in the code, so `scriptmap` will fail to produce a plot in this case.

Function `scriptmap` produces a `ggplot` object, using the **ggplot2**
package.  You can therefore modify aspects of the plot using
**ggplot2** functions like `xlim` and `theme` via the usual ggplot2
syntax.  If using scriptmap inside a loop or function, remember that
you will need to explicitly `print` the ggplot object before it will
display.

Function `scriptmap` has three main arguments:

+ `file` must specify the path to an R script file - there should be
  no R syntax errors within the script file.
+ `tokenType` is a character vector specifying the 'token' types to be
  plotted - sensible choices for this are `"SYMBOL"` (to plot the
  locations of data object names-this is the default) and/or
  `"SYMBOL_FUNCTION_CALL"` (to plot the locations of function calls).
  See help for `getParseData` and examples of `getParseData` output for
  further details.
+ `rgx` is a character vector of regular expression strings, to be
  used to select only a subset of the symbols in the script for
  plotting via R function `grepl`.  When more than one regex is
  provided in `rgx`, the symbols plotted are the combination of the
  symbols matched by each regex.

CAVEAT An important limitation of scriptmap is that character strings
within the R script are NOT currently searched by `scriptmap`.  So
take extra care to 'manually' check object names passed as/in
character strings to functions.
