
## AW notes that one can get rid of the NOTE's concerning "no visible
## binding for global variable' using functions filter_, arrange_, etc
## in dplyr, and using aes_string instead of aes in ggplot2

##require(dplyr)
##require(ggplot2)

## Note that in pd, line1 is where the item starts, line2 is where it
## ends.  Hence I expect that for most simple tokens, line1 = line2,
## for expressions line1 may not equal line2:


## TODO - what happens if the rgx vector returns no matches?  What
## should happen?
##
## Function condMultiFilter
##
##' Filter (subset) a data frame by a vector \code{rgx} of regular
##' expressions applied to a specified column of the data frame.  The
##' filter is essentially '\code{rgx[1] | rgx[2] | ... rgx[N]}'.
##' Filtering is conditional, in that if the argument \code{rgx} is
##' \code{NULL}, the dataframe is returned unchanged.
##'
##' @title Conditionally filter a data frame by multiple regex's
##' @param dfr the data frame to be filtered/subsetted
##' @param col character: the name of the column of the data frame to
##' which filtering is to be applied.
##' @param rgx a character vector of regular expressions.  Remember
##' that the character strings are parsed by the R interpreter before
##' being parsed by the regular expression parser, so backslashes must
##' be 'doubled up'.
##' @param ... other arguments to be passed to \code{\link{grepl}}.
##' @return A data frame.
##' @author Alexander Zwart (alec.zwart at csiro.au)
##' @export
##'
condMultiFilter <- function(dfr,col,rgx,...)
  {
    ## Two options - preserve the order in dp, or order by rgx order.
    ## For now, preserve the order in dp...
    stopifnot(is.data.frame(dfr))
    stopifnot(is.null(rgx) || is.character(rgx))
    stopifnot(is.character(col))
    stopifnot(length(col)==1)
    if (is.null(rgx)) {
      return(dfr)
    } else {
      ll <- rep(FALSE,dim(dfr)[1])
      for (rx in rgx) {
        ll <- ll | grepl(rx,dfr[[col]],...)
      }
      return(dplyr::filter(dfr,ll))
    }
  }


## TODO dealing with finding object names inside character strings.
## Implement functionality to find object name sequences inside
## character strings.  Also, implement the ability to specify object
## names (or just text strings) to search for explicitly (via a
## fixed=TRUE grepl search).  Then, advise the user that if copying in
## code from another file (with response names that are different to
## those analysed in the file being mapped), to keep a record of the
## responses, and search for those responses in the text strings
## explicitly...  specification of explicit text strings to search
## for, and use of rgx= to filter the object list should be mutually
## exclusive functionalities.



## TODO function calls (SYMBOL_FUNCTION_CALL) are not included in
## SYMBOLs.  Need to decide how to handle function calls...  Probably
## less important to worry aboou them by default, but there could be
## some situations where mapping 'em would be useful...


## TODO need to describe the tokenType argument in more detail below.

## TODO need to describe the rgx argument in more detail below.

## TODO Add a 'tokens' argument for explicit specification of tokens
## to plot?  A more specific alternative to rgx?  rgx and tokens
## cannot both be specified at the same time?

## TODO check on use of backslashes in rgx expressions - do I need to
## double backslash?  If so, document this

## Function scriptmap
##
##' Function \code{scriptmap} uses \code{\link{parse}} and
##' \code{\link{getParseData}} to identify symbols (i.e. names)
##' representing R objects in an R script and plots their locations
##' within the script.
##'
##' When analysing multiple reponses in a single script file, the
##' workflow invariably involves a lot of copy/paste/edit'ing of code,
##' and this leads to the possibility of errors or oversights in the
##' editing of code.  Function \code{scriptmap} provides a graphical
##' overview of where object names ('tokens') appear in a script file,
##' so that names that appear out of place may be more easily noticed.
##' Note that \code{scriptmap} is intended as an \emph{aid} to
##' spotting name-out-of-place coding errors, but should NOT be
##' regarded as a tool for definitively identifying all such errors -
##' see the Caveat below.
##'
##' Function \code{scriptmap} uses \code{\link{parse}} and
##' \code{\link{getParseData}} to identify symbols (i.e. names)
##' representing R objects in an R script and plots their locations
##' within the script.  The 'location' of a symbol is defined as the
##' line number (in the R script) of the \emph{beginning of the
##' statement containing the symbol} (and hence may not be the line
##' actually containing the symbol, in a multi-line statement).  This
##' limitation is a consequence of the information provided by
##' \code{getParseData}.
##'
##' The option \code{sortTokens} is provided to specify that the order
##' of the tokens on the y-axis is to be sorted according to the line
##' number at which each token first appears in the script. Such a
##' sort order may aid in spotting name-out-of-place errors. The
##' default is \code{sortTokens=FALSE}.
##'
##' Arguments \code{lmin} and \code{lmax} can be specified to restrict
##' the line range of tokens to be plotted.  Note that \code{lmin} and
##' \code{lmax} do not directly restrict the range of the plot (which
##' can be achieved by the \pkg{dplyr} \code{xlim} function). Rather,
##' these arguments remove token occurrences lying outside the
##' \code{lmin:lmax} range prior to sorting the tokens and
##' constructing the plot.  Hence they (\code{lmin} particularly) can
##' impact on the sort order used when \code{sortTokens=TRUE}.
##'
##' Caveat: A limitation of \code{scriptmap} is that it only works
##' with object names that are recognised by the parsing of the R
##' script via \code{\link{parse}}.  Character strings in a script are
##' not parsed for R object names by \code{\link{parse}}, so new or
##' misspelled object names that appear only in character strings will
##' not be plotted by \code{scriptmap}.  So do take extra care to
##' check, for example, character arguments to functions.
##'
##' @title Plot locations of object references in an R script.
##' @param file character: the path to the R script file to be
##' 'mapped'.
##' @param tokenType Choice of \code{\link{getParseData}} token type
##' to plot. Default value "SYMBOL".
##' @param rgx character: a vector of regular expressions, to be used
##' to filter the set of symbols to be plotted.  Default is
##' \code{NULL}, implying no filtering.
##' @param lmin integer: minimum token line number used to produce the
##' plot.  This argument doesn't simply adjust the range of the plot
##' (which can be done via the \pkg{ggplot2} \code{xlim} function) -
##' it removes occurances of tokens having line number below
##' \code{lmin} prior to sorting the data ( see the \code{sortTokens}
##' argument below.
##' @param lmax integer: maximum token line number used to produce the
##' plot.
##' @param sortTokens logical: if TRUE, the order of the tokens on the
##' plot y axis is sorted by the minimum line number in which the
##' token appears.  Default is FALSE.
##' @param ... other arguments to be passed to \code{\link{grepl}}.
##' @return A \code{ggplot} object.
##' @author Alexander Zwart (alec.zwart at csiro.au)
##' @export
##'
scriptmap <- function(file,tokenType="SYMBOL",rgx=NULL,
                      lmin=NULL,lmax=NULL,sortTokens=FALSE,...)
  {
    stopifnot(is.character(file))
    stopifnot(length(file)==1)
    stopifnot(is.character(tokenType))
    ##
    if (file.exists(file)) {
      doc <- readLines(file)
    } else {
      stop(paste("\n  File",file,"not found\n"))
    }
    pd <- getParseData(parse(text=doc,keep.source=TRUE))
    ##
    pd  <- dplyr::filter(pd,token %in% tokenType)
    pd <- condMultiFilter(pd,col="text",rgx=rgx)
    if (!is.null(lmin))
      {
        stopifnot(length(lmin)==1)
        stopifnot(is.numeric(lmin) & lmin > 0 & round(lmin) == lmin)
        pd <- dplyr::filter(pd,line1 >= lmin)
      }
    if (!is.null(lmax))
      {
        stopifnot(length(lmax)==1)
        stopifnot(is.numeric(lmax) & lmax > 0 & round(lmax) == lmax)
        pd <- dplyr::filter(pd,line1 <= lmax)
      }
    if (sortTokens)
      {    ## Sorting should be done AFTER lmin, lmax are applied.
        dd <- dplyr::group_by(pd,text)
        dd <- dplyr::summarize(dd,
                               lrmin = min(line1),
                               lrmax = max(line1))
        dd <- dplyr::arrange(dd,lrmin,lrmax)
        pd$text <- factor(pd$text, levels=dd$text)
      }
    ggplot(pd,aes(x=line1,y=text)) + geom_point() +
     xlab("Script line number") + ylab("")
  }

## scriptmap("../../devel/BaseModels.R",rgx=NULL)
## scriptmap("../../devel/BaseModels.R",rgx=NULL) + xlim(2500,6000)
## scriptmap("../../devel/BaseModels.R",rgx="^aerialct") + xlim(2500,6000)
## scriptmap("../../devel/BaseModels.R",rgx=c("^aerialct","^MVQTL","^QTLSumm")) + xlim(2500,6000)
##
## scriptmap("../../devel/BaseModels.R",tokenType="SYMBOL_FUNCTION_CALL",
##           rgx=NULL)
##
## scriptmap("../../devel/BaseModels.R",
##           tokenType=c("SYMBOL","SYMBOL_FUNCTION_CALL"),
##           rgx=NULL) + xlim(2500,6000)



## Re: Regarding fixed string searches: Need a function that:
## 1. Searches for the longest symbol name(s) first.  2. Where found,
## extracts the matching line numbers and then removes (all instances)
## of the symbol names from the affected strings 3. Moves on to search
## for the next longest symbol name(s), and so-on.  Call this the
## 'conservative' search.


## Need stuff to decide upon symbols to plot.  Options:
##
## 1. default: All valid symbols in the parse object.
##
## 2. character vector - a vector of symbols to plot - check against
## parse object symbols, drop symbols not present in parse object
## symbols, and warn user.  Drop non-unique symbols and warn. Make
## sure that plot relfects the order specified by the user specified
## symbols vector.  Can I use check.names() or related also for
## validity checks on user specified symbols before comparing to parse
## symbols?  Probably unnecessary...
##
## For anything else, suggest having a little function to return all
## of the valid (unique) symbols in the parse object, then the user
## can do some manual subsetting/sorting of this vector as desired.
## It's going to get too complicated trying to pass subsetting
## operations through to the scriptmapr function.
##
## Return a ggplot2 object - allowing the user to further modify...



## TODO - does this include operators? `+`, %>%? etc - check.  I think there may be an OPERATOR token type?
##
## Given an R script file containing valid R code, extract the names
## of all functions that are called in the code.
getFunctionsCalled <- function(file)
  {
    stopifnot(is.character(file))
    stopifnot(length(file)==1)
    if (file.exists(file)) {
      pd  <- getParseData(parse(file=file,keep.source=TRUE))
      return(unique(pd$text[pd$token=="SYMBOL_FUNCTION_CALL"]))
    } else {
      stop(paste("\n  File",file,"not found\n"))
    }
  }


## Given an R script file containing valid R code, extract all object
## symbols in the code. (The symbols are valid R object names,
## including names of functions that are treated as \emph{data}, but
## not names of functions that are \emph{called}. To extract symbols
## in the code associated with function \emph{calls}, use
## getFunctionsCalled()).
##
getSymbols <- function(file)
  {
    stopifnot(is.character(file))
    stopifnot(length(file)==1)
    if (file.exists(file)) {
      pd  <- getParseData(parse(file=file,keep.source=TRUE))
      return(unique(pd$text[pd$token=="SYMBOL"]))
    } else {
      stop(paste("\n  File",file,"not found\n"))
    }
  }

## getSymbols("../../devel/BaseModels.R")

## getFunctions("../../devel/BaseModels.R")



