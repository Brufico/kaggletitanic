#' ---
#' title: 'Titanic - Kaggle - Some exploration'
#' subtitle: "R code for general preparation, called by Titanic_Explore"
#' author:  "Bruno Fischer Colonimos"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---




#
#  Code for adjusting ggplot2 theme
#  =================================


if (bwtheme) {
        theme_set(theme_bw())
}

if (specialpalette) {
        # palette color-blind-friendly: The palette with black (Cookbook for R,
        # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)
        cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

        # modified BFC
        cbfPalette <- cbbPalette
        cbfPalette[3] <- cbbPalette[7]
        cbfPalette[6] <- cbbPalette[3]
        cbfPalette[8] <- cbbPalette[7]

        # * `set_default_scale` has been removed. If you want to change the default
        # scale for an aesthetic, just create a function called
        # `scale_aesthetic_continuous` or `scale_aesthetic_discrete` that returns the
        # scale that you want.

        scale_colour_discrete <- function(...) {
                scale_colour_manual(values = cbbPalette, ...)
        }

        scale_fill_discrete <- function(...) {
                scale_fill_manual(values = cbbPalette, ...)
        }

        scale_colour_continuous <- function(...) {
                ggplot2::scale_colour_brewer("Greys", ...)
        }

}


#######################################################
# Identify context: output type ***********************
# ====================================================

# From Hadley's ggplot2 book:  Knowing conversion target
is_latex <- function() {
        identical(knitr::opts_knit$get("rmarkdown.pandoc.to"), "latex")
}

# html same
is_html <- function() {
        identical(knitr::opts_knit$get("rmarkdown.pandoc.to"), "html")
}

pdfoutput <- is_latex() # tells if we are converting through latex, ie to pdf

# getcontext returns the doc conversion context
getcontext <- function(){
        if (is_latex()) {"latex"
        } else if (is_html()) {
                "html"
        } else {"other"}
}


# get the code chunk name (for captionning)

chunkname <- function() {knitr::opts_current$get("label")}



#######################################################
# Captioning / cross-referencing ***********************
# =====================================================

# Table captions
# tabcap <- function(caption,
#                    chunklabel=knitr::opts_current$get("label"),
#                    context=getcontext()) {
#
#         switch(context,
#                latex = paste0("\\label{tab:", chunklabel, "}",
#                               caption),
#                html = paste0("<div id=tab:", chunklabel, ">",
#                              caption, "</div>"),
#                other = caption )
# }


tabcap <- function(caption,
                   chunklabel=knitr::opts_current$get("label"),
                   context=getcontext()) {

        switch(context,
               latex = paste0("\\label{tab:", chunklabel, "}",
                              caption),
               html = paste0("<a name=tab:", chunklabel, ">",
                             caption, "</a>"),
               other = caption )
}






# figure captions ?? ==> No, not needed with pdf_output
figcap <- function(caption, chunklabel=knitr::opts_current$get("label")) {
        # debug
        # thislabel <<- chunklabel
        paste0("\\label{fig:", chunklabel, "}", caption)
}

# tests

# tabcap("one", "thzchunk", context = "latex")
# tabcap("one", "thzchunk", context = "html")


# inserting a reference to a label, using the context

.ref <- function(prefix = NULL,
                 reflabel = "",
                 context = getcontext()) {
        switch(EXPR = context,
               latex = paste0("\\ref{",prefix, reflabel,"}"),
               html = paste0("<A HREF=\\#",
                             prefix, reflabel,
                             "\\>", reflabel , "</A>"), # reflabel here ?
               other = paste0("(#",prefix, reflabel ,")")
        )
}

# .ref <- function(prefix = NULL,
#                  reflabel = "",
#                  context = getcontext()) {
#         switch(EXPR = context,
#                latex = paste0("\\ref{",prefix, reflabel,"}"),
#                html = paste0("<a href= #",
#                               prefix, reflabel,
#                               ">", reflabel , "</a>"), # reflabel here ?
#                other = paste0("(#",prefix, reflabel ,")")
#         )
# }






# testit

# .ref(prefix= "tab:",reflabel = "onelab", context =  "latex" )
# .ref(prefix= "fig:", reflabel = "onelab", context = "html" )
# .ref(reflabel = "onelab", context = "html" )
# .ref(prefix= "fig:", reflabel = "onelab", context = "other" )




#######################################################
# figures dimensions parameters ***********************
# =====================================================

#   aspect ratio
a.13 <- 1 / 3
a.12 <- 0.5
a.34 <- 0.75
a.11 <- 1
a.43 <- 1.33
a.21 <- 2

#  width
# == doc line width in inches * security coeff 0.98

doclinewidth <- (21 - 1.5 - 1.5) * 0.3937 * 0.98  # légère correction

w.11 <-  doclinewidth
w.34 <- 3 / 4 * doclinewidth
w.23 <- 2 / 3 * doclinewidth
w.12  <-  1 / 2 * doclinewidth
w.13 <- 1 / 3 * doclinewidth
w.14 <- 1 / 4 * doclinewidth * 0.99 # de nouveau: légère correction

# #   output width (only pdf)
# one3outwidth <- if (pdfoutput) {"0.32\\textwidth"} else {NULL}
# halfoutwidth <- if (pdfoutput) {"0.48\\textwidth"} else {NULL}
# two3outwidth <- if (pdfoutput) {"0.64\\textwidth"} else {NULL}
# fulloutwidth <- if (pdfoutput) {"\\textwidth"} else {NULL}

# configs pr?d?finies knitr::opts_template$set()

.codeR <- paste0( "knitr::opts_template$set(",
                  if (pdfoutput) {paste0(".f14 = list(fig.width = ",
                                         w.14,
                                         ", fig.asp = ",
                                         a.12, "), ")
                  } else { # w.14 too narrow to render ok in html
                          paste0(".f14 = list(fig.width = ",
                                 w.13,
                                 ", fig.asp = ",
                                 a.34, "), ")

                  }  ,
                  "f13 = list(fig.width = ", w.13, ", fig.asp = ", a.34, "), ",
                  "f12 = list(fig.width = ", w.12, ", fig.asp = ", a.34, "), ",
                  "fnorm = list(fig.width = ", w.23, ", fig.asp = ", a.34, "), ",
                  "fnormsq = list(fig.width = ", w.23, ", fig.asp = ", a.11, "), ",
                  "fnormhigh = list(fig.width = ", w.23, ", fig.asp = ", a.43, "), ",
                  "fnormvhigh = list(fig.width = ", w.23, ", fig.asp = ", a.21, "), ",
                  "fulllow = list(fig.width = ", w.11, ", fig.asp = ", a.34, "),",
                  "full = list(fig.width = ", w.11, ", fig.asp = ", a.34, "),",
                  "fullsq = list(fig.width = ", w.11, ", fig.asp = ", a.11, "),",
                  "fullhigh = list(fig.width = ", w.11, ", fig.asp = ", a.43, "),",
                  "fullvhigh = list(fig.width = ", w.11, ", fig.asp = ", a.21, ")",
                  ")"
)


eval(parse(text = .codeR))


# verif
# knitr::opts_template$get()
# knitr::opts_template$get("f13")
