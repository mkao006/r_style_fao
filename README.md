# R programming guidelines

This is the repository for the R programming guideline of the Food and
Agricultural Organization of the United Nations.


## Three Golden Rule

1. Have meaningful names for your `varable`, `function`, and `script`
2. Keep the functions simple and compact. Do one thing and do it well. (rule
   of thumb, do not exceed 100 lines).
3. Wrap your code, do not exeed 80~100 characters. Indent your code
   both 2 or 4 space are fine.

## Writing R codes

### Style

Unlike other languages, there is no one single style for programming
in R. We list the important elements while refer the user to the
following suggested materials.


[Google's R Style
Guide](http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml)

[Hadley Wickham's R style](http://adv-r.had.co.nz/Style.html)

[Paul Johnson's R
style](http://cran.r-project.org/web/packages/rockchalk/vignettes/Rstyle.pdf)

[Henrik Bengtsson's R style](https://docs.google.com/document/d/1esDVxyWvH8AsX-VJa-8oqWaHLs4stGlIbk8kLc5VlII/edit)

### Documentation

Documentation is a daunting yet necessary task, thankfully there are
simple documentation system in R using the `roxygen2`.

A simple use of the roxygen declaration can be found in the
`hello_world.R` under the `functions` folder. The script starts with
the declaration describing the function and the parameters of the
function.

[Roxygen: Simple documentation for
R](https://github.com/yihui/roxygen2)


### Explanation and comment

In order for other users to quickly understand and debug your code if
necessary, comment are necessary.

A simple description such as **Perform data manipulation** or
**Performing linear regression for prediction** should suffice. 

The purpose is to tell the user what task is performed at each step.
This allows people to skim through the script to understand the
purpose and the logic of the analysis and program.



## Managing scripts

Organization of project is another crucial element in helping people
understanding your work.


### Functions

Each function should be in a seperate script, the function should be
compact and efficient and contained within a single folder.

> A function should do only one thing, and do it well - Curly's Law


It is generally better to split up the function, each with their own
documentation and do just one thing. You can then have a wrapper
function to wrap everything up if you would like.


### Analysis

This repository illustrates how I usually organize my work which may
be very restrictive. However the important point to note is that there
is a logical structure.


## Building R packages

[A good guideline from Leek group
policy](https://github.com/jtleek/rpackages)