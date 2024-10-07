\name{ui}
\alias{ui}
\docType{data}
\title{
A Capitalized Title for the Data Set
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("ui")}
\format{
  The format is:
List of 4
 $ :List of 10
  ..$ name      : chr "jquery"
  ..$ version   : chr "3.6.0"
  ..$ src       :List of 1
  .. ..$ file: chr "www/shared"
  ..$ meta      : NULL
  ..$ script    : chr "jquery.min.js"
  ..$ stylesheet: NULL
  ..$ head      : NULL
  ..$ attachment: NULL
  ..$ package   : chr "shiny"
  ..$ all_files : logi FALSE
  ..- attr(*, "class")= chr "html_dependency"
 $ : NULL
 $ : NULL
 $ :List of 1
  ..$ :List of 3
  .. ..$ name    : chr "div"
  .. ..$ attribs :List of 1
  .. .. ..$ class: chr "container-fluid"
  .. ..$ children:List of 3
  .. .. ..$ :List of 2
  .. .. .. ..$ :List of 3
  .. .. .. .. ..$ name    : chr "head"
  .. .. .. .. ..$ attribs : Named list()
  .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. ..$ name    : chr "title"
  .. .. .. .. .. .. ..$ attribs : Named list()
  .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. ..$ : chr "Kolada"
  .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. ..$ :List of 3
  .. .. .. .. ..$ name    : chr "h2"
  .. .. .. .. ..$ attribs : Named list()
  .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. ..$ : chr "Kolada"
  .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. ..- attr(*, "class")= chr [1:2] "shiny.tag.list" "list"
  .. .. ..$ :List of 3
  .. .. .. ..$ name    : chr "div"
  .. .. .. ..$ attribs :List of 1
  .. .. .. .. ..$ class: chr "col-sm-4"
  .. .. .. ..$ children:List of 1
  .. .. .. .. ..$ :List of 3
  .. .. .. .. .. ..$ name    : chr "form"
  .. .. .. .. .. ..$ attribs :List of 2
  .. .. .. .. .. .. ..$ class: chr "well"
  .. .. .. .. .. .. ..$ role : chr "complementary"
  .. .. .. .. .. ..$ children:List of 3
  .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. .. .. ..$ attribs :List of 1
  .. .. .. .. .. .. .. .. ..$ class: chr "form-group shiny-input-container"
  .. .. .. .. .. .. .. ..$ children:List of 2
  .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
  .. .. .. .. .. .. .. .. .. ..$ attribs :List of 3
  .. .. .. .. .. .. .. .. .. .. ..$ class: chr "control-label"
  .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "m_id-label"
  .. .. .. .. .. .. .. .. .. .. ..$ for  : chr "m_id"
  .. .. .. .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "Select city"
  .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. .. .. .. .. ..$ attribs : Named list()
  .. .. .. .. .. .. .. .. .. ..$ children:List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "select"
  .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 2
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "m_id"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "shiny-input-select"
  .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ : 'html' chr "<option value=\"Stockholm\" selected>Stockholm</option>\n<option value=\"Linköping\">Linköping</option>\n<optio"| __truncated__
  .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "html")= logi TRUE
  .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "script"
  .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ type         : chr "application/json"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-for     : chr "m_id"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-nonempty: chr ""
  .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ : 'html' chr "{\"plugins\":[\"selectize-plugin-a11y\"]}"
  .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "html")= logi TRUE
  .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. ..- attr(*, "html_dependencies")=List of 1
  .. .. .. .. .. .. .. .. ..$ :function ()  
  .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag.function"
  .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. .. .. ..$ attribs :List of 1
  .. .. .. .. .. .. .. .. ..$ class: chr "form-group shiny-input-container"
  .. .. .. .. .. .. .. ..$ children:List of 2
  .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
  .. .. .. .. .. .. .. .. .. ..$ attribs :List of 3
  .. .. .. .. .. .. .. .. .. .. ..$ class: chr "control-label"
  .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "year-label"
  .. .. .. .. .. .. .. .. .. .. ..$ for  : chr "year"
  .. .. .. .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "Select year"
  .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. .. .. .. .. ..$ attribs : Named list()
  .. .. .. .. .. .. .. .. .. ..$ children:List of 2
  .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "select"
  .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 2
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "year"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "shiny-input-select"
  .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ : 'html' chr "<option value=\"2017\" selected>2017</option>\n<option value=\"2018\">2018</option>\n<option value=\"2019\">201"| __truncated__
  .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "html")= logi TRUE
  .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "script"
  .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ type         : chr "application/json"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-for     : chr "year"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ data-nonempty: chr ""
  .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ : 'html' chr "{\"plugins\":[\"selectize-plugin-a11y\"]}"
  .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "html")= logi TRUE
  .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. ..- attr(*, "html_dependencies")=List of 1
  .. .. .. .. .. .. .. .. ..$ :function ()  
  .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag.function"
  .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. .. .. ..$ attribs :List of 4
  .. .. .. .. .. .. .. .. ..$ id             : chr "gender"
  .. .. .. .. .. .. .. .. ..$ class          : chr "form-group shiny-input-checkboxgroup shiny-input-container"
  .. .. .. .. .. .. .. .. ..$ role           : chr "group"
  .. .. .. .. .. .. .. .. ..$ aria-labelledby: chr "gender-label"
  .. .. .. .. .. .. .. ..$ children:List of 2
  .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
  .. .. .. .. .. .. .. .. .. ..$ attribs :List of 3
  .. .. .. .. .. .. .. .. .. .. ..$ class: chr "control-label"
  .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "gender-label"
  .. .. .. .. .. .. .. .. .. .. ..$ for  : chr "gender"
  .. .. .. .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ : chr "Select genders"
  .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ class: chr "shiny-options-group"
  .. .. .. .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
  .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "checkbox"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : Named list()
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "checkbox"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "gender"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "M"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : Named list()
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : 'html' chr "M"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "html")= logi TRUE
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : list()
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
  .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "checkbox"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : Named list()
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "checkbox"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "gender"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "F"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : Named list()
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : 'html' chr "F"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "html")= logi TRUE
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : list()
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
  .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "checkbox"
  .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
  .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : Named list()
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "checkbox"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "gender"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "T"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : Named list()
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : 'html' chr "T"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "html")= logi TRUE
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : list()
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. ..$ :List of 3
  .. .. .. ..$ name    : chr "div"
  .. .. .. ..$ attribs :List of 2
  .. .. .. .. ..$ class: chr "col-sm-8"
  .. .. .. .. ..$ role : chr "main"
  .. .. .. ..$ children:List of 1
  .. .. .. .. ..$ :List of 3
  .. .. .. .. .. ..$ name    : chr "div"
  .. .. .. .. .. ..$ attribs :List of 4
  .. .. .. .. .. .. ..$ id   : chr "barplot"
  .. .. .. .. .. .. ..$ class: chr "shiny-plot-output"
  .. .. .. .. .. .. ..$ style: chr "width:100%;height:400px;"
  .. .. .. .. .. .. ..$ class: chr "html-fill-item"
  .. .. .. .. .. ..$ children: list()
  .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. .. .. .. .. ..- attr(*, "html_dependencies")=List of 1
  .. .. .. .. .. .. ..$ :List of 10
  .. .. .. .. .. .. .. ..$ name      : chr "htmltools-fill"
  .. .. .. .. .. .. .. ..$ version   : chr "0.5.8.1"
  .. .. .. .. .. .. .. ..$ src       :List of 1
  .. .. .. .. .. .. .. .. ..$ file: chr "fill"
  .. .. .. .. .. .. .. ..$ meta      : NULL
  .. .. .. .. .. .. .. ..$ script    : NULL
  .. .. .. .. .. .. .. ..$ stylesheet: chr "fill.css"
  .. .. .. .. .. .. .. ..$ head      : NULL
  .. .. .. .. .. .. .. ..$ attachment: NULL
  .. .. .. .. .. .. .. ..$ package   : chr "htmltools"
  .. .. .. .. .. .. .. ..$ all_files : logi TRUE
  .. .. .. .. .. .. .. ..- attr(*, "class")= chr "html_dependency"
  .. .. .. ..- attr(*, "class")= chr "shiny.tag"
  .. ..- attr(*, "class")= chr "shiny.tag"
 - attr(*, "class")= chr [1:2] "shiny.tag.list" "list"
 - attr(*, "html_dependencies")=List of 1
  ..$ :function ()  
  .. ..- attr(*, "class")= chr "shiny.tag.function"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ui)
## maybe str(ui) ; plot(ui) ...
}
\keyword{datasets}
