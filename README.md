author: Hui Fang/Joy Zhou
===========================
# ST558_Project3
   
This is a repo containing work from project3 for ST558.

The following R packages used for this project:
+ [`tidyverse`](https://www.tidyverse.org/)  
+ [`caret`](https://cran.r-project.org/web/packages/caret/)
+ [`shiny`](https://cran.r-project.org/web/packages/shiny/index.html)
+ [`DT`](https://rstudio.github.io/DT/)

The code used to create the analyses from a single .Rmd file (i.e. the render() code)
```
library(rmarkdown)

EducationLv <- unique(diabetes$Education)
output_file <- paste0(EducationLv, ".html")
params = lapply(EducationLv, FUN = function(x){list(Edu = x)})
reports <- tibble::tibble(output_file, params)
reports
apply(reports, MARGIN = 1,
      FUN = function(x){render(input = "work.Rmd", 
				output_file = x[[1]], 
				params = x[[2]])
 				})
```
 				
links to .html files of the generated analyses (which will be created by github pages! Not you!)
For example,
You can access the render document:   
+ Analysis for [work.md](work.html). Note you should only have a college_graduate_analysis.md file in the repo - github pages will render the .html file for you
+ Analysis for [SomeElementaty](https://hzhoujoy.github.io/ST558_Project3/SomeElementary.html)    
+ Analysis for [SomeHighSchool](SomeHighSchool.html)  
+ Analysis for [HighSchool](HighSchool.html)  
+ Analysis for [SomeCollege](SomeCollege.html)  
+ Analysis for [College](College.html)  

