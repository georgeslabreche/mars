library(whisker)
library(slugify)

build_latex_plot_figure = function(title, path){
  latex_template = 
    "\\begin{figure}[H]
  \\centering
  \\hypersetup{linkcolor=captionTextColor}
  \\includegraphics[width=0.8\\linewidth]{ {{path}} }\\\\
  \\caption[{{title}}]
  { {{title}} }
  \\label{fig:plot:{{title_slug}} }
\\end{figure}"
  
  data = list(
    title = gsub("\n", " ", title),
    title_slug = slugify(gsub("\n", " ", title), space_replace="-"),
    path = path
  )
  
  latex = whisker.render(latex_template, data)
  latex = gsub("\\{ ", "{", latex)
  latex = gsub(" \\}", "}", latex)
  
  cat(latex)
  
  return(latex)
}
