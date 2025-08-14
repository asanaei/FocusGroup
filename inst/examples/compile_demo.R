# Script to compile comprehensive demo
library(FocusGroup)
library(rmarkdown)
library(knitr)

# Check if API key is available
api_key <- Sys.getenv('GROQ_API_KEY')
if (api_key == '') {
  cat('Warning: GROQ_API_KEY not found. Setting dummy key for compilation test.\n')
  Sys.setenv(GROQ_API_KEY = 'dummy_key_for_testing')
}

# Try to render the document
cat('Attempting to render comprehensive_demo.qmd...\n')
tryCatch({
  rmarkdown::render('comprehensive_demo.qmd', 
                   output_file = 'comprehensive_demo_improved.html',
                   quiet = FALSE)
  cat('SUCCESS: Report compiled!\n')
}, error = function(e) {
  cat('ERROR during compilation:\n')
  cat(conditionMessage(e), '\n')
  cat('This may be due to API limitations, but the code structure should be sound.\n')
})
