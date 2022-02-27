# WORDPRED

## GOAL  
The goal for this capstone project is the building of predictive model of English text. Ideally we want to be able to develop a product that will predict with accuracy the most probable next word to come in a sentence much alike it can be experienced when typing on a smartphone for example. This will be achieved by means of Natural Language Processing (NPL) techniques using the R programming language. This was the final project of the data science course provided by John Hopkins university.  

## DATA    
The corpus, collected from publicly available sources by a web crawler, come from 3 different sources (news, blogs and Twitter) for four different language (English, Russian, Deutch and Finish) have been kindly provided by the Swiftkey company. Here we focused on the English language.

## METHOD   
Given computational limitations, corpus were split into smaller files and then processed using the Quanteda package. Next word prediction is performed by a simple back-off algorithm. The data and the script are incorporated in a shiny application hosted on shinyapps.io:  
https://tapewormer.shinyapps.io/wordPred/  

For more information please see the project description on rpub here:  
https://rpubs.com/Tapewormer/860809  


## Some online ressources
- Wikipedia links  
https://en.wikipedia.org/wiki/Natural_language_processing  
https://en.wikipedia.org/wiki/Lexical_analysis  
- Stanford university's lectures  
https://web.stanford.edu/~jurafsky/NLPCourseraSlides.html
- Related packages on CRAN  
https://cran.r-project.org/web/views/NaturalLanguageProcessing.html  
- Tm package paper  
https://www.jstatsoft.org/article/view/v025i05
- Quanteda  
https://tutorials.quanteda.io/  
https://joss.theoj.org/papers/10.21105/joss.00774  
- Générer des n-gram avec tidytext. (French)  
https://thinkr.fr/text-mining-n-gramme-avec-r/  
