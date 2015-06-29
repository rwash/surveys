This R package allows you to easily load data from surveys.  It has specfically been tested 
using Qualtrics surveys.

To install the package:

    library(devtools)
    devtools::install_github("rwash/surveys")
    
Loading Surveys
---------------
    
To load a survey:

    survey <- load_survey("filename.csv")
    
You may see warnings -- questions that were not auto-detected.  To fix these, you can add new
multiple choice options:

    add_multiple_choice(c("Never", "Always"), ordered=T)

You can also tell it to ignore a specific question:

    ignore_question("Q15")
    
If you have attention check questions, you can specifically tell it which question is an attention check,
and what the correct answer is:

    add_attention_check("pres", "Barack Obama")
    
Computing Scales
----------------

You can combine multiple questions into a scale using

    survey$scale <- item.scale(~Q1 + Q2, data=survey)
    
If you have lots of items, you can use `mf()` to combine them into a formula:

    survey$scale <- item.scale(mf("Q", 1:20), data=survey)
    
There are the normal S3 functions for working with scales:

    print(survey$scale)
    summary(survey$scale)
    plot(survey$scale)
