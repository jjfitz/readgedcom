# Reading GEDCOM
Reading GEDCOM files and doing analysis in R

I was surprised to find that with Family history being the 2nd most popular hobby in the United States, that there hasn't been a package built in R to read GEDCOM files. This is my stab at it. I am hoping that this repository can help others learn more about their ancestors using data analysis.
Right now, with the read_gedcom function, you can read gedcom files and store them in a tidy format. This is still in development, and hopefully it can work to fill your family history needs.

To get GEDCOM file off of ancestry:
https://support.ancestry.com/s/article/Uploading-and-Downloading-Trees

I also found this script to get a GEDCOM file off of FamilySearch:
https://github.com/Linekio/getmyancestors

There are also many sites that offer free GEDCOM files for celebrities/royalty/etc. 

You can start using this package by using:
```
# install.packages("devtools")
devtools::install_github("jjfitz/readgedcom")
```

Here is also an article that I wrote with exploring my family history:
https://jjfitz.github.io/projects/Genealogy.html
