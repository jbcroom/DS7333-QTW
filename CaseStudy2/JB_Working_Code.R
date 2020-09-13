library(data.table)
library(RCurl)
library(XML)
library(stringr)
library(tidyr)
library(urltools)
library(rvest)
library(dplyr)
library(XML)


ubase = "http://www.cherryblossom.org/"

womenURLs = 
  c("results/1999/cb99f.html", 
    "results/2000/Cb003f.htm", 
    "results/2001/oof_f.html",
    "results/2002/ooff.htm", 
    "results/2003/CB03-F.HTM",
    "results/2004/women.htm", 
    "results/2005/CB05-F.htm", 
    "results/2006/women.htm", 
    "results/2007/women.htm", 
    "results/2008/women.htm", 
    "results/2009/09cucb-F.htm",
    "results/2010/2010cucb10m-F.htm", 
    "results/2011/2011cucb10m-F.htm",
    "results/2012/2012cucb10m-F.htm")

urls = paste(ubase, menURLs, sep = "")

urls[1:3]


extractResTable =
  # takes a list of websites from the cherry blossom race
  # a list of years corresponding to the year the result is for
  # and the gender of the participant
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  # returns a list of strings corrsponding to lines in the web url
  function(url = "http://www.cherryblossom.org/results/2009/09cucb-F.htm",
           year = 1999, sex = "male", file = NULL)
  {
    doc = htmlParse(url)
    
    if (year == 2000) {
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
    }
    else if (year == 2009 & sex == "male") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
      els = gsub("Â", " ", els)
    }
    else if (year == 1999) {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\n")[[1]]   
    } 
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]   
    } 
    
    if (is.null(file)) return(els)
    # Write the lines as a text file.
    writeLines(els, con = file)
  }


years = 1999:2012
urls = paste(ubase, womenURLs, sep = "")
urls[1:3]
womenTables = mapply(extractResTable, url = urls, year = years, sex='female')
names(womenTables) = years
sapply(womenTables, length)

# 1999
womenTables[1]

# 2000
womenTables[2]

# 2001
womenTables[3]

# 2002
womenTables[4]

# 2003
womenTables[5]

# 2004
womenTables[6]

# 2005
womenTables[7]

# 2006
womenTables[8]

# 2007
womenTables[9]

# 2008
womenTables[10]

# 2009
women_2009_lst <- womenTables[11]
women_2009_lst <- lapply(women_2009_lst, function(x) {x[c(-1:-4)]})
lengths(women_2009_lst) # 8329

women_2009_t = as.data.frame(women_2009_lst)
women_2009_df <- data.frame(Place = substr(women_2009_t[5:8329,],1,5),
                            DivTot =substr(women_2009_t[5:8329,],7,17),
                            Num =substr(women_2009_t[5:8329,],19,24),
                            Name =substr(women_2009_t[5:8329,],26,47),
                            Ag =substr(women_2009_t[5:8329,],49,50),
                            Hometown =substr(women_2009_t[5:8329,],52,71),
                            Gun_Time =substr(women_2009_t[5:8329,],73,79),
                            NetTime =substr(women_2009_t[5:8329,],81,87),
                            Pace =substr(women_2009_t[5:8329,],89,94),
                            S =substr(women_2009_t[5:8329,],96,96))

head(women_2009_df)
tail(women_2009_df)



### 2010
women_2010_lst <- womenTables[12]
women_2010_lst <- lapply(women_2010_lst, function(x) {x[c(-1:-4)]})
lengths(women_2010_lst) # 8859

# Convert to dataframe
women_2010_t = as.data.frame(women_2010_lst)
women_2010_df <- data.frame(Place = substr(women_2010_t[5:8859,],1,5),
                            DivTot =substr(women_2010_t[5:8859,],7,17),
                            Num =substr(women_2010_t[5:8859,],19,24),
                            Name =substr(women_2010_t[5:8859,],26,47),
                            Ag =substr(women_2010_t[5:8859,],49,50),
                            Hometown =substr(women_2010_t[5:8859,],52,71),
                            Five_Mile =substr(women_2010_t[5:8859,],73,79),
                            GunTime =substr(women_2010_t[5:8859,],81,87),
                            NetTime =substr(women_2010_t[5:8859,],89,96),
                            Pace =substr(women_2010_t[5:8859,],98,102),
                            S =substr(women_2010_t[5:8859,],104,104))

head(women_2010_df)
tail(women_2010_df)

#### 2011
women_2011_lst <- womenTables[13]
women_2011_lst <- lapply(women_2011_lst, function(x) {x[c(-1:-4)]})
lengths(women_2011_lst) # 9034

# Convert to dataframe
women_2011_t = as.data.frame(women_2011_lst)
women_2011_df <- data.frame(Place = substr(women_2011_t[5:9034,],1,5),
                            DivTot =substr(women_2011_t[5:9034,],7,17),
                            Num =substr(women_2011_t[5:9034,],19,24),
                            Name =substr(women_2011_t[5:9034,],26,47),
                            Ag =substr(women_2011_t[5:9034,],49,50),
                            Hometown =substr(women_2011_t[5:9034,],52,71),
                            Five_Mile =substr(women_2011_t[5:9034,],73,79),
                            Time =substr(women_2011_t[5:9034,],81,87),
                            NetTime =substr(women_2011_t[5:9034,],89,96),
                            Pace =substr(women_2011_t[5:9034,],98,101),
                            S =substr(women_2011_t[5:9034,],103,103))

head(women_2011_df)
tail(women_2011_df)


### 2012
women_2012_lst <- womenTables[14]
women_2012_lst <- lapply(women_2012_lst, function(x) {x[c(-1,-2,-3,-4)]})
lengths(women_2012_lst) # 9733

# Convert to dataframe
women_2012_t = as.data.frame(women_2012_lst)
women_2012_df <- data.frame(Place = substr(women_2012_t[5:9733,],1,5),
                  DivTot =substr(women_2012_t[5:9733,],7,17),
                  Num =substr(women_2012_t[5:9733,],19,24),
                  Name =substr(women_2012_t[5:9733,],26,47),
                  Ag =substr(women_2012_t[5:9733,],49,50),
                  Hometown =substr(women_2012_t[5:9733,],52,71),
                  Five_Mile =substr(women_2012_t[5:9733,],73,79),
                  Time =substr(women_2012_t[5:9733,],81,87),
                  Pace =substr(women_2012_t[5:9733,],89,93),
                  S =substr(women_2012_t[5:9733,],95,95))

head(women_2012_df)
tail(women_2012_df)