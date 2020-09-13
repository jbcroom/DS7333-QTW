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

head(womenTables)
tail(womenTables)

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
womenTables[11]

# 2010
womenTables[12]

# 2011
womenTables[13]

# 2012
womenTables[14]

