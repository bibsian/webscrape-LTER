# Created by: Andrew Bibian
# Date created: 08/28/2015

# This is a scrip to go to each LTER Site through the portal, following the links to each 
# data packages, go to the metadata link, and scrape the alternate ID
# Edit: changes the code to handle lter site that have more than 110 records.
# and added a conditional statement to handle meta data that wasn't standardized
# some knb metadata links use Local identifier and others use Alternate ID.
# EditV4: changed the return from alternate/ local identifier to Title
# EditV5: Changed to run the whole process through a loop and
# write a csv file with the title of every study that is listed on the LTER data portal.
# The produced file will be compared to the studies available on individual LTER websites
# and references will be made to discrepencies found in our email to LTER mangaers.
# Edited to add more fields to be returned: links to each dataset within a package
# number of total datasets
# 

# Initial run time was 45.35 minutes

## Set working directory at home
#setwd("/Users/bibsian/Box Sync/LTER")

## Set working directory in lab
setwd("C:/Users/MillerLab/Box Sync/LTER/R Scripts for Automated Synthesis of LTER Data Portal")

## This is the list of libraries that must be installed on your computer before proceeding
libraries<- c("httr", "XML", "RCurl", "stringr", "zoo", "foreach", "doParallel", "plyr")
lapply(libraries, require, character.only=T)

## Clearing the environment before running script
rm(list=ls())

## Timing the whole process 
par.st<-proc.time()

## Storing the url home for the data portal: will be recalled repeatedly
url<- "https://portal.lternet.edu"

## Defing the certificate authority
## through RCurl (need this step because the data portal is a secure site (https vs http))
cafile<- system.file("CurlSSL", "cacert.pem", package="RCurl")

## Get url information 
page.info<- GET(url, path="nis/browse.jsp", config(cainfo=cafile)) 

## CHecking the status of the reponse from the server ##
http_status(page.info)

## Parsing the html so it can be read and links can be extracted ##
h<-htmlParse(page.info)
#class(h)

## Using html indexing to return all the links on the wepgaes ##
links<-xpathSApply(h, "//li/a", xmlGetAttr, "href" )

## creating a vector with the LTER site abbreviations ##
## This vector will be used to search the links retured from above ##
sites<- c("AND", "ARC", "BES", "BNZ", "CCE", "CDR", "CAP", "CWT", "FCE", "GCE", "HFR", "HBR", "JRN", "KBS"
          ,"KNZ", "NWK", "LUQ", "MCM", "MCR", "NWT", "NIN", "NTL", "PAL", "PIE", "SBC", "SEV", "SGS", "VCR")

## Extracting the links from the page.info url  ##
## that lets us search the webpage for datapackages for each site ##
sitepath.link<-unlist(sapply(sites, function(z) grep(pattern=z, x=links)))


## take a subset of the sitepath.link string because it was returned with ##
## unnecessary notation. This ##
## command returns the string starting with second letter ##
sitepage.vec<-  as.character(sapply(sitepath.link, function(x) substring(links[x],2)))

## Creating a list where every element of the  ##
## list will represent a LTER site ##
## and within that element will be the titles ##
## of every study that there is data for ##
title.list<-list()



## Starting the process of extracting all 
## the titles of the studies avaialbe from
## each site on the LTER data portal

for (i in 1:length(sites)){
  
  # Getting url for a site specific search
  site.url<-GET(url, path=paste0("nis", sitepage.vec[i]), config(cainfo=cafile))
  
  # Extracting the html from the sitepage.vec
  site.html<- htmlParse(site.url)
  
  # Pulling the links from the table presented: these are 
  # links to individual knb data packages 
  pset1.paths<- xpathSApply(site.html, "//td[@class='nis'][@align='left']/a", xmlGetAttr, "href")
  
  # Pulling links to search the remaining tables that
  # link to knb data packages
  # i.e. when there are more than 10 knb data packages
  # you can seach 1: X number of pages on a  table that 
  # list each data package
  remaining.pages<-xpathSApply(site.html, "//p[@align='center']/a", xmlGetAttr, "href")
  
  # If the remaining vector of links above
  # is 0 then all the packages are on the 1st page 
  # of the table
  if(length(remaining.pages)==0){
    
    # All data packages availabe for the site 
    # can be linked with this vector
    packages<- pset1.paths
    
  } 
  
  # If the remaing vector of links above
  # is greater than 0 then we need to
  # seach every page of the table and
  # extract links to the remaining data packages
  else{
    
    # This is command to get the index for the upper
    # bounds on the length of the table that we need 
    # to search 
    max.page<-max(as.numeric(matrix(unlist(strsplit(remaining.pages, "[=&]")), length(remaining.pages), 6, byrow=T)[,2]))
    
    # Creating a vector to index the table
    page.seq<- seq(10, max.page, 10)
    
    # Creating links to the table indices 
    pset.remain.paths<- sapply(page.seq, function(x) gsub("10&rows=", paste0(x,"&rows="),remaining.pages[1]))
    
    # Creating a list that will be filled with
    # links to data packages on different pages of 
    # the table that houses them
    pset.list<-list()
    
    ## Loopin gover the indicies of the table 
    # to extract the links to data packages 
    for (j in 1:length(pset.remain.paths)){
      
      # Extract html on each page of the table that list data packages
      page.html<- htmlParse(GET(url, path=paste0("nis/",pset.remain.paths[j])))
      
      # Extract the links to the data packages
      pset.list[[j]]<-xpathSApply(page.html, "//td[@class='nis'][@align='left']/a", xmlGetAttr, "href")
    }
    
    # Combine all links to all data packages for a site 
    packages<-c(pset1.paths,unlist(pset.list))  
    
  }
  
  num.datasets<-NULL
  
  cl<-makeCluster(7)
  registerDoParallel(cl)
  
  list.data.link<-list()
  list.data.link <- foreach (j=1:length(packages), .packages=c("httr", "XML", "stringr")) %dopar%{
    
    # Extracting html from summary data page 
    summary.html<-htmlParse(GET(url, path=paste0("nis/",packages[j])))
    
    # Getting the links to each data set from their Xpath index
    data.link.path<-xpathSApply(summary.html, "//div/ul/li/ol/li/a", xmlGetAttr, "href")
    
    # Making the link to the actual dataset so we can click and download if we want
    data.link<-paste0(url, "/nis", as.character(lapply(data.link.path, function(x) substring(x,2))))
    
    # filling in the dataset list
    list.data.link<- data.link
    
  }
  
  # stoping the parallel clusters (see foreach package)
  stopCluster(cl)
  # There will be a table with these data links
  # It will be attached to the output
  
  # calculating the biggest number of datasets for the set  
  c.length<-max(unlist(lapply(list.data.link, length)))
  
  # Combing the links into a table and putting in NA where needed based on 
  # c.length
  data.link.table<- lapply(list.data.link, function(x) {
    if(length(x)< c.length){
      matrix(c(x, rep(NA, c.length-length(x))), 1, c.length)
    } else{
      matrix(x, 1, c.length)
    }
  })
  
  # Creating the link to the metadata file from the link to data packages
  # this is simple subsitution of strings
  metalink<-gsub("./mapbrowse", "https://portal.lternet.edu/nis/metadataviewer", packages)
  
  
  ## Creating a parallel for loop
  # to go through every metadata link created above
  # and extract the title of studies housed within that link
  # Creating number of clusters to be used
  cl<-makeCluster(7)
  
  # registering them with the computer (see package foreach)
  registerDoParallel(cl)
  
  # Starting the parallel loop to get a vector of study titles
  title<- foreach (j=1:length(metalink), .combine=c, .packages=c("httr", "XML", "stringr")) %dopar% {
    
    # extracting url info from the metadata link
    html.meta<-GET(metalink[j]) 
    
    # extracting html data from the link
    meta.parse<-htmlParse(html.meta)
    
    # Returning the title based on html index
    title<- gsub("\\s+", " ", str_replace_all(xpathSApply(meta.parse, "//div/h2", xmlValue)[3], "\n", ""))
  }
  
  # stoping the parallel clusters (see foreach package)
  stopCluster(cl)
  
  # Storing the list of titles and the corresponding knb package in the big list
  # that indexes each site
  title.list[[i]]<-cbind(title, str_split_fixed(packages, "=", 2)[,2], unlist(lapply(list.data.link, length)), 
                         do.call(rbind, data.link.table))
  
}

# Creating a vector to label each title returned with
# the respective site it came from
rep.list<-cbind(sites, unlist(lapply(title.list, nrow)))

# Making the title.list into a dataframe to write on a csv
title.df<- cbind(unlist(apply(rep.list, 1, function(x) rep(x[1], each=x[2]))), ldply(title.list, data.frame ))

# naming the dataframe
colnames(title.df)<-c("site", "title")

# writing the csv file with the data the portal was searched
write.csv(title.df, paste0("List_Of_Studies_On_Data_Portal_", gsub("\\s", "_", gsub(":", "", date())),".csv"))

# timing the processes
par.stop<-proc.time()-par.st

