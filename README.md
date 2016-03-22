# webscrape-LTER
R script to scrape the LTER data portal for all potential data entities that exist.

The webscraping was done with R because I hadn't actually learned to use python
before I tried this. The packages used were as follows:
httr, XML, RCurl, stringr, zoo, foreach, doParallel, plyr.

I used foreach and doParallel to implement a couple of parallelized loops
to speed up the script. I didn't add any sleep commands to give the server
a break because this was before I knew how. Either way I didn't get booted
with all the request I was giving the server so as of 08/31/2015 the file
worked. 

R version: TO add later (I forgot if I upgraded R after doing this or not...so 
should check that out)

Package versions: To add later (I'll get to this if someone really shows interest in 
using this script)
