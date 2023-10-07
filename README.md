# house_of_representatives
This R script creates an edge social network dataset for current House of Representatives. The finished product lists pairs of House members and a count of shared votes. House members who have voted together more often will have a higher count.
The script uses rvest (https://cran.r-project.org/web/packages/rvest/index.html) to scrape data from the Office of the Clerk, U.S. House of Represntatives website (https://clerk.house.gov/Votes). 

The current version is up to date as of October 6th, 2023 and pulls in every vote with the exception of votes for Speaker of the House. I plan to add a new function which will harvest votes for Speaker of the House.
