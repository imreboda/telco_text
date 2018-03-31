# Telco Posts

This is a term assignment of the Unstructured Text Analysis Seminar at CEU in the 2018 winter semester.

The attached R scripts look at the Facebook pages of a few telco and related vendors (to some extent related to telcos' traditional or new business segments), and present / compare their key marketing themes during the past few quarters. Also, does basic sentiment scoring on the comments along the time.

# Installation

## Facebook App and Token:
- Registering at Facebook Developer page (https://developers.Facebook.com),
- Creating a new App, getting the App ID and App Secret,
- On Facebook, setting site URL of the App to "http://localhost:1410/",
- From Rstudio (with Rfacebook installed in previous chapter and included), start fboAuth with the App ID and App Secret parameters obtained in the previous step,
- Follow the instructions seen on the console. 
- Save the received token somewhere for later use.

## Script files:
- The shiny script (app.R) is to be deployed the usual way to shiny
- The script file should be run regularly, I used Jenkins with daily schedule
- The DIR variable in both scripts must be set to a directory to store temporary files for operation

