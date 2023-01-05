# The rsconnect package deploys applications to the shinyapps.io service
library(rsconnect)

# TODO
#  create a shinyapp.io account (free)
#  you can login w/ Google/GitHub

# TODO
#  on sidebar, go to Account > Tokens and generate a Token
#  click on Show and copy to clipboard

# set account info
rsconnect::setAccountInfo(name='floge', token='98815ECE8300DD510E84FF2AD5E3AC73', secret='vpnhHietPSNraqXo60TIK1zP9bs7y7sFg2LhBlG6')

# check that you app works fine in local
library(shiny)
runApp(appDir = 'apps/app_to_deploy/app.R')

# now ... deploy
deployApp(appDir = 'apps/app_to_deploy/')
