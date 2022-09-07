# The rsconnect package deploys applications to the shinyapps.io service
library(rsconnect)

# TODO
#  create a shinyapp.io account (free)
#  you can login w/ Google/GitHub

# TODO
#  on sidebar, go to Account > Tokens and generate a Token
#  click on Show and copy to clipboard

# set account info
rsconnect::setAccountInfo(name='xxx', token='xxx', secret='xxx')

# check that you app works fine in local
library(shiny)
runApp(appDir = 'apps/app_to_deploy/app.R')

# now ... deploy
deployApp(appDir = 'apps/app_to_deploy/app.R')
