######################################################
##########            Kiosk Mode          ############
######################################################
#
# Runs chrome and other apps in full-screen mode 
# on predefined screens
# ----------------------------------------------------

$chromePath = 'C:\Program Files (x86)\Google\Chrome\Application\chrome.exe'
$chromeArguments = '--new-window'
# if Window not moved (especially on machine start) - try increaing the delay. 
$ChromeStartDelay = 5

Set-Location $PSScriptRoot
. .\HelperFunctions.ps1

# Kill all running instances
# &taskkill /im chrome* /F

#URLS for external version
#Chrome-Kiosk 'https://nickt.shinyapps.io/swotr?=Wall' -MonitorNum 1 
#Chrome-Kiosk 'https://nickt.shinyapps.io/swotr?=Floor' -MonitorNum 2

#URLS for IDEA Cluster
Chrome-Kiosk 'https://lp01.idea.rpi.edu/shiny/erickj4/swotr?=Wall' -MonitorNum 1 
Chrome-Kiosk 'https://lp01.idea.rpi.edu/shiny/erickj4/swotr?=Floor' -MonitorNum 2
