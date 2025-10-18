## Diffdf v1.1.2

Note that I have updated the maintainer email. The previous email was my work email for my former employer which I have since left. The new email is my personal email address. I no longer have access to nor control the prior work email address.

In this verion of diffdf
- we made several bug fixes for user reported issues namely:
    - Fixed bug of where an internal function was reliant on a partial argument match
    - Fixed bug where `diffdf()` would error if the input was a non-trivial expression
    - Fixed bug where `diffdf()` would error if an input dataset contained a POSIXct column which contains a missing value
    - Added argument to `print()` function to allow users to print more than 10 rows at once when creating a file output



## R CMD check results

R CMD check results
Status: 1 NOTE

New maintainer:
  Craig Gower-Page <craiggower@gmail.com>
Old maintainer(s):
  Craig Gower-Page <craig.gower-page@roche.com>

## revdepcheck results

We checked the following reverse dependencies, all passed with no issues:

- admiral
- admiraldev
- admiralmetabolic
- admiralneuro
- admiralonco
- admiralophtha
- admiralvaccine
- pharmaverseadam
- random.cdisc.data
