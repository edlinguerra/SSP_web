README
================

## Repository of the Online version of SSP

**SSP** is an **R** package designed to estimate sample effort in
studies of ecological communities using intensive sampling over several
sets of simulated data. Our procedure is based on the previous
definition of MultSE but eludes the double resampling over a unique
pilot data set (Anderson and Santana-Garcon 2015). The protocol in
**SSP** consists of simulating several extensive data matrices that
mimic some of the relevant ecological features of the community of
interest using a pilot data set. For each simulated data, several
sampling efforts are repeatedly executed and *MultSE* is calculated to
each one. The mean value, 0.025 and 0.975 quantiles of *MultSE* for each
sampling effort across all simulated data are then estimated and
plotted. The mean values are standardized in relation to the lowest
sampling effort (consequently, the worst precision), and an optimal
sampling effort can be identified as that in which the increase in
sample size do not improve the precision beyond a threshold value
(e.g. 3 %).

The SSP package is developed at <https://github.com/edlinguerra/SSP/>.
This online version was built with [Shiny](https://shiny.rstudio.com/),
and is hosted at [Shinyapps.io](https://https://www.shinyapps.io). This
application has a limit of 25 active hours per month on this server, so
depending on the use of the app during this month, it may not be able to
run until the beginning of next month.

For citations use the pre-print:

Guerra-Castro, E. J., J. C. Cajas, F. N. Dias Marques Simoes, J. J.
Cruz-Motta, and M. Mascaro. 2020. SSP: An R package to estimate sampling
effort in studies of ecological communities.
bioRxiv:2020.2003.2019.996991.
