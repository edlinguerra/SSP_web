<table style="width:18%;">
<colgroup>
<col style="width: 18%" />
</colgroup>
<tbody>
<tr class="odd">
<td># SSP On line</td>
</tr>
<tr class="even">
<td><strong>Edlin Guerra-Castro<sup>2</sup>, Juan Carlos Cajas, Juan José Cruz-Motta, Nuno Simoes and Maite Mascaro</strong> Universidad NAcional Autónoma de México y Universidad de Puerto Rico ## RATIONALE OF SSP</td>
</tr>
<tr class="odd">
<td><strong>SSP</strong> is an <strong>R</strong> package designed to estimate sample effort in studies of ecological communities using intensive sampling over several sets of simulated data. Our procedure is based on the previous definition of MultSE but eludes the double resampling over a unique pilot data set (Anderson and Santana-Garcon 2015). The protocol in <strong>SSP</strong> consists of simulating several extensive data matrices that mimic some of the relevant ecological features of the community of interest using a pilot data set. For each simulated data, several sampling efforts are repeatedly executed and <em>MultSE</em> is calculated to each one. The mean value, 0.025 and 0.975 quantiles of <em>MultSE</em> for each sampling effort across all simulated data are then estimated and plotted. The mean values are standardized in relation to the lowest sampling effort (consequently, the worst precision), and an optimal sampling effort can be identified as that in which the increase in sample size do not improve the precision beyond a threshold value (e.g. 3 %).</td>
</tr>
<tr class="even">
<td><strong>SSP</strong> includes seven functions: <code>assempar</code> for extrapolation of assemblage parameters using pilot data; <code>simdata</code> for simulation of several data sets based on extrapolated parameters; <code>datquality</code> for evaluation of plausibility of simulated data; <code>sampsd</code> for repeated estimations of <em>MultSE</em> for different sampling designs in simulated data sets; <code>summary_sd</code> for summarizing the behavior of <em>MultSE</em> for each sampling design across all simulated data sets, <code>ioptimum</code> for identification of the optimal sampling effort, and <code>plot_ssp</code> to plot sampling effort vs <em>MultSE</em>.</td>
</tr>
<tr class="odd">
<td>The SSP package is developed at <a href="https://github.com/edlinguerra/SSP/" class="uri">https://github.com/edlinguerra/SSP/</a></td>
</tr>
</tbody>
</table>

THE ONLINE VERSION OF SSP
-------------------------

The sequence of using **SSP** online is the same as in R.

1.  First, you must start with `1. assempar`. For this, upload the pilot
    data as a .csv file (species names in columns and samples as rows,
    the first column should indicate the site to which the sample
    belongs, regardless of whether a single site has been sampled).
    Identify the type of data (presence / absence, counts, coverage) as
    well as the extrapolation method of species richness. Then press
    `GO assempar`. Wait for the progress bar to finish to move to
    `2. simdata`.

2.  Once in `2. simdata`, define the number of data sets to be simulated
    (cases), the number of sites (this applies only if you have many
    sites in the pilot data, keep at 1 if you do not have multiple
    sites), and the number of samples to be simulated in each site.
    Press `GO simdata` and wait for the progress bar to finish.

3.  In `3. sampsd`, define the maximum sampling effort (m for sites, n
    for samples) as well as the number of repetitions (k) for each
    combination. Remember, if you are simulating a single site keep m
    at 1. You should also identify the appropriate pretreatment for
    abundances and choose the similarity measure. Press `GO sampsd` and
    wait. Don’t skip to the next panel until the progress bar ends, this
    process can take a while, especially if you requested a lot of
    datasets, sites, samples, and repetitions. We suggest using few
    simulations (cases) and few repetitions to obtain exploratory
    results. Then you can repeat the process by increasing these
    arguments.

4.  In the fourth panel `4. summary_ssp & datquality` you must identify
    whether you simulated one site (by default) or several. At this
    point you can assess the quality of the simulated data with
    `GO datquality`. Then, press `Go summary` to calculate the *MultSE*
    behavior across all simulated data sets for each sampling effort.
    The results will be displayed in the *Summary MultSE* tab. Both
    outputs can be downloaded as .csv files.

5.  Once the summary is displayed, you can go to `5. ioptimum & plot`.
    You can choose the optimization breakpoints regarding the lowest
    sampling (by default 10%, 5% and 3%) as well as the format of the
    file to download (png or pdf). Press `Go ioptimum`, the plot will be
    displayed in the *MultSE plot* tab.

This online version was built with [Shiny](https://shiny.rstudio.com/),
and is hosted at [Shinyapps.io](https://https://www.shinyapps.io). This
application has a limit of 25 active hours per month on this server, so
depending on the use of the app during this month, it may not be able to
run until the beginning of next month.

------------------------------------------------------------------------

CITATION of SSP
---------------

Guerra-Castro, E. J., J. C. Cajas, F. N. Dias Marques Simoes, J. J.
Cruz-Motta, and M. Mascaro. 2020. SSP: An R package to estimate sampling
effort in studies of ecological communities.
bioRxiv:2020.2003.2019.996991.
