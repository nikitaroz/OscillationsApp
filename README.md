# OscillationsApp

An app for looking at oscillations within a spectrum. Part of Fang research 
group at Oregon State University.

## Specifications
This will be exclusively a Shiny app (not enough new functionality to merit a
complete package).
The first tab will display the raw data with a contour plot. Below this data
will be a plot of the integrated intensity along the x and y axes. Selecting
a region on the main plot will cause the lower plots to display the integrated
intensity for that region.

The second tab will display the Fast Fourier transform of the data (by default).
The user can choose from several options of estimating the power spectral 
density, including the lomb periodogram. The tab will display, depending of the
method, the psd estimate and/or the phase as contour plots. Along the margins 
are line plots displaying the total integrated intensity along the axes. Upon
highlighting a region on the contour plot, another set of line plots appear on 
the margins displaying the integrated intensity of the region (similar to 
[here](https://plot.ly/r/shinyapp-linked-brush/)). Also, if one of the line 
plots is hovered over when no region is selected, the other will update with the
integrated intensity of that point.

The third tab will display the continuous wavelet transform of the data. The 
phase of the wavelet transform is not particularly telling from inspection, so
it is omitted. A sidebar allows the user to choose the parameters of the 
transform (octaves, voices, etc.). The wavelet transform is lazily calculated.
The tab also contains a small line plot of the frequency integrated intensity
along the wavenumber axis. The user can hover over the plot, and the wavelet
transform will update. Also, the plot has a range selector 
[here](https://plot.ly/r/range-slider/) to allow for selecting ranges of data.
