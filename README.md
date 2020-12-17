# datagoboop
The data. It goes boop now.

## Overview

`datagoboop` is package for an audio alternative to data visualization: data sonification. We provide a series of functions for creatively sonifying your data and include sonified versions of the histogram, scatterplot, boxplot, and `lm()` diagnostics to help get you started.

## Installation

In the R console, run:

`devtools::install_github("akseong/datagoboop")`

## Usage
You can use `datagoboop` in two ways. Either use the provided functions to create an audio summary of your data, or use it as a framework to make R output sounds.

### Sonify your data
* `sonify_hist()` - Create an audio histogram. Map the relative frequencies to the pitches of the songs that are played in ascending order of your variable.
* `sonify_box()` - Create an audio boxplot. A sound will slide pitchwise along the distribution of the data. You can compare distributions by comparing the range of pitches being traveled.
* `sonify_scatter()` - Create an audio scatter plot. The x variable will play on a lower register in ascending order while the y variable will play their value in terms of relative pitches.
* `sonify_mod()` - Create model diagnostics from linear model output. Both studentized residuals and qq comparisons. 

### Make R sing
* `note()`, `notes()`, and `chord()` - Create audio vectors that can be played using ...
* `wplay()` - Playback the audio vector using the loaded sound driver. Also creates a play button when knitting an html document using R Markdown.

## Customize to your liking!
* Don't like synthesized sine waves or pianos? Load in your own sounds for playback using add_inst(). You can vary the pitch of your instrument using the repitch() function!
* Make your own audio summaries by mapping data to pitch, volume, duration, phase, etc... We've made the functions as modular as possible so you can make the sounds you want. 
