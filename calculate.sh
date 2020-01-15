#!/bin/bash

Rscript func/norm/n5mean.R
Rscript func/norm/n5mean1var.R
Rscript func/norm/n5mean2var.R
Rscript func/norm/n5var.R

Rscript func/cauchy/n5mean.R
Rscript func/cauchy/n5mean1var.R
Rscript func/cauchy/n5mean2var.R
Rscript func/cauchy/n5var.R

Rscript func/laplas/n5mean.R
Rscript func/laplas/n5mean1var.R
Rscript func/laplas/n5mean2var.R
Rscript func/laplas/n5var.R