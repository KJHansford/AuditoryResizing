These materials are a computationally reproducible version of the paper:

Hansford, K., McKenzie, K., Baker, D.H. & Preston, C.E. (2023). The Effect of Non-Naturalistic Auditory Input on Conscious and Unconscious Experiences of Resizing Illusions

The file sizeadaptation.Rmd is an R markdown file that will perform all analyses and figure creation, and produce a pdf version of the manuscript.

The full repository can be downloaded (cloned), and contains all the required data files. 
However if any data files are missing the code will attempt to download them from the OSF repository for this project:
http://doi.org/10.17605/OSF.IO/KTHG3

The 'docker' directory contains a Dockerfile and instructions for making a local computationally reproducible version of the analysis. In addition, the Docker environment is set up to run automatically on a remote server via Github Actions, each time a change is made (i.e. on a 'commit' to the repo). The output document is then posted back to the main repository (sizeadaptation.pdf). If you want to make changes to the analysis and have these build automatically, you can fork the repository into your own account.

Production of the reproducible version of this manuscript was supported by an Enhancing Research Culture award from [Research England](https://www.ukri.org/councils/research-england/).

![autobuild](https://github.com/bakerdh/SizeAdaptation/workflows/autobuild/badge.svg)