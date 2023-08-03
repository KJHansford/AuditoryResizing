These materials are a computationally reproducible version of the paper:

Hansford, K.J., Baker, D.H., McKenzie, K.J. & Preston, C.E. (2023). Multisensory Integration and Proprioceptive Drift During Resizing Illusions. Preprint: https://doi.org/10.31234/osf.io/n56ha

The file AudioManuscript.Rmd is an R markdown file that will perform all analyses and figure creation, and produce a pdf version of the manuscript.

The full repository can be downloaded (cloned), and contains all the required data files. 
However if any data files are missing the code will attempt to download them from the OSF repository for this project:
https://osf.io/b9s48/

The 'docker' directory contains a Dockerfile and instructions for making a local computationally reproducible version of the analysis. In addition, the Docker environment is set up to run automatically on a remote server via Github Actions, each time a change is made (i.e. on a 'commit' to the repo). The output document is then posted back to the main repository (AudioManuscript.pdf). If you want to make changes to the analysis and have these build automatically, you can fork the repository into your own account.

![autobuild](https://github.com/KJHansford/AuditoryResizing/workflows/autobuild/badge.svg)
