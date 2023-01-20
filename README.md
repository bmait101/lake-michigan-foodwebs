# Lake Michigan Food Web Architecture

<img align="right" src="img/Lake_Michigan_bathymetry_map.png" alt="lm-bathy" width="300" style="margin-top: 20px">

Repo for Lake Michigan Food Web Architecture project. 

The landscape theory of food web architecture (LTWFA; [Rooney et al. 2008](https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1461-0248.2008.01193.x)) describes relationships among animal body size, mobility, and coupling of energy channels from heterogeneous habitats. As such, it can help predict which species are likely to play a critical role in connecting different parts of a food web. However, empirical tests of the LTWFA are rare and support differs among terrestrial, freshwater, and marine systems. Further. the LTWFA has not yet been tested in the Great Lakes and it remains unclear whether the theory applies in highly invaded ecosystems such as Lake Michigan. In this project, we will test predictions of the LTWFA by quantifying links among animal body size, trophic position, and the coupling of energy channels from heterogeneous habitats using carbon and nitrogen stable isotope ratio data from organisms throughout the Lake Michigan food web.

Collaborators: 

* [Fred Keppeler](https://fkeppeler.github.io/) (UW-Madison CFL)
* [Olaf Jensen](https://jensen.limnology.wisc.edu/) (UW-Madison CFL)
* [Joel Hoffman](https://www.researchgate.net/profile/Joel-Hoffman) (EPA)
* [Bo Bunnell](https://www.usgs.gov/staff-profiles/david-b-bunnell) (USGS GLSC)
* [Scott McNaught](http://people.se.cmich.edu/mcnau1as/) (Central Michigan University)


## Folders 

**R:** 
- Contains .R files to clean data, run analyses, and make figures. 
- Run R/00_prep.R before running any other scripts.
- R/03_tp_models takes ~12 hours to run (individual models ~ 8 hours)
- R/05_brms_models takes ~ 2 hours to run

**data-raw**
- Contains raw, flat data files

**out**
- Contains output created using script in the R folder
- Because they are too large, estimate model objects from tRophicposion and brms are not uploaded. They can be downloaded [here](https://drive.google.com/drive/folders/1oCmnLMbQSnCN5oI8_hBjcKneRu_XxpSW?usp=sharing). 

**img**

- images used in project