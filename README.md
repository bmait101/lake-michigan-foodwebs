# Lake Michigan Food Web Architecture

<img align="right" src="img/fig01.jpg" alt="fig01-concept" width="300" style="margin-top: 20px">

Repo for Lake Michigan Food Web Architecture project. In review at **Ecology**. 

Abstract: The landscape theory of food web architecture (LTFWA) describes relationships among body size, trophic position, mobility, and energy channels that serve to couple heterogeneous habitats, which in turn promotes long-term system stability. However, empirical tests of the LTFWA are rare and support differs among terrestrial, freshwater, and marine systems. Further, it is unclear if the theory applies in highly altered ecosystems dominated by introduced species such as the Laurentian Great Lakes. Here, we provide an empirical test of the LTFWA by relating body size, trophic position, and the coupling of different energy channels using stable isotope data from species throughout the Lake Michigan food web. We found that body size was positively related to trophic position, but for a given trophic position, organisms predominately supported by pelagic energy had smaller body sizes than organisms predominately supported by nearshore benthic energy. We also found a gradual increase in the coupling of pelagic and nearshore energy channels with larger body sizes as well as higher trophic positions. Results suggest a hump-shaped trophic relationship between basal energy source and trophic position in large lakes where there is increased coupling of benthic and pelagic energy channels as trophic position increases, highlighting the role of body size and the importance of connectivity among habitats in structuring large lake food webs. However, important deviations from expectations are suggestive of how species introductions and other anthropogenic impacts can affect food web structure in large lakes. First, native top predators appear to be flexible couplers that may provide food web resilience, whereas introduced top predators may confer less stability as they specialize on a single energy pathway. Second, some smaller bodied prey fish and invertebrates, in addition to mobile predators, coupled energy from pelagic and nearshore energy channels, which suggests that some prey species may also be important integrators of energy pathways in the system. We conclude that patterns predicted by the LTFWA are to a large degree robust and persistent in the face of  species introductions and other anthropogenic stressors, but time-series evaluations are needed to fully understand the mechanisms that promote stability. 

## Folders 

**R** 
- Contains .R files to clean data, run analyses, and make figures. 
- Run R/00_prep.R before running any other scripts.

**data-raw**
- Contains raw, flat data files

**out**
- Contains output created using script in the R folder
- Because they are too large, estimate model objects from tRophicposion and brms are not uploaded. 

**img**
- images used in project