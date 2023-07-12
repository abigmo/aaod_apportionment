# aaod_apportionment

Implementation of the AAOD apportionment following Bahadur et al. (2012) applied to AERONET v3 data.
The code is written in R programming language and released alongside a scientific article Bigi et al. (2023). It allows to apportion AAOD to Black Carbon, Brown Carbon and Dust.

**How to use it**: set the AAE for BC, BrC and Dust at the several wavelenghts: the mean and the standard deviation of each AAE are needed (some samples are provided). 1E4 samples which will be randomly drawn from a normal distribution featured by these value. The code uses a "poor's man parallelization" based on `doParallel`: by default the code is using all available cores besides one (change this option if needed). The code includes the command to generate the plot S3 and S6 in Bigi et al. (2023).

**How to acknowledge this code**:
Only use lower case letters when mentioning `aaod_apportionment`. Always include the version number: ideally, you should also include the Digital Object Identifier (DOI) associated to the specific release you have been using:

*aaod_apportionment*   release 1.0.0   DOI:10.258/zenodo.12341234

If `aaod_apportionment` was useful for your research, please cite the dedicated article:

*Bigi et al.(2023)*

`aaod_apportionment` relies on external R libraries that require & deserve to be acknowledged in their own right. The following LaTeX blurb is one way to do so:

```This research has made use of \textit{aaod_apportionment v1.0.0} \citep[DOI:10.xxxxx][]{Bigi_2023}. \textit{aaod_apportionment} relies on the following R packages: \textit{foreach}, \textit{doParallel}```


**References**

Bigi, A., Veratti, G., Andrews, E., Collaud Coen, M., Guerrieri, L., Bernardoni, V., Massabò, D., Ferrero, L., Teggi, S., Ghermandi, G: Black Carbon and Brown Carbon absorption by in-situ filter-based photometer and ground-based sun-photometer in an urban atmosphere. Atmos. Chem. Phys., XX,  

Bahadur, R., Praveen, P. S., Xu, Y., and Ramanathan, V.: Solar absorption by elemental and brown carbon determined from spectral observations, Proceedings of the National Academy of Scicences, 109, 17 366–17 371, https://doi.org/10.1073/pnas.1205910109, 2012.
