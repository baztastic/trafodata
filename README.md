# README #

Electrical Analytics prototype user interface based on the R Shiny framework.

### Required R packages: ###

* ``"shiny"``
* ``"ggplot2"``
* ``"lubridate"``
* ``"ggthemes"``
* ``"devtools"``
* ``"ggTimeSeries"``  - Use ``devtools::install_github('Ather-Energy/ggTimeSeries')``
* ``"DT"``
* ``"RPostgreSQL"``
* ``"ggpmisc"``
* ``"dplyr"``
* ``"shinyWidgets"``
* ``"ggalt"``  - Use ``devtools::install_github('hrbrmstr/ggalt')``
* ``"dygraphs"``
* ``"shinyjs"``
* ``"RcppTOML"``

### How do I get set up? ###

1. Install R from [r-project.org](https://www.r-project.org/).

2. libcurl and libproj dev packages are required under ubuntu. Install if necessary.

3. After installing R, clone this repo and install the dependencies using:

        R -f install_deps.R

4. If that doesn't work, use ``install.packages([package_name])`` to manually install the packages listed above. See the note for ``ggTimeSeries`` and ``ggalt``.

5. On linux, ``libpq-dev`` is required for RPostgreSQL, so (for ubuntu-like distros) run:

        sudo apt install libpq-dev

6. For local TCD access, open a tunnel to the database with this command:

        ssh -L 9000:localhost:5432 [username]@transglobal.cloud.tilaa.com

7. Run the app:

* From the interpreter use ``shiny::runApp('path/to/repo')``
* From the command line use ``R -e "shiny::runApp('path/to/repo', port=4815, launch.browser=TRUE)"``

Note, the password for my db read-only user is hard-coded into ``baztools.R`` so please don't share it with anyone.
