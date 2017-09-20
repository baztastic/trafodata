# README #

Electrical Analytics prototype user interface based on the R Shiny framework.

### Required R packages: ###

* ``"shiny"``
* ``"ggplot2"``
* ``"RPostgreSQL"``
* ``"lubridate"``
* ``"ggthemes"``
* ``"ggTimeSeries"``
* ``"gridExtra"``
* ``"DT"``

### How do I get set up? ###

1. After installing R, in the interpreter run:

        install.packages("shiny")

2. Clone this repo and navigate its folder and run:

        runApp('.')
    
3. The first lines of ``ui.R`` should install any missing packages.

4. If that doesn't work, use ``install.packages()`` to manually install the packages listed above.

5. On linux, ``libpq-dev`` is required for RPostgreSQL, so (for ubuntu-like distros) run:

        sudo apt install libpq-dev

6. For local TCD access, open a tunnel to the database with this command:

        ssh -L 9000:localhost:5432 [username]@transglobal.cloud.tilaa.com

Note, the password for my db read-only user is hard-coded into ``baztools.R`` so please don't share it with anyone.