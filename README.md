# README #

Electrical Analytics prototype user interface based on the R Shiny framework.

### How do I get set up? ###

Required R packages:
* "shiny"
* "ggplot2"
* "RPostgreSQL"
* "lubridate"
* "ggthemes"
* "ggTimeSeries"
* "gridExtra"
* "DT"

After installing R, in the interpreter run:

    install.packages("shiny")

Then navigate to this repo's folder and run:

    runApp('.')
    
The first lines of ``ui.R`` should install any missing packages.

If that doesn't work, use ``install.packages()`` to manually install the packages listed above.

On linux, ``libpq-dev`` is required for RPostgreSQL, so (for ubuntu-like distros) run:

    sudo apt install libpq-dev

For local TCD access, open a tunnel to the database with this command:

    ssh -L 9000:localhost:5432 [username]@transglobal.cloud.tilaa.com
