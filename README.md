# grolprep


## Synopsis

FCC GROL exam prep web application (Haskell)


## Description

grolprep is a web application for studying the FCC GROL questions
in preparation of taking the exams.

This is a flash-card style study tool. Students can choose from
Elements 1, 3 and 8 and can specify any subelement of those
three for specific study. Questions and answers can be randomly
presented. grolprep displays the images for questions with figures.

Additionally, simulations of the randomly-chosen exams can be
practiced with this software.

The source test data questions can be acquired
from the [FCC Commercial Operator License
site](http://wireless.fcc.gov/commoperators/eqp.html).

grolprep is written in Haskell using many libraries including:
cgi, Crypto, HDBC-sqlite3, hslogger, HTTP, split, xhtml. At this
time, no special web framework was used, just Network.CGI and
Text.XHtml


## Getting source

- Get the source with darcs: `$ darcs get http://hub.darcs.net/dino/grolprep`
- If you're just looking, [browse the source](http://hub.darcs.net/dino/grolprep)

And once you have it, building the usual way:

    $ stack build
    $ stack test


## Installation

To install, a script is included, run it with `--help` for more info

    $ cd /dir/containing/grolprep/source
    $ ./util/install.hs

Installation defaults to `/var/www/grolprep-x.y.z`. After
first-time installation, you will need to copy
`/var/www/grolprep.../resources/grolprep.conf` to `/etc/` and make
it readable by the web server group or user. To use the feedback
facility, reCAPTCHA keys will need to be added to this config file.

Runtime data (user sessions and feedback messages) is stored in
`/var/local/grolprep`. This directory will need to be created ahead
of time and permissions set as with `grolprep.conf`


## Using the web application

[Visit the live site](http://ui3.info/grolprep/bin/fcc-grol-prep.cgi) and start studying!


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>
