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

grolprep is currently being used by people all over the US, including
students of the Avionics program at the [Burlington Aviation
Technology School](http://kevaco.net/aviationtech/). Some of the
technology in this web application was already helpful in preparing
students for these tests in the form of the prior command-line
version, [fequiz-pl](http://ui3.info/d/proj/fequiz-pl.html).

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

    $ cabal configure
    $ cabal build
    $ cabal install


## Using the web application

[Visit the live site](http://ui3.info/grolprep/bin/fcc-grol-prep.cgi) and start studying!


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>
