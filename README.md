[![Build Status (travis)](https://secure.travis-ci.org/dtekcth/DtekPortalen.png)](http://travis-ci.org/dtekcth/DtekPortalen)

This project is a webpage written in yesod. It is the home page of
the "computer department" of Chalmers University of Technology.

Installation and maintaining instructions are on the github wiki,
but since this is intended only for the local students of Chalmers
it's in Swedish.

# Installation

Denna installationsguide är menad att vara komplett och inte anta några
förkunskaper. Om du känner att nått saknas så gör en pull request! :)

## Reqs

GHC 7.8.3 and modern Cabal

    $ ghc --version
    The Glorious Glasgow Haskell Compilation System, version 7.8.3
    $ cabal --version
    cabal-install version 1.20.0.3

We use SQLite3 for dev and Postgres for production.

## Hacking

Get the code

    $ git clone https://github.com/dtekcth/DtekPortalen
    $ cd DtekPortalen

Install dependencies

    $ cabal sandbox init
    $ cabal install --only-dependencies

Build the portal

    $ cabal configure -fdev
    $ cabal build


# Interaktiv utveckling

Installera yesod CLI

    $ cabal install yesod-bin

Starta dev-server

    $ yesod devel

Och gå in på sidan <http://localhost:3000/>. Nu så ska sidan automatiskt
uppdateras när ni ändrar i någon `.hs`-fil.
