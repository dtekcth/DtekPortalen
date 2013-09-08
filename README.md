[![Build Status (travis)](https://secure.travis-ci.org/dtekcth/DtekPortalen.png)](http://travis-ci.org/dtekcth/DtekPortalen)
[![Build Status (jenkins)](http://dtek.se:8080/job/DtekPortalen/badge/icon)](http://dtek.se:8080/job/DtekPortalen/)

This project is a webpage written in yesod. It is the home page of
the "computer department" of Chalmers University of Technology.

Installation and maintaining instructions are on the github wiki,
but since this is intended only for the local students of Chalmers
it's in Swedish.

## Installation

Denna installationsguide är menad att vara komplett och inte anta några
förkunskaper. Om du känner att nått saknas så gör en pull request! :)

### Förberedelser

Kolla ghc versioner, bra om ni har `>= 7.4.1`

    $ ghc --version
    The Glorious Glasgow Haskell Compilation System, version 7.6.3
    $ cabal --version
    cabal-install version 0.14.0
    using version 1.14.0 of the Cabal library

Installera [hsenv](https://github.com/Paczesiowa/hsenv)

    $ cabal update
    $ cabal install hsenv

### Hämta portalen

Clona

    $ CLONEURL=https://github.com/dtekcth/DtekPortalen # eller ssh om ni vill
    $ git clone $CLONEURL
    $ cd DtekPortalen

### Cabal sandbox (frivilligt, rekomenderas starkt)

Skapa en cabal-sandbox

    $ hsenv
    $ echo 'source .hsenv/bin/activate' > .env
    $ cd .

Notera att den sistan raderna troligen inte gör något för er om ni inte har
autoenv. Det gör inget, glöm bara inte bort att alltid skriva `source
.hsenv/bin/activate` när ni börjar jobba. Ni ska se att er `PS1` har ändrats.
Typ er prompt ska börja med texten `[hsenv]`

### Installera yesod och portalen

Nu, så installera rätt version av yesod-platformen:

    $ cabal install --force-reinstalls $(grep "yesod-platform\s*==" *.cabal | tr --delete ", ")

Sen så installera depedencies:

    $ cabal install --only-dependencies

Sen resten.

    $ cabal install

Notera att dessa tre steg har jag splittat upp medvetet bara föra
att felsökningen ska bli enklare.

### Interaktiv utveckling

Installera yesod CLI

    $ cabal install yesod-bin

Starta dev-server

    $ yesod devel

Och gå in på sidan <http://localhost:3000/>. Nu så ska sidan automatiskt
uppdateras när ni ändrar i någon `.hs`-fil.
