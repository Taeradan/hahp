# HAHP

/!\ Work In Progress !

You should NOT use it in production.

## Introduction

This haskell lib is an implementation of AHP Algorithm - [Analytic Hierarchy Process, Wikipedia](https://en.wikipedia.org/wiki/Analytic_hierarchy_process) - defined by Thomas L. Saaty.

This algortihm is used to help decision making and can be used to make automatic decision.
AHP select the best alternative (choice) according to a global objective.

## Requirements

This software is written with Haskell.

To execute this code, you need :

* a haskell compiler with cabal : `haskell-platform` include both compiler, essentials libraries and `cabal`
* git to fetch sources files : `git clone https://github.com/Taeradan/hahp.git`

To produce PDF report : `cabal install pandoc`

## First launch - Demo

(TODO)

* `cabal run` : run demo samples and produce a console report.
* `make` : idem (run demo samples and produce a console report).
* `make pdf` : produce a report `out.pdf`

## How to use it ?

(TODO)


## How to start web API ?

* `strack build` : compile web server
* ` stack exe hahp-server` : launch service on 8081 port.

Start a web browser on :

* http://localhost:8081/ahp/trees
* http://localhost:8081/ahp/tree