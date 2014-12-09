Minesweeper
===========

This repo contains an implementation of Minesweeper written in Haskell, for a group assignment in Trinity College Dublin's CS4012: Topics in Functional Programming module 2014.

## Requirements

The project requires you to install both the Glasgow Haskell Compiler and the Cabal package system. This can be done by installing the [Haskell Platform](https://www.haskell.org/platform/) or using homebrew by running `$ brew install ghc && brew install cabal`

## Setup

Create a cabal sandbox to isolate dependencies:

`$ cabal sandbox init`

Install all the project dependencies:

`$ cabal install --only-dependencies`

Build and run the project:

`$ cabal run`