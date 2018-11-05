#!/bin/sh

cd src
ocamldep *.ml *.mll >> Makefile
