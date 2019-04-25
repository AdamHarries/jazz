#!/bin/sh
pdftk Parts/*Piano.pdf cat output concert.pdf verbose
pdftk Parts/*Trumpet.pdf cat output bb.pdf verbose
pdftk Parts/*Alto*.pdf cat output eb.pdf verbose
