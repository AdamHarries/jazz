[![Build Status](https://travis-ci.org/AdamHarries/jazz.svg?branch=master)](https://travis-ci.org/AdamHarries/jazz)

# Jazz

Scores &amp;cetera for Stompin' At Summerhall.

This is now also dockerised, so if you have docker installed, it should be as easy as running the `run_docker.sh` script to generate pdfs (which should then be copied out of the container).

## Adding charts 

Adding charts is as easy as creating a copy of the template chart (in `resources/Template.mscx`), saving it as the name of the chart that you want to create (e.g. `src/Rhythm_Changes.mscx`), then editing the new file (i.e. `src/Rhythm_Changes.mscx`). I personally find it easiest to edit in one instrument, get all the notes, chords, etc correct and then copy to the other instruments in the score. Note that spacing (e.g. keeping each line to four bars, like the real books) needs to be done on an instrument-by-instrument basis. 