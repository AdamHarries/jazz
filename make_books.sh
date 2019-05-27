#!/bin/bash
if [ -d tmp ]; then
    rm -rf tmp
fi
mkdir -p tmp

for f in $(ls MuseScore); do
    cp "MuseScore/$f" tmp
    pushd tmp
    ../export_parts.py "$f"
    popd
    
    break
done

pdftk tmp/*Piano.pdf cat output concert.pdf verbose
pdftk tmp/*Trumpet.pdf cat output bb.pdf verbose
pdftk tmp/*Alto*.pdf cat output eb.pdf verbose

# rm -rf tmp