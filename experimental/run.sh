#!/bin/bash 

mscore_install="${1:-"/home/adam/personal/MuseScore/install"}"
echo "Musescore install: ${mscore_install}"

mscore_bin=${mscore_install}/bin/mscore
mscore_share=${mscore_install}/share/mscore-3.4/plugins/
echo "Musescore bin: ${mscore_bin}"
echo "Musescore share: ${mscore_share}"

cp scripts/* ${mscore_share}/

${mscore_bin} src/Splanky.mscx -d -p transposer_0.qml -o ./build/Splanky-Bb.mscx