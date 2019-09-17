#!/bin/bash -l

# Exit on error.
set -e

# Get the release (if passed to docker)
release="${1:-untagged}"

echo "Building and labelling with release '$release'"

# Install musescore - this has to be done at runtime, rather than when building
# the docker image, as it requires fuse.
chmod a+x MuseScore-3.1.0-x86_64.AppImage
./MuseScore-3.1.0-x86_64.AppImage install

# Set our locale, and XDG_RUNTIME direcotry
export LC_ALL=$(locale -a | grep UTF-8)
export XDG_RUNTIME_DIR=$(pwd)

# Create a virtual x screen for musescore
Xvfb :99 -screen 0 1024x768x24 +extension GLX +render -noreset >> xsession.log 2>&1 &
export DISPLAY=:99

# Generate the pdfs with the python script
rm -rf build
python3 make_books.py --source_d src --book_d books --release_name $release
python3 make_books.py --source_d drafts --book_d draft_books --release_name "$release-drafts"

for f in $(ls draft_books); do
    mv draft_books/$f draft_books/draft_$f
done

echo "Generated all books!"