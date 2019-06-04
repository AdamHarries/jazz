#!/bin/sh -l

set -e
set -o pipefail

sh -c "echo $*"

# Install musescore to start off with.
chmod a+x MuseScore-3.1.0-x86_64.AppImage
./MuseScore-3.1.0-x86_64.AppImage install


# Set our locale, and XDG_RUNTIME direcotry
export LC_ALL=$(locale -a | grep UTF-8)
export XDG_RUNTIME_DIR=$(pwd)

# Create a virtual x screen for musescore
Xvfb :99 -screen 0 1024x768x24 +extension GLX +render -noreset >> xsession.log 2>&1 &
export DISPLAY=:99

# Generate the pdfs.
# python3 make_books.py


# The deployment aspects of this script are heavily borrowed from JasonEtco's
# upload-to-release script

# Ensure that the GITHUB_TOKEN secret is included
if [[ -z "$GITHUB_TOKEN" ]]; then
    echo "Set the GITHUB_TOKEN env variable."
    exit 1
fi

for f in $(ls books)
do
    path=books/$f
    
    # Prepare the headers
    AUTH_HEADER="Authorization: token ${GITHUB_TOKEN}"
    CONTENT_LENGTH_HEADER="Content-Length: $(stat -c%s "$path")"
    CONTENT_TYPE_HEADER="Content-Type: application/pdf"
    
    # Build the Upload URL from the various pieces
    RELEASE_ID="latest"
    FILENAME=$f
    UPLOAD_URL="https://uploads.github.com/repos/${GITHUB_REPOSITORY}/releases/${RELEASE_ID}/assets?name=${FILENAME}"
    echo "$UPLOAD_URL"
    
    # Upload the file
    curl \
    -sSL \
    -XPOST \
    -H "${AUTH_HEADER}" \
    -H "${CONTENT_LENGTH_HEADER}" \
    -H "${CONTENT_TYPE_HEADER}" \
    --upload-file "$path" \
    "${UPLOAD_URL}"
done