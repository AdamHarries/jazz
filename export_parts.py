#!/usr/bin/python3

# Obtained from https://github.com/klutt/klutt-musescore-tools
# Adapted/cleaned up further from there.

mscore = "musescore-portable"


def main():
    import sys
    import os
    import json
    import subprocess
    import xml.etree.ElementTree as et

    if len(sys.argv) < 2:
        print("Usage: getPartNames.py <filename>")
        exit

    inFile = sys.argv[1]
    filename, fileExtention = os.path.splitext(inFile)
    print("Filename %s" % filename)
    print("Extension %s" % fileExtention)
    mscx = filename + ".mscx"

    if fileExtention not in [".mscx", ".mscz"]:
        print("Unknown file extention: " + fileExtention)
        exit

    if fileExtention == ".mscz":
        proc = subprocess.Popen(
            [mscore, "-o", mscx, "-P", inFile])
        proc.wait()

    tree = et.parse(mscx)

    scoreList = []

    for score in tree.iter('Score'):
        scoreList.append(score)

    data = []
    partList = []

    for i in range(len(scoreList)-1):
        name = ""
        for trackName in scoreList[i+1].iter('trackName'):
            name = trackName.text
            partList.append(trackName)
            break

        tree.getroot().remove(scoreList[i])
        tree.getroot().append(scoreList[i+1])

        partFileBase = filename + "-" + name
        partFile = partFileBase + ".mscx"
        entry = {}
        entry['in'] = partFile
        entry['out'] = partFileBase + ".pdf"
        data.append(entry)
        tree.write(partFile)

    jsonfile = filename + '.json'
    with open(jsonfile, 'w') as outfile:
        json.dump(data, outfile)

    proc = subprocess.Popen(
        [mscore, "-j", jsonfile])
    proc.wait()


if __name__ == "__main__":
    main()
