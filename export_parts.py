#!/usr/bin/python3

# Obtained from https://github.com/klutt/klutt-musescore-tools
# Adapted/cleaned up further from there.

mscore = "musescore-portable"
import sys
import os
import glob
import json
import subprocess
from operator import itemgetter
from pprint import pprint
import xml.etree.ElementTree as et


def get_key(instrument):
    if instrument == "B♭ Trumpet":
        return "Bb"
    elif instrument == "Alto Sax":
        return "Eb"
    elif instrument == "Alto Saxophone":
        return "Eb"
    elif instrument == "Piano":
        return "C"
    print("Didn't recognise instrument: " + instrument)
    exit


# Test to see whether we should create a file, based on whether it exists,
# and whether it is newer/older than the source file
# Essentially, this is the same behaviour as Make :D
def should_createf(sourcef, *targetfs):
    for tf in targetfs:
        print("Checking tf: " + tf)
        if not os.path.exists(tf):
            return True
        sourcef_time = os.path.getmtime(sourcef)
        targetf_time = os.path.getmtime(tf)
        if sourcef_time > targetf_time:
            return True
    return False


def generate(path, tmpd, outd, bookd):
    track_info = {}
    track_info['pdfs'] = {}
    with open(os.path.join(tmpd, 'log.txt'), 'w+') as logf:
        basename, fileExtention = os.path.splitext(os.path.basename(path))

        print("Basename %s" % basename)
        print("Extension %s" % fileExtention)

        tmpf = os.path.join(tmpd, basename)
        outf = os.path.join(outd, basename)

        mscx = tmpf + ".mscx"

        if should_createf(path, mscx):
            if fileExtention not in [".mscx", ".mscz"]:
                print("Unknown file extention: " + fileExtention)
                exit

            if fileExtention == ".mscz":
                proc = subprocess.Popen([mscore, "-o", mscx, "-P", path],
                                        stdout=logf,
                                        stderr=logf)
                proc.wait()

        tree = et.parse(mscx)

        scoreList = []

        for score in tree.iter('Score'):
            scoreList.append(score)

        data = []
        partList = []

        for textElem in scoreList[0].iter("Text"):
            style = textElem.find("style")
            subtext = textElem.find("text")
            if style.text == "Title":
                track_info['title'] = subtext.text.strip()
                print("Found title: " + track_info['title'])
                break

        for i in range(len(scoreList) - 1):
            name = ""
            key = ""
            for trackName in scoreList[i + 1].iter('trackName'):
                name = trackName.text
                key = get_key(name)
                partList.append(trackName)
                print("\tFound part: " + name)
                break

            tree.getroot().remove(scoreList[i])
            tree.getroot().append(scoreList[i + 1])

            partFileBase = tmpf + "_" + key
            partFile = partFileBase + ".mscx"
            pdfFile = outf + "_" + key + ".pdf"

            track_info['pdfs'][key] = pdfFile

            entry = {}
            entry['in'] = partFile
            entry['out'] = pdfFile
            data.append(entry)
            tree.write(partFile)

        print("Generating pdfs...")
        print("Generated: ")
        pprint(track_info)

        if should_createf(mscx, *track_info['pdfs'].values()):
            jsonfile = tmpf + '.json'
            with open(jsonfile, 'w') as outfile:
                json.dump(data, outfile)

            proc = subprocess.Popen([mscore, "-j", jsonfile],
                                    stdout=logf,
                                    stderr=logf)
            proc.wait()

        print("")

        return track_info


def main():

    score_folder = "MuseScore"

    if len(sys.argv) < 2:
        print("Usage: getPartNames.py <score_folder>")
        print("Defaulting to folder 'MuseScore'\n")
    else:
        score_folder = sys.argv[1]

    if not os.path.exists("tmp"):
        os.makedirs("tmp")

    # Clear the log file, if there is one.
    with open(os.path.join("tmp", 'log.txt'), 'w') as logf:
        logf.writelines("")

    if not os.path.exists("pdf"):
        os.makedirs("pdf")

    if not os.path.exists("books"):
        os.makedirs("books")

    # get a list of musescore files
    msfiles = glob.glob(score_folder + "/*.mscz")

    # Generate PDFs, and get a list of charts from a glob.
    charts = [generate(f, "tmp", "pdf", "books") for f in msfiles]
    charts = sorted(charts, key=itemgetter('title'))

    # Generate list of title/PDF pairs for LaTeX
    pairs = {}
    pairs['Eb'] = {}
    pairs['Bb'] = {}
    pairs['C'] = {}

    for c in charts:
        title = c['title']
        print("Chart: " + title)
        for k in c['pdfs']:
            pairs[k][title] = c['pdfs'][k]

    print(json.dumps(pairs))

    for k in pairs:
        print("Key : " + k)
        for name in pairs[k]:
            print("\t" + name + " - " + pairs[k][name])


if __name__ == "__main__":
    main()
