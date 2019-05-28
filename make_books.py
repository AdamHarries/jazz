#!/usr/bin/python3

# Obtained from https://github.com/klutt/klutt-musescore-tools
# Adapted/cleaned up further from there.

# Define the name of the 'musescore' program that we'll be calling at the command line.
mscore = "musescore-portable"

# Imports
import sys
import os
import glob
import json
import subprocess
from operator import itemgetter
from pprint import pprint
import xml.etree.ElementTree as et

# What are we naming our various keys?
eb_key_name = "eb"
bb_key_name = "bb"
c_key_name = "c"


def get_key(instrument):
    if instrument == "B♭ Trumpet":
        return bb_key_name
    elif instrument == "Alto Sax":
        return eb_key_name
    elif instrument == "Alto Saxophone":
        return eb_key_name
    elif instrument == "Piano":
        return c_key_name
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


def generate(path, tmpd, outd, bookd, logf):
    track_info = {}
    track_info['pdfs'] = {}
    with open(logf, 'w+') as logfo:
        basename, fileExtention = os.path.splitext(os.path.basename(path))

        print("Basename %s" % basename)
        print("Extension %s" % fileExtention)

        tmpf = os.path.join(tmpd, basename)
        outf = os.path.join(outd, basename)

        mscx = tmpf + ".mscx"

        if fileExtention not in [".mscx", ".mscz"]:
            print("Unknown file extention: " + fileExtention)
            exit

        if fileExtention == ".mscz":
            if should_createf(path, mscx):
                proc = subprocess.Popen([mscore, "-o", mscx, "-P", path],
                                        stdout=logfo,
                                        stderr=logfo)
                proc.wait()

        if fileExtention == ".mscx":
            mscx = path

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
                                    stdout=logfo,
                                    stderr=logfo)
            proc.wait()

        print("")

        return track_info


tex_header = """
%!TEX encoding = UTF-8 Unicode
\\documentclass{report}
\\usepackage{pdfpages}
\\usepackage{hyperref}
\\usepackage{fontspec}
\\setmainfont[Ligatures={Common,TeX}, Mapping=tex-ansi]{MuseJazzText}

\\renewcommand{\\contentsname}{Stompin' At Summerhall}

\\newcommand{\\chart}[1]{%
  \\par\\refstepcounter{section}% Increase section counter
  \\sectionmark{#1}% Add section mark (header)
  \\addcontentsline{toc}{section}{\\protect\\numberline{\\thesection}#1}% Add section to ToC
  % Add more content here, if needed.
}

\\begin{document}

\\tableofcontents

\\clearpage
"""

tex_footer = """
\\end{document}
"""


def generate_tex(tp_pairs):
    tex = ""
    # Explicit copy
    tex += tex_header
    for title, pdf in tp_pairs.items():
        tex += """
% {}
    \\chart{{{}}}
    \\includepdf[pages=-]{{{}}}
    """.format(title, title, pdf)
    tex += tex_footer
    return tex


def main():
    sourced = "MuseScore"
    buildd = "build"
    tmpd = os.path.join(buildd, "tmp")
    pdfd = os.path.join(buildd, "pdf")
    texd = os.path.join(buildd, "tex")
    bookd = "books"
    logf = os.path.join(buildd, "log.txt")

    if len(sys.argv) < 2:
        print("Usage: getPartNames.py <sourced>")
        print("Defaulting to folder 'MuseScore'\n")
    else:
        sourced = sys.argv[1]

    for d in [buildd, tmpd, pdfd, texd, bookd]:
        if not os.path.exists(d):
            os.makedirs(d)

    # Clear the log file, if there is one.
    with open(logf, 'w') as logfo:
        logfo.writelines("")

    # get a list of musescore files
    msfiles = glob.glob(sourced + "/*.msc[zx]")

    # Generate PDFs, and get a list of charts from a glob.
    charts = [generate(f, tmpd, pdfd, bookd, logf) for f in msfiles]
    charts = sorted(charts, key=itemgetter('title'))

    # Generate list of title/PDF pairs for LaTeX
    pairs = {}
    pairs[eb_key_name] = {}
    pairs[bb_key_name] = {}
    pairs[c_key_name] = {}

    for c in charts:
        title = c['title']
        for k in c['pdfs']:
            pairs[k][title] = c['pdfs'][k]

    for k in pairs:
        print("Key : " + k)
        print("\tGenerating tex links and imports")
        tex = generate_tex(pairs[k])
        texfile = os.path.join(texd, "{}.tex".format(k))
        with open(texfile, 'w') as f:
            f.write(tex)

        print("\tRunning xelatex")
        with open(logf, 'w+') as logfo:
            for i in range(2):
                proc = subprocess.Popen(
                    ["xelatex", "-output-directory", texd, texfile],
                    stdout=logfo,
                    stderr=logfo)
                proc.wait()

        print("\tCopying")
        subprocess.Popen([
            "cp",
            os.path.join(texd, "{}.pdf".format(k)),
            os.path.join(bookd, "{}.pdf".format(k))
        ]).wait()

    print("Done.")


if __name__ == "__main__":
    main()
