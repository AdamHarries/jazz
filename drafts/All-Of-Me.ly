\version "2.18.2"

\header {
  title = "All Of Me"
  composer = "Gerald Marks/Seymour Simons"
}

#(define (left-parenthesis-ignatzek-chord-names in-pitches bass  
inversion context)
(markup #:line ("( " (ignatzek-chord-names in-pitches bass inversion  
context))))

#(define (right-parenthesis-ignatzek-chord-names in-pitches bass  
inversion context)
(markup #:line ((ignatzek-chord-names in-pitches bass inversion  
context) " )"))) 

LPC = { \set chordNameFunction = #left-parenthesis-ignatzek-chord-names }
RPC = { \set chordNameFunction = #right-parenthesis-ignatzek-chord-names }
NPC = { \unset chordNameFunction } 

global = {
  \time 4/4
}


theme = { 
  \key c \major
  <<
          
  \chords \with {alignAboveContext = "main" } { 
    \set chordChanges = ##t
    c1:maj7 c1:maj7 e1:7 e1:7
    a1:7 a1:7 d1:m d1:m
    e1:7 e1:7 a1:m a1:m  
    d1:7 d1:7 d1:m7 g1:7
    c1:maj7 c1:maj7 e1:7 e1:7
    a1:7 a1:7 d1:m d1:m
    f1 f1:m c2:maj7 e2:m7 a1:7
    d1:m7 g1:7 \LPC c2:6 \NPC e2:m7 \RPC a1:7
  }             
   \relative c'' {  
      c4 g8 e~ e2~ e2 \tuplet 3/2 { c'4 d c} b gis8 e8~ e2~ e1 \break 
      a4. g8 e2~ e4 dis \tuplet 3/2 { e4 bes' a} g2 f2~ f1 \break
      e4. ees8 d2~ d2 \tuplet 3/2 { e4 gis b} d2 c2~ c1 \break
      b4. bes8 a2~ a2 \tuplet 3/2 { a4 d b} a1 b1 \break
      c4 g8 e~ e2~ e2 \tuplet 3/2 { c'4 d c} b gis8 e8~ e2~ e1 \break 
      a4. g8 e2~ e4 dis \tuplet 3/2 { e4 bes' a} g2 f2~ f1 \break
      d'2 c4 b4 d2. c4 b2 e,4 g4 b2. a4 \break
      c2 a4 c4 e2 e2 c1~ c1
   }

  >>
} 

altoSax =  {
  \global
  \transposition es
  % Music follows here.
  \transpose es c {
      \theme 
  }  
}

clarinet = {
  \global
  \transposition bes
  % Music follows here.
  \transpose bes c' {
      \theme 
  }  
}

trumpetBb = {
  \global
  \transposition bes
  % Music follows here.
  \transpose bes c'  {
      \theme 
  }  
}

piano =  {
  \global
  % Music follows here.
  \theme
  
}

jazzGuitar = {
  \global
  % Music follows here.
  \theme
  
}

drum = \drummode {
  \global
  % Drums follow here.
  \theme   
}

altoSaxPart = \new Staff = "main" \with {
  instrumentName = "Alto Sax"
  midiInstrument = "alto sax"  
} \altoSax

clarinetPart = \new Staff = "main" \with {
  instrumentName = "Clarinet"
  midiInstrument = "clarinet"
} \clarinet

trumpetBbPart = \new Staff = "main" \with {
  instrumentName = "Trumpet in Bb"
  midiInstrument = "trumpet"
} \trumpetBb

pianoPart = \new Staff = "main" \with {
  instrumentName = "Piano"
  midiInstrument = "acoustic grand"
} \piano

jazzGuitarPart = \new Staff = "main"  \with {
  midiInstrument = "electric guitar (jazz)"
  instrumentName = "Jazz guitar"
} \jazzGuitar

drumsPart = \new DrumStaff = "main" \with {
  \consists "Instrument_name_engraver"
  instrumentName = "Drums"
} \drum

\score {
  <<
    \altoSaxPart
    \clarinetPart
    \trumpetBbPart
    \pianoPart
    \jazzGuitarPart
    \drumsPart
  >>
  \midi {
    \tempo 4=130
  }
}

\book {
  \bookpart {
   \score { \altoSaxPart }  
  }
 \bookpart {
 \score { \clarinetPart} 
 }
 \bookpart {
 \score {\trumpetBbPart }
 }\bookpart {
 \score { \pianoPart } 
 }
 \bookpart {
 \score { \jazzGuitarPart}
 }
 \bookpart {
 \score { \drumsPart}
 }
}
