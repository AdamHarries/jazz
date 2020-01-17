      import QtQuick 2.0
      import MuseScore 3.0

      MuseScore {
            menuPath: "Plugins.Transposer"
            description: "Description goes here"
            version: "1.0"

            onRun: {
                  console.log("Hello Transposer")

                  console.log("Score title: " + curScore.title);

                  if (!curScore)
                        Qt.quit();

                  // transpose up a whole step for Bb instrument
                  // note: to get the key right, go up 3 half-steps and down 1
                  console.log("Original key: " + curScore.keysig);
                  for (var i = 3; i-- > 0; ) {
                        cmd("transpose-up");
                  }
                  cmd("transpose-down");
                  console.log("Transposed key: " + curScore.keysig);

                  // insert instrument name on first note
                  var cursor = curScore.newCursor();
                  cursor.staffIdx = 0;
                  cursor.voice = 0;
                  cursor.rewind(Cursor.SCORE_START);

                  var text = newElement(Element.INSTRUMENT_CHANGE);
                  text.autoplace = false;
                  text.placement = Placement.ABOVE;
                  text.text = "Bb Instrument";
                  text.fontSize = 12;
                  text.sizeSpatiumDependent = false;
                  cursor.add(text);
                  // offsets must be done AFTER adding
                  text.offsetX = -20;
                  text.offsetY = -18;

                  console.log("Staying Alive");
                  // Qt.quit();
            }
      }