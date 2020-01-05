// def tests: Unit = {
//   // basic features (15)
//   test(run("""{+ 1 2}"""), "3")
//   test(run("""{- {+ 1 2} 3}"""), "0")
//   test(run("""{fun {x} x}"""), "function")
//   test(run("""{fun {x} {+ x 1}}"""), "function")
//   test(run("""{{{fun {f} {fun {x} {f x}}} {fun {x} {+ x 1}}} 42}"""), "43")
//   test(run("""{{{fun {g} {fun {y} {g y}}} {fun {k} {- k 1}}} 42}"""), "41")
//   test(run("""{{fun {x} {{{fun {x} {fun {y} x}} 2} 42}} 1}"""), "2")
//   test(run("""{{fun {a} {{{fun {a} {fun {b} a}} 3} 7}} 5}"""), "3")
//   test(run("""{newbox 1}"""), "box")
//   test(run("""{newbox {fun {x} x}}"""), "box")
//   test(run("""{setbox {newbox 1} 2}"""), "2")
//   test(run("""{setbox {newbox 2} {fun {x} x}}"""), "function")
//   test(run("""{{fun {b} {openbox b}} {newbox 1}}"""), "1")
//   test(run("""{{fun {b} {seqn {setbox b 42} {openbox b}}} {newbox 1}}"""), "42")
//   testExc(run("""{openbox 1}"""), "")

//   // improving sequences (10)
//   test(run("""{seqn 1}"""), "1")
//   test(run("""{seqn {fun {x} x}}"""), "function")
//   test(run("""{seqn 1 2 3}"""), "3")
//   test(run("""{seqn 1 {+ 1 2} {newbox 1}}"""), "box")
//   test(run("""{seqn 1 2 3 4 5}"""), "5")
//   test(run("""{seqn 1 {newbox 1} {fun {x} x} 3 {- 42 1}}"""), "41")
//   test(run("""{{fun {b} {seqn {openbox b}}} {newbox 1}}"""), "1")
//   test(run("""{{fun {b} {seqn {setbox b {+ 2 {openbox b}}}
//                               {setbox b {+ 3 {openbox b}}}
//                               {setbox b {+ 4 {openbox b}}}
//                               {openbox b}}}
//               {newbox 1}}"""), "10")
//   test(run("""{{{fun {f} {fun {b} {seqn {f b} {f b} {f b} {openbox b}}}} {fun {b} {setbox b {+ 1 {openbox b}}}}} {newbox 1}}"""), "4")
//   test(run("""{{{fun {f} {fun {b} {seqn {f b} {f b} {f b} {openbox b}}}} {fun {b} {setbox b {+ {openbox b} {openbox b}}}}} {newbox 1}}"""), "8")

//   // records (25)
//   test(run("""{rec}"""), "record")
//   test(run("""{rec {x 1} {y 2} {z 3}}"""), "record")
//   test(run("""{get {rec {x 1} {y 2} {z 3}} y}"""), "2")
//   test(run("""{get {rec {x 1} {y 2} {z 3}} z}"""), "3")
//   testExc(run("""{get {rec {x 1}} y}"""), "no such field")
//   testExc(run("""{get {rec {x 1}} a}"""), "no such field")
//   test(run("""{set {rec {x 1} {y 2} {z 3}} x 3}"""), "3")
//   test(run("""{set {rec {x 1} {y 2} {z 3}} y 10}"""), "10")
//   testExc(run("""{set {rec {x 1}} y 2}"""), "no such field")
//   testExc(run("""{set {rec {x 1}} a 2}"""), "no such field")
//   test(run("""{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"""), "5")
//   test(run("""{{fun {r} {seqn {set r x 7} {get r x}}} {rec {x 42}}}"""), "7")
//   test(run("""{openbox {{fun {b} {get {rec {x {setbox b 3}} {y b}} y}} {newbox 1}}}"""), "3")
//   test(run("""{openbox {{fun {b} {get {rec {x {setbox b 42}} {y b}} y}} {newbox 1}}}"""), "42")
//   testExc(run("""{seqn {rec {a 1}} {get {rec {b 2}} a}}"""), "no such field")
//   testExc(run("""{seqn {rec {a 1}} {get {rec {x 2}} a}}"""), "no such field")
//   testExc(run("""{{fun {x} {get {rec} b}} {rec {b 2}}}"""), "no such field")
//   testExc(run("""{{fun {x} {get {rec {a 1}} b}} {rec {b 2}}}"""), "no such field")
//   testExc(run("""{get {fun {x} x} x}"""), "")
//   testExc(run("""{get {fun {x} {rec {x 1}}} x}"""), "")
//   testExc(run("""{set {fun {x} x} x 1}"""), "")
//   testExc(run("""{set {fun {x} {rec {x 2}}} x 1}"""), "")
//   testExc(run("""{get {set {rec {a 3}} a 4} a}"""), "")
//   testExc(run("""{get {set {rec {b 3}} b 4} b}"""), "")
//   test(run("""{{fun {r} {seqn {set r x r}
//                               {get {get {get {get {get {get r x} x} x} x} x} y}}}
//                {rec {x 42} {y 1}}}"""), "1")
// }
