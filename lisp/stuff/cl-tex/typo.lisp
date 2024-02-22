(in-package #:cl-tex)

(defclass style ()
  ((name :accessor name :initform nil :initarg :name))
  (:documentation "Top-level style class."))




'(font-family
  (weight (thin 
           ultra-light 
           extra-light 
           light 
           (semi-light semi) 
           (book normal regular roman plain medium) 
           (demi semi-bold) 
           bold 
           (extra-bold extra) 
           heavy 
           black 
           extra-black 
           (ultra ultra-black)))
  (style (roman
          italic
          cursive
          script))
  (orientation (back-slanted
                upright
                slanted
                left-slanted
                right-slanted))
  (stretch (condensed
            normal
            extended))
  (size (tiny
         small
         scriptsize
         normal
         large
         very-large
         huge)))
(/ 127/5 70)

9000/27706
(length-unit ((:sp "Scaled point")
              (:pt "Point"            (65536 . :sp))
              (:pc "Pica"             (12 . :pt))
              (:dd "Didot"            (1238/1157 . :pt))
              (:cc "Cicero"           (12 . :dd))
              (:pf "Pied Francais"    (864 . :dd))
              (:pp "PostScript point" (7227/7200 . :pt))
              (:mm "Milimetre"        (360/127 . :pp))
              (:cm "Centimetre"       (10 . :mm))
              (:m  "Metre"            (1000 . :mm))
              (:in "Inch"             (72 . :pp))
              (:ex "")
              em
              tan
              deg
              rad))



||#