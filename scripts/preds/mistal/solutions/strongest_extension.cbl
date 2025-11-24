       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRONGEST-EXTENSION.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-MAX-STRENGTH PIC S9(4) VALUE -9999.
           05 WS-CURRENT-STRENGTH PIC S9(4).
           05 WS-CAP-COUNT PIC 9(3).
           05 WS-SM-COUNT PIC 9(3).
           05 WS-STRONGEST-EXTENSION PIC X(4).
           05 WS-I PIC 9(3).
           05 WS-J PIC 9(3).
           05 WS-CHAR PIC X.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-CLASS-NAME PIC X(100).
           05 L-EXTENSIONS OCCURS 4 TIMES INDEXED BY NI PIC X(4).
           05 RESULT PIC X(100).

      * You will be given the name of a class (a string) and a list of extensions.
      * The extensions are to be used to load additional classes to the class. The
      * strength of the extension is as follows: Let CAP be the number of the uppercase
      * letters in the extension's name, and let SM be the number of lowercase letters
      * in the extension's name, the strength is given by the fraction CAP - SM.
      * You should find the strongest extension and return a string in this
      * format: ClassName.StrongestExtensionName.
      * If there are two or more extensions with the same strength, you should
      * choose the one that comes first in the list.
      * For example, if you are given "Slices" as the class and a list of the
      * extensions: ['SErviNGSliCes', 'Cheese', 'StuFfed', 'Cehese'] then you should
      * return 'Slices.SErviNGSliCes' since 'SErviNGSliCes' is the strongest extension
      * (its strength is -1).
      * Example:
      * for Strongest_Extension('my_class', ['AA', 'Be', 'CC', 'eB']) == 'my_class.AA'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE 0 TO WS-CAP-COUNT WS-SM-COUNT
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > FUNCTION LENGTH(L-EXTENSIONS(WS-I))
                   MOVE L-EXTENSIONS(WS-I)(WS-J:1) TO WS-CHAR
                   IF WS-CHAR >= 'A' AND WS-CHAR <= 'Z'
                       ADD 1 TO WS-CAP-COUNT
                   ELSE IF WS-CHAR >= 'a' AND WS-CHAR <= 'z'
                       ADD 1 TO WS-SM-COUNT
                   END-IF
               END-PERFORM
               COMPUTE WS-CURRENT-STRENGTH = WS-CAP-COUNT - WS-SM-COUNT
               IF WS-CURRENT-STRENGTH > WS-MAX-STRENGTH
                   MOVE WS-CURRENT-STRENGTH TO WS-MAX-STRENGTH
                   MOVE L-EXTENSIONS(WS-I) TO WS-STRONGEST-EXTENSION
               END-IF
           END-PERFORM

           STRING L-CLASS-NAME DELIMITED BY SIZE
                  '.' DELIMITED BY SIZE
                  WS-STRONGEST-EXTENSION DELIMITED BY SIZE
           INTO RESULT
           END-STRING

           GOBACK.
       END PROGRAM STRONGEST-EXTENSION.
