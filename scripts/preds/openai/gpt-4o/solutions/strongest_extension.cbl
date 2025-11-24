       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRONGEST-EXTENSION.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 TEMP-STRENGTH PIC S9(4) COMP.
       01 MAX-STRENGTH PIC S9(4) COMP VALUE -9999.
       01 STRONGEST-INDEX PIC 9(4) COMP VALUE 1.
       01 CAPCOUNT PIC 9(4) COMP.
       01 SMCOUNT PIC 9(4) COMP.
       01 TEMP-CHAR PIC X.

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
       
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 4
               MOVE ZERO TO CAPCOUNT
               MOVE ZERO TO SMCOUNT
               PERFORM VARYING NI2 FROM 1 BY 1 UNTIL NI2 > LENGTH OF L-EXTENSIONS(NI)
                   MOVE L-EXTENSIONS(NI)(NI2:1) TO TEMP-CHAR
                   IF TEMP-CHAR >= "A" AND TEMP-CHAR <= "Z"
                       ADD 1 TO CAPCOUNT
                   ELSE IF TEMP-CHAR >= "a" AND TEMP-CHAR <= "z"
                       ADD 1 TO SMCOUNT
                   END-IF
               END-PERFORM
               SUBTRACT SMCOUNT FROM CAPCOUNT GIVING TEMP-STRENGTH
               IF TEMP-STRENGTH > MAX-STRENGTH
                   MOVE TEMP-STRENGTH TO MAX-STRENGTH
                   MOVE NI TO STRONGEST-INDEX
               END-IF
           END-PERFORM

           STRING L-CLASS-NAME DELIMITED BY SPACE
                  "."
                  L-EXTENSIONS(STRONGEST-INDEX) DELIMITED BY SPACE
                  INTO RESULT

           GOBACK.
       END PROGRAM STRONGEST-EXTENSION.
