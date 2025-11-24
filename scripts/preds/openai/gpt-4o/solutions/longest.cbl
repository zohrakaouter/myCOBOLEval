       IDENTIFICATION DIVISION.
       PROGRAM-ID. LONGEST.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 MAX-LENGTH PIC 9(3) VALUE 0.
       01 TEMP-LENGTH PIC 9(3) VALUE 0.
       01 TEMP-STRING PIC X(100).
       
       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRINGS OCCURS 3 TIMES INDEXED BY NI PIC X(3).
           05 RESULT PIC X(100).

      * Out of list of strings, return the longest one. Return the first one in case of multiple
      * strings of the same length.
      * >>> longest(['a', 'b', 'c'])
      * 'a'
      * >>> longest(['a', 'bb', 'ccc'])
      * 'ccc'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 3
               EVALUATE TRUE
                   WHEN L-STRINGS(NI) = SPACES
                       CONTINUE
                   WHEN OTHER
                       STRING L-STRINGS(NI) DELIMITED BY X'00' INTO TEMP-STRING
                       UNSTRING TEMP-STRING DELIMITED BY SPACE INTO TEMP-STRING
                       COMPUTE TEMP-LENGTH = LENGTH OF TEMP-STRING
                       
                       IF TEMP-LENGTH > MAX-LENGTH
                           MOVE TEMP-LENGTH TO MAX-LENGTH
                           MOVE L-STRINGS(NI) TO RESULT
                       END-IF
               END-EVALUATE
           END-PERFORM.
           GOBACK.
       END PROGRAM LONGEST.
