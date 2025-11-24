       IDENTIFICATION DIVISION.
       PROGRAM-ID. LONGEST.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-MAX-LEN PIC 9(3) VALUE 0.
       01 WS-CURRENT-LEN PIC 9(3).
       01 WS-I PIC 9(3).

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
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               COMPUTE WS-CURRENT-LEN = FUNCTION LENGTH(L-STRINGS(WS-I))
               IF WS-CURRENT-LEN > WS-MAX-LEN
                   MOVE WS-CURRENT-LEN TO WS-MAX-LEN
                   MOVE L-STRINGS(WS-I) TO RESULT
               END-IF
           END-PERFORM
           GOBACK.
       END PROGRAM LONGEST.
