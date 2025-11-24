       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILTER-BY-PREFIX.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTERS.
           05 WS-I PIC 99 VALUE 0.
           05 WS-J PIC 99 VALUE 0.
           05 WS-PREFIX-LEN PIC 99.
           05 WS-MATCH-LEN PIC 99.
       01 WS-TEMP-STRING PIC X(100).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRINGS OCCURS 4 TIMES INDEXED BY NI PIC X(4).
           05 L-PREFIX PIC X(100).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC X(100).

      * Filter an input list of strings only for ones that start with a given prefix.
      * >>> filter_by_prefix(['abc', 'bcd', 'cde', 'array'], 'ab')
      * ['abc']
      * >>> filter_by_prefix(['abc', 'bcd', 'cde', 'array'], 'a')
      * ['abc', 'array']
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE 0 TO WS-J
           INSPECT L-PREFIX TALLYING WS-PREFIX-LEN FOR CHARACTERS
                   BEFORE INITIAL SPACE

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE L-STRINGS(WS-I) TO WS-TEMP-STRING
               INSPECT WS-TEMP-STRING TALLYING WS-MATCH-LEN
                       FOR CHARACTERS BEFORE INITIAL SPACE

               IF WS-MATCH-LEN >= WS-PREFIX-LEN AND
                  WS-TEMP-STRING(1:WS-PREFIX-LEN) = L-PREFIX(1:WS-PREFIX-LEN)
                   ADD 1 TO WS-J
                   MOVE WS-TEMP-STRING TO RESULT(WS-J)
               END-IF
           END-PERFORM

           GOBACK.
       END PROGRAM FILTER-BY-PREFIX.
