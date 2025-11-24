       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNT-DISTINCT-CHARACTERS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTERS.
           05 WS-I PIC 9(3) VALUE 0.
           05 WS-J PIC 9(3) VALUE 0.
           05 WS-COUNT PIC 9(3) VALUE 0.
           05 WS-LENGTH PIC 9(3) VALUE 0.
           05 WS-FOUND PIC 9 VALUE 0.
       01 WS-CHARS.
           05 WS-CHAR PIC X.
           05 WS-COMPARE-CHAR PIC X.
       01 WS-UPPER-STRING PIC X(100).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRING PIC X(100).
           05 RESULT PIC S9(10).

      * Given a string, find out how many distinct characters (regardless of case) does it consist of
      * >>> count_distinct_characters('xyzXYZ')
      * 3
      * >>> count_distinct_characters('Jerry')
      * 4
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE FUNCTION UPPER-CASE(L-STRING) TO WS-UPPER-STRING
           INSPECT WS-UPPER-STRING TALLYING WS-LENGTH FOR CHARACTERS
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LENGTH
               MOVE WS-UPPER-STRING(WS-I:1) TO WS-CHAR
               SET WS-FOUND TO 0
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-I - 1
                   MOVE WS-UPPER-STRING(WS-J:1) TO WS-COMPARE-CHAR
                   IF WS-CHAR = WS-COMPARE-CHAR THEN
                       SET WS-FOUND TO 1
                   END-IF
               END-PERFORM
               IF WS-FOUND = 0 THEN
                   ADD 1 TO WS-COUNT
               END-IF
           END-PERFORM
           MOVE WS-COUNT TO RESULT
           GOBACK.
       END PROGRAM COUNT-DISTINCT-CHARACTERS.
