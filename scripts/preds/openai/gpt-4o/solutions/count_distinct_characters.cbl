       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNT-DISTINCT-CHARACTERS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 L-CHAR-SET PIC X(256) VALUE SPACES.
       01 I PIC 9(03) VALUE 1.
       01 J PIC 9(03).
       01 TEMP-CHAR PIC X.
       01 CHAR-COUNT PIC 9(03) VALUE 0.
       01 L-UPPER-CASE STRING LENGTH 100.

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

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 100 OR L-STRING(I:1) = HIGH-VALUE
               MOVE FUNCTION UPPER-CASE(L-STRING(I:1)) TO TEMP-CHAR

               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 100 OR L-CHAR-SET(J:1) = HIGH-VALUE
                   IF L-CHAR-SET(J:1) = TEMP-CHAR THEN
                       EXIT PERFORM
                   END-IF
               END-PERFORM

               IF L-CHAR-SET(J:1) = HIGH-VALUE THEN
                   MOVE TEMP-CHAR TO L-CHAR-SET(J:1)
                   ADD 1 TO CHAR-COUNT
               END-IF
           END-PERFORM

           MOVE CHAR-COUNT TO RESULT.

           GOBACK.
       END PROGRAM COUNT-DISTINCT-CHARACTERS.
