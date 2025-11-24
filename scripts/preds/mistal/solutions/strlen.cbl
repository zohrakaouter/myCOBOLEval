       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRLEN.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTER PIC 9(3) VALUE 0.
       01 WS-LEN PIC 9(3) VALUE 0.
       01 WS-START-FOUND PIC X VALUE 'N'.
       01 WS-END-FOUND PIC X VALUE 'N'.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRING PIC X(100).
           05 RESULT PIC S9(10).

      * Return length of given string. Ignore leading and trailing spaces.
      * >>> strlen('')
      * 0
      * >>> strlen('abc')
      * 3
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING WS-COUNTER FROM 1 BY 1
                   UNTIL WS-COUNTER > LENGTH OF L-STRING
                   OR WS-START-FOUND = 'Y'
               IF L-STRING(WS-COUNTER:1) NOT = SPACE
                   MOVE 'Y' TO WS-START-FOUND
                   MOVE WS-COUNTER TO WS-LEN
               END-IF
           END-PERFORM

           PERFORM VARYING WS-COUNTER FROM LENGTH OF L-STRING BY -1
                   UNTIL WS-COUNTER < WS-LEN
                   OR WS-END-FOUND = 'Y'
               IF L-STRING(WS-COUNTER:1) NOT = SPACE
                   MOVE 'Y' TO WS-END-FOUND
                   SUBTRACT WS-LEN FROM WS-COUNTER
                   ADD 1 TO WS-COUNTER
                   ADD WS-COUNTER TO WS-LEN
               END-IF
           END-PERFORM

           IF WS-START-FOUND = 'N'
               MOVE 0 TO RESULT
           ELSE
               MOVE WS-LEN TO RESULT
           END-IF

           GOBACK.
       END PROGRAM STRLEN.
