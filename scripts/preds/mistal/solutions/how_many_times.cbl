       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOW-MANY-TIMES.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-VARIABLES.
           05 WS-LEN-STRING PIC 9(3).
           05 WS-LEN-SUBSTRING PIC 9(3).
           05 WS-INDEX PIC 9(3).
           05 WS-COUNT PIC 9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRING PIC X(100).
           05 L-SUBSTRING PIC X(100).
           05 RESULT PIC S9(10).

      * Find how many times a given substring can be found in the original string. Count overlaping cases.
      * >>> how_many_times('', 'a')
      * 0
      * >>> how_many_times('aaa', 'a')
      * 3
      * >>> how_many_times('aaaa', 'aa')
      * 3
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           MOVE 0 TO RESULT

           IF L-STRING = SPACES OR L-SUBSTRING = SPACES THEN
               GOBACK
           END-IF

           INSPECT FUNCTION REVERSE(L-STRING)
               TALLYING WS-LEN-STRING FOR CHARACTERS
               BEFORE INITIAL SPACE
           END-INSPECT

           INSPECT FUNCTION REVERSE(L-SUBSTRING)
               TALLYING WS-LEN-SUBSTRING FOR CHARACTERS
               BEFORE INITIAL SPACE
           END-INSPECT

           IF WS-LEN-SUBSTRING > WS-LEN-STRING THEN
               GOBACK
           END-IF

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-LEN-STRING
               IF L-STRING(WS-INDEX:WS-LEN-SUBSTRING) =
                  L-SUBSTRING(1:WS-LEN-SUBSTRING)
                   ADD 1 TO WS-COUNT
               END-IF
           END-PERFORM

           MOVE WS-COUNT TO RESULT

           GOBACK
           .
       END PROGRAM HOW-MANY-TIMES.
