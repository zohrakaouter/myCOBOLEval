       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-SEQUENCE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01  WS-COUNTER          PIC S9(10) VALUE 0.
       01  WS-TEMP-STRING      PIC X(10) VALUE SPACES.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC X(100).

      * Return a string containing space-delimited numbers starting from 0 upto n inclusive.
      * >>> string_sequence(0)
      * '0'
      * >>> string_sequence(5)
      * '0 1 2 3 4 5'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE SPACES TO RESULT
           PERFORM VARYING WS-COUNTER FROM 0 BY 1 UNTIL WS-COUNTER > L-N
               MOVE WS-COUNTER TO WS-TEMP-STRING
               IF WS-COUNTER = 0
                   MOVE WS-TEMP-STRING TO RESULT
               ELSE
                   STRING RESULT DELIMITED BY SIZE
                          " " DELIMITED BY SIZE
                          WS-TEMP-STRING DELIMITED BY SPACE
                   INTO RESULT
               END-IF
           END-PERFORM
           GOBACK.
       END PROGRAM STRING-SEQUENCE.
