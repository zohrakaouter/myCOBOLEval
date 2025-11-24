       IDENTIFICATION DIVISION.
       PROGRAM-ID. FLIP-CASE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-LOOP-COUNTER PIC 9(3) VALUE 0.
       01 WS-CHAR PIC X.
       01 WS-CHAR-VALUE PIC 9(3).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRING PIC X(100).
           05 RESULT PIC X(100).

      * For a given string, flip lowercase characters to uppercase and uppercase to lowercase.
      * >>> flip_case('Hello')
      * 'hELLO'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1
                   UNTIL WS-LOOP-COUNTER > LENGTH OF L-STRING
               MOVE L-STRING(WS-LOOP-COUNTER:1) TO WS-CHAR
               IF WS-CHAR >= 'a' AND WS-CHAR <= 'z'
                   COMPUTE WS-CHAR-VALUE = FUNCTION ORD(WS-CHAR) - 32
                   MOVE FUNCTION CHAR(WS-CHAR-VALUE) TO RESULT(WS-LOOP-COUNTER:1)
               ELSE IF WS-CHAR >= 'A' AND WS-CHAR <= 'Z'
                   COMPUTE WS-CHAR-VALUE = FUNCTION ORD(WS-CHAR) + 32
                   MOVE FUNCTION CHAR(WS-CHAR-VALUE) TO RESULT(WS-LOOP-COUNTER:1)
               ELSE
                   MOVE WS-CHAR TO RESULT(WS-LOOP-COUNTER:1)
               END-IF
           END-PERFORM
           GOBACK.
       END PROGRAM FLIP-CASE.
