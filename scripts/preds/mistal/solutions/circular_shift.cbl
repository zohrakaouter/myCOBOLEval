       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIRCULAR-SHIFT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-X-STR PIC X(20).
           05 WS-X-LEN PIC 9(3).
           05 WS-SHIFT-AMOUNT PIC 9(3).
           05 WS-I PIC 9(3).
           05 WS-J PIC 9(3).
           05 WS-TEMP PIC X(1).
           05 WS-NUM-DIGITS PIC 9(3).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-X PIC S9(10).
           05 L-SHIFT PIC S9(10).
           05 RESULT PIC X(100).

      * Circular shift the digits of the integer x, shift the digits right by shift
      * and return the result as a string.
      * If shift > number of digits, return digits reversed.
      * >>> circular_shift(12, 1)
      * "21"
      * >>> circular_shift(12, 2)
      * "12"
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-X TO WS-X-STR
           INSPECT WS-X-STR TALLYING WS-X-LEN FOR CHARACTERS
               BEFORE INITIAL SPACE

           IF WS-X-LEN = 0
               MOVE "0" TO RESULT
               GOBACK
           END-IF

           COMPUTE WS-NUM-DIGITS = FUNCTION NUMVAL(WS-X-STR)
           COMPUTE WS-SHIFT-AMOUNT = FUNCTION MOD(L-SHIFT, WS-X-LEN)

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-SHIFT-AMOUNT
               MOVE WS-X-STR(WS-X-LEN:1) TO WS-TEMP
               MOVE WS-X-STR(1:WS-X-LEN-1) TO WS-X-STR(2:)
               MOVE WS-TEMP TO WS-X-STR(1:1)
           END-PERFORM

           MOVE WS-X-STR TO RESULT
           GOBACK
           .
       END PROGRAM CIRCULAR-SHIFT.
