       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODP.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-I PIC S9(10) VALUE 0.
           05 WS-TEMP-RESULT PIC S9(10) VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 L-P PIC S9(10).
           05 RESULT PIC S9(10).

      * Return 2^n modulo p (be aware of numerics).
      * >>> modp(3, 5)
      * 3
      * >>> modp(1101, 101)
      * 2
      * >>> modp(0, 101)
      * 1
      * >>> modp(3, 11)
      * 8
      * >>> modp(100, 101)
      * 1
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           IF L-N = 0 THEN
               MOVE 1 TO RESULT
               GOBACK
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > L-N
               COMPUTE WS-TEMP-RESULT = FUNCTION MOD(
                   WS-TEMP-RESULT * 2, L-P)
           END-PERFORM

           MOVE WS-TEMP-RESULT TO RESULT
           GOBACK
           .

       END PROGRAM MODP.
