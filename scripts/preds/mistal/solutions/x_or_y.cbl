       IDENTIFICATION DIVISION.
       PROGRAM-ID. X-OR-Y.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-I PIC S9(10) VALUE 2.
           05 WS-PRIME-FLAG PIC 9 VALUE 1.
               88 IS-PRIME VALUE 1.
               88 NOT-PRIME VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 L-X PIC S9(10).
           05 L-Y PIC S9(10).
           05 RESULT PIC S9(10).

      * A simple program which should return the value of x if n is
      * a prime number and should return the value of y otherwise.
      * 
      * Examples:
      * for x_or_y(7, 34, 12) == 34
      * for x_or_y(15, 8, 5) == 5
      * 
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           IF L-N <= 1
               SET NOT-PRIME TO TRUE
           ELSE
               PERFORM VARYING WS-I FROM 2 BY 1
                   UNTIL WS-I * WS-I > L-N OR NOT IS-PRIME
                   IF FUNCTION MOD(L-N, WS-I) = 0
                       SET NOT-PRIME TO TRUE
                   END-IF
               END-PERFORM
           END-IF

           IF IS-PRIME
               MOVE L-X TO RESULT
           ELSE
               MOVE L-Y TO RESULT
           END-IF

           GOBACK.
       END PROGRAM X-OR-Y.
