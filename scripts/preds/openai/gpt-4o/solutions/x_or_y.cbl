       IDENTIFICATION DIVISION.
       PROGRAM-ID. X-OR-Y.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 I PIC S9(10).
       77 IS-PRIME PIC X VALUE 'Y'.
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
       
       MAIN-PROCEDURE.
           IF L-N < 2 
               MOVE 'N' TO IS-PRIME
           ELSE
               PERFORM CHECK-PRIME
           END-IF
           
           IF IS-PRIME = 'Y'
               MOVE L-X TO RESULT
           ELSE
               MOVE L-Y TO RESULT
           END-IF.
           
       CHECK-PRIME.
           MOVE 2 TO I
           PERFORM UNTIL I * I > L-N
               IF L-N MOD I = 0 THEN
                   MOVE 'N' TO IS-PRIME
                   EXIT PERFORM
               END-IF
               ADD 1 TO I
           END-PERFORM.

       GOBACK.
       END PROGRAM X-OR-Y.
