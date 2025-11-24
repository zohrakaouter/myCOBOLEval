       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRIME-FIB.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-F1 PIC S9(10) VALUE 1.
       01 WS-F2 PIC S9(10) VALUE 1.
       01 WS-TEMP PIC S9(10).
       01 WS-INDEX PIC S9(10) VALUE 0.
       01 WS-PRIME-FIB-INDEX PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * prime_fib returns n-th number that is a Fibonacci number and it's also prime.
      * >>> prime_fib(1)
      * 2
      * >>> prime_fib(2)
      * 3
      * >>> prime_fib(3)
      * 5
      * >>> prime_fib(4)
      * 13
      * >>> prime_fib(5)
      * 89
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PROCESS.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-PRIME-FIB-INDEX = L-N
               
               * Calculate next Fibonacci number
               MOVE WS-F2 TO WS-TEMP
               ADD WS-F1 TO WS-F2
               MOVE WS-TEMP TO WS-F1
               
               * Check if WS-F2 is prime
               IF WS-F2 > 1 AND FUNCTION MOD(WS-F2 2) NOT = 0 THEN
                   IF IS-PRIME(WS-F2)
                       ADD 1 TO WS-PRIME-FIB-INDEX
                       IF WS-PRIME-FIB-INDEX = L-N DISPLAY WS-F2
                   END-IF
               END-IF
           END-PERFORM

           MOVE WS-F2 TO RESULT

           GOBACK.

       IS-PRIME SECTION.
           DECLARE A PIC S9(10).
           DECLARE I PIC S9(10).
           IF WS-F2 = 2 OR WS-F2 = 3 THEN
               EXIT SECTION
           ELSE
               MOVE 2 TO I
               ADD 1 TO WS-F2 GIVING A
               DIVIDE WS-F2 BY I GIVING A REMAINING R
               PERFORM UNTIL I * I > WS-F2 OR R = 0
                   IF R = 0 THEN
                       MOVE 0 TO WS-F2
                       EXIT SECTION
                   END-IF
                   ADD 1 TO I
                   DIVIDE WS-F2 BY I GIVING A REMAINING R
               END-PERFORM
           END-IF
       END-IS-PRIME.

       END PROGRAM PRIME-FIB.
