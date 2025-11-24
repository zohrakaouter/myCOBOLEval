       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRIME-FIB.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-COUNT          PIC S9(10) VALUE 0.
           05 WS-FIB-PREV       PIC S9(10) VALUE 0.
           05 WS-FIB-CURRENT    PIC S9(10) VALUE 1.
           05 WS-FIB-NEXT       PIC S9(10) VALUE 0.
           05 WS-I             PIC S9(10).
           05 WS-PRIME-FLAG     PIC X VALUE 'Y'.
               88 IS-PRIME      VALUE 'Y'.
               88 NOT-PRIME     VALUE 'N'.

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
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-COUNT = L-N
               COMPUTE WS-FIB-NEXT = WS-FIB-PREV + WS-FIB-CURRENT
               MOVE WS-FIB-CURRENT TO WS-FIB-PREV
               MOVE WS-FIB-NEXT TO WS-FIB-CURRENT

               IF WS-FIB-NEXT > 1
                   PERFORM CHECK-PRIME
                   IF IS-PRIME
                       ADD 1 TO WS-COUNT
                       MOVE WS-FIB-NEXT TO RESULT
                   END-IF
               END-IF
           END-PERFORM.

           GOBACK.

       CHECK-PRIME.
           SET IS-PRIME TO TRUE
           PERFORM VARYING WS-I FROM 2 BY 1
                   UNTIL WS-I * WS-I > WS-FIB-NEXT
                   OR NOT-PRIME
               DIVIDE WS-FIB-NEXT BY WS-I GIVING WS-I REMAINDER WS-I
               IF REMAINDER = 0
                   SET NOT-PRIME TO TRUE
               END-IF
           END-PERFORM.
