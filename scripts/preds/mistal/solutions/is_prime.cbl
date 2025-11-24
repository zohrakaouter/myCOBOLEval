       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-PRIME.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARS.
           05 I PIC S9(10) VALUE 2.
           05 IS-PRIME-FLAG PIC 9 VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC 9.

      * Return true if a given number is prime, and false otherwise.
      * >>> is_prime(6)
      * False
      * >>> is_prime(101)
      * True
      * >>> is_prime(11)
      * True
      * >>> is_prime(13441)
      * True
      * >>> is_prime(61)
      * True
      * >>> is_prime(4)
      * False
      * >>> is_prime(1)
      * False
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           IF L-N <= 1
               MOVE 0 TO RESULT
               GOBACK
           END-IF

           PERFORM VARYING I FROM 2 BY 1 UNTIL I * I > L-N
               IF FUNCTION MOD(L-N, I) = 0
                   MOVE 0 TO RESULT
                   GOBACK
               END-IF
           END-PERFORM

           MOVE 1 TO RESULT
           GOBACK
           .
       END PROGRAM IS-PRIME.
