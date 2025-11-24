       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-PRIME.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 TEMP-VAR PIC S9(10).
       01 I PIC S9(10).
       01 FLAG PIC 9 VALUE 0.

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
          ELSE IF L-N = 2
             MOVE 1 TO RESULT
          ELSE
             MOVE 1 TO FLAG
             PERFORM VARYING I FROM 2 BY 1 
                      UNTIL I * I > L-N OR FLAG = 0
                DIVIDE L-N BY I GIVING TEMP-VAR REMAINDER RESULT
                IF RESULT = 0
                   MOVE 0 TO FLAG
                END-IF
             END-PERFORM
             IF FLAG = 0
                MOVE 0 TO RESULT
             ELSE
                MOVE 1 TO RESULT
             END-IF
          END-IF

          GOBACK.
       END PROGRAM IS-PRIME.
