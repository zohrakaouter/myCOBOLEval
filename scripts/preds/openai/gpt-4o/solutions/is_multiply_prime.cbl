       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-MULTIPLY-PRIME.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 PRIMES-TABLE.
           05 PRIMES OCCURS 25 TIMES PIC 9 VALUE ZEROS.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-A PIC S9(10).
           05 RESULT PIC 9.

      * Write a function that returns true if the given number is the multiplication of 3 prime numbers
      * and false otherwise.
      * Knowing that (a) is less then 100.
      * Example:
      * is_multiply_prime(30) == True
      * 30 = 2 * 3 * 5
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-LOGIC SECTION.
           PERFORM INITIALIZE-PRIMES
           PERFORM CHECK-MULTIPLY-PRIME
           GOBACK.

       INITIALIZE-PRIMES.
           MOVE 2  TO PRIMES(1)
           MOVE 3  TO PRIMES(2)
           MOVE 5  TO PRIMES(3)
           MOVE 7  TO PRIMES(4)
           MOVE 11 TO PRIMES(5)
           MOVE 13 TO PRIMES(6)
           MOVE 17 TO PRIMES(7)
           MOVE 19 TO PRIMES(8)
           MOVE 23 TO PRIMES(9)
           MOVE 29 TO PRIMES(10)
           MOVE 31 TO PRIMES(11)
           MOVE 37 TO PRIMES(12)
           MOVE 41 TO PRIMES(13)
           MOVE 43 TO PRIMES(14)
           MOVE 47 TO PRIMES(15)
           MOVE 53 TO PRIMES(16)
           MOVE 59 TO PRIMES(17)
           MOVE 61 TO PRIMES(18)
           MOVE 67 TO PRIMES(19)
           MOVE 71 TO PRIMES(20)
           MOVE 73 TO PRIMES(21)
           MOVE 79 TO PRIMES(22)
           MOVE 83 TO PRIMES(23)
           MOVE 89 TO PRIMES(24)
           MOVE 97 TO PRIMES(25).

       CHECK-MULTIPLY-PRIME.
           MOVE 0 TO RESULT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 23
               PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > 24
                   PERFORM VARYING K FROM J + 1 BY 1 UNTIL K > 25
                       COMPUTE TEMP-RESULT = PRIMES(I) * PRIMES(J) * PRIMES(K)
                       IF TEMP-RESULT = L-A
                           MOVE 1 TO RESULT
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM.

       END PROGRAM IS-MULTIPLY-PRIME.
