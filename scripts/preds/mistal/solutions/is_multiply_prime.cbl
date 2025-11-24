       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-MULTIPLY-PRIME.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-I PIC 9(3) VALUE 2.
           05 WS-J PIC 9(3) VALUE 2.
           05 WS-K PIC 9(3) VALUE 2.
           05 WS-TEMP PIC 9(10).
           05 WS-PRIME1 PIC 9(3) VALUE 0.
           05 WS-PRIME2 PIC 9(3) VALUE 0.
           05 WS-PRIME3 PIC 9(3) VALUE 0.
           05 WS-IS-PRIME PIC 9 VALUE 1.
           05 WS-COUNT PIC 9 VALUE 0.

       01 PRIME-TABLE.
           05 PRIME-NUMBERS PIC 9(3) OCCURS 25 TIMES.

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
           MOVE 0 TO RESULT

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 25
               MOVE FUNCTION ORDINAL(WS-I) TO PRIME-NUMBERS(WS-I)
           END-PERFORM

           MOVE 2 TO PRIME-NUMBERS(1)
           MOVE 3 TO PRIME-NUMBERS(2)
           MOVE 5 TO PRIME-NUMBERS(3)
           MOVE 7 TO PRIME-NUMBERS(4)
           MOVE 11 TO PRIME-NUMBERS(5)
           MOVE 13 TO PRIME-NUMBERS(6)
           MOVE 17 TO PRIME-NUMBERS(7)
           MOVE 19 TO PRIME-NUMBERS(8)
           MOVE 23 TO PRIME-NUMBERS(9)
           MOVE 29 TO PRIME-NUMBERS(10)
           MOVE 31 TO PRIME-NUMBERS(11)
           MOVE 37 TO PRIME-NUMBERS(12)
           MOVE 41 TO PRIME-NUMBERS(13)
           MOVE 43 TO PRIME-NUMBERS(14)
           MOVE 47 TO PRIME-NUMBERS(15)
           MOVE 53 TO PRIME-NUMBERS(16)
           MOVE 59 TO PRIME-NUMBERS(17)
           MOVE 61 TO PRIME-NUMBERS(18)
           MOVE 67 TO PRIME-NUMBERS(19)
           MOVE 71 TO PRIME-NUMBERS(20)
           MOVE 73 TO PRIME-NUMBERS(21)
           MOVE 79 TO PRIME-NUMBERS(22)
           MOVE 83 TO PRIME-NUMBERS(23)
           MOVE 89 TO PRIME-NUMBERS(24)
           MOVE 97 TO PRIME-NUMBERS(25)

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 25
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 25
                   PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 25
                       COMPUTE WS-TEMP = PRIME-NUMBERS(WS-I) *
                                        PRIME-NUMBERS(WS-J) *
                                        PRIME-NUMBERS(WS-K)
                       IF WS-TEMP = L-A
                           MOVE 1 TO RESULT
                           GOBACK
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM

           GOBACK.
       END PROGRAM IS-MULTIPLY-PRIME.
