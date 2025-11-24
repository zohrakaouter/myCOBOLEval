       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNT-UP-TO.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-I PIC S9(10) VALUE 0.
           05 WS-J PIC S9(10) VALUE 0.
           05 WS-PRIME-COUNT PIC S9(10) VALUE 0.
           05 WS-IS-PRIME PIC 9 VALUE 1.
           05 WS-TEMP PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * Implement a function that takes an non-negative integer and returns an array of the first n
      * integers that are prime numbers and less than n.
      * for example:
      * count_up_to(5) => [2,3]
      * count_up_to(11) => [2,3,5,7]
      * count_up_to(0) => []
      * count_up_to(20) => [2,3,5,7,11,13,17,19]
      * count_up_to(1) => []
      * count_up_to(18) => [2,3,5,7,11,13,17]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           IF L-N <= 1
               GOBACK
           END-IF

           PERFORM VARYING WS-I FROM 2 BY 1 UNTIL WS-I >= L-N
               MOVE 1 TO WS-IS-PRIME
               PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J * WS-J > WS-I
                   IF FUNCTION MOD(WS-I, WS-J) = 0
                       MOVE 0 TO WS-IS-PRIME
                       EXIT PERFORM
                   END-IF
               END-PERFORM

               IF WS-IS-PRIME = 1
                   ADD 1 TO WS-PRIME-COUNT
                   MOVE WS-I TO RESULT(WS-PRIME-COUNT)
               END-IF
           END-PERFORM

           GOBACK.
       END PROGRAM COUNT-UP-TO.
