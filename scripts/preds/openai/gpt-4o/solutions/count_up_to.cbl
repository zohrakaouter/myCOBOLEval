       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNT-UP-TO.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 I PIC S9(10).
       77 J PIC S9(10).
       77 IS-PRIME PIC X(1) VALUE 'Y'.

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

       IF L-N LESS THAN 2
           MOVE LOW-VALUES TO RESULT
           GOBACK.

       SET NI TO 1.

       PERFORM VARYING I FROM 2 BY 1 UNTIL I = L-N
           MOVE 'Y' TO IS-PRIME

           PERFORM VARYING J FROM 2 BY 1 UNTIL J * J > I OR IS-PRIME = 'N'
               IF I REM J = 0
                   MOVE 'N' TO IS-PRIME
               END-IF
           END-PERFORM

           IF IS-PRIME = 'Y'
               MOVE I TO RESULT (NI)
               SET NI UP BY 1
           END-IF
       END-PERFORM

       GOBACK.
