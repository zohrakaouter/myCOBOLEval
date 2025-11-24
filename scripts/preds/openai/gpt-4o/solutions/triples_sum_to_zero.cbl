       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIPLES-SUM-TO-ZERO.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 N              PIC S9(4) COMP-5 VALUE 4.
       01 I              PIC S9(4) COMP-5.
       01 J              PIC S9(4) COMP-5.
       01 K              PIC S9(4) COMP-5.
       01 FOUND          PIC X VALUE 'N'.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC 9.

      * 
      * triples_sum_to_zero takes a list of integers as an input.
      * it returns True if there are three distinct elements in the list that
      * sum to zero, and False otherwise.
      * 
      * >>> triples_sum_to_zero([1, 3, 5, 0])
      * False
      * >>> triples_sum_to_zero([1, 3, -2, 1])
      * True
      * >>> triples_sum_to_zero([1, 2, 3, 7])
      * False
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           SET RESULT TO ZERO
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > N
                   PERFORM VARYING K FROM J + 1 BY 1 UNTIL K > N
                       IF L-L (I) + L-L (J) + L-L (K) = 0 THEN
                           MOVE 'Y' TO FOUND
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM

           IF FOUND = 'Y' THEN
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF
           GOBACK.
       END PROGRAM TRIPLES-SUM-TO-ZERO.
