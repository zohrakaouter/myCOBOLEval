       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAIRS-SUM-TO-ZERO.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 I PIC 9(2) VALUE 0.
       01 J PIC 9(2) VALUE 0.
       01 FOUND-FLAG PIC 9 VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC 9.

      * 
      * pairs_sum_to_zero takes a list of integers as an input.
      * it returns True if there are two distinct elements in the list that
      * sum to zero, and False otherwise.
      * >>> pairs_sum_to_zero([1, 3, 5, 0])
      * False
      * >>> pairs_sum_to_zero([1, 3, -2, 1])
      * False
      * >>> pairs_sum_to_zero([1, 2, 3, 7])
      * False
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > 4
                   IF L-L(I) + L-L(J) = 0
                       MOVE 1 TO FOUND-FLAG
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               IF FOUND-FLAG = 1
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF FOUND-FLAG = 1
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF

           GOBACK.
       END PROGRAM PAIRS-SUM-TO-ZERO.
