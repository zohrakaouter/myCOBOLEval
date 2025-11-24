       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEXT-SMALLEST.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 PREV-SMALLEST PIC S9(10) VALUE 0.
       77 CURRENT-SMALLEST PIC S9(10) VALUE 0.
       77 TEMP-SMALLEST PIC S9(10) VALUE 0.
       77 SMALLEST-FOUND PIC 9 VALUE 0.
       77 NEXT-SMALLEST-FOUND PIC 9 VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * You are given a list of integers.
      * Write a function next_smallest() that returns the 2nd smallest element of the list.
      * Return 0 if there is no such element.
      * 
      * next_smallest([1, 2, 3, 4, 5]) == 2
      * next_smallest([5, 1, 4, 3, 2]) == 2
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
           SET NI TO 1
           MOVE 1 TO SMALLEST-FOUND
           
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 5
               IF SMALLEST-FOUND = 1
                   MOVE L-LST(NI) TO CURRENT-SMALLEST
                   MOVE 0 TO SMALLEST-FOUND
               ELSE
                   IF L-LST(NI) < CURRENT-SMALLEST
                       MOVE CURRENT-SMALLEST TO PREV-SMALLEST
                       MOVE L-LST(NI) TO CURRENT-SMALLEST
                   ELSE
                       IF (L-LST(NI) > CURRENT-SMALLEST AND
                           (NEXT-SMALLEST-FOUND = 1 OR L-LST(NI) < PREV-SMALLEST))
                           MOVE L-LST(NI) TO PREV-SMALLEST
                           MOVE 0 TO NEXT-SMALLEST-FOUND
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           IF PREV-SMALLEST = 0
               MOVE 0 TO RESULT
           ELSE
               MOVE PREV-SMALLEST TO RESULT
           END-IF

           GOBACK.
