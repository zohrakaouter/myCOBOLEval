       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEXT-SMALLEST.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARS.
           05 WS-SMALLEST PIC S9(10) VALUE 0.
           05 WS-NEXT-SMALLEST PIC S9(10) VALUE 0.
           05 WS-FIRST-ITEM PIC S9(10) VALUE 0.
           05 WS-I PIC 9(2) VALUE 0.

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
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               IF WS-I = 1
                   MOVE L-LST(WS-I) TO WS-SMALLEST
                   MOVE L-LST(WS-I) TO WS-NEXT-SMALLEST
               ELSE
                   IF L-LST(WS-I) < WS-SMALLEST
                       MOVE WS-SMALLEST TO WS-NEXT-SMALLEST
                       MOVE L-LST(WS-I) TO WS-SMALLEST
                   ELSE
                       IF L-LST(WS-I) < WS-NEXT-SMALLEST
                           AND L-LST(WS-I) > WS-SMALLEST
                           MOVE L-LST(WS-I) TO WS-NEXT-SMALLEST
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           IF WS-NEXT-SMALLEST = WS-SMALLEST
               MOVE 0 TO RESULT
           ELSE
               MOVE WS-NEXT-SMALLEST TO RESULT
           END-IF

           GOBACK.
       END PROGRAM NEXT-SMALLEST.
