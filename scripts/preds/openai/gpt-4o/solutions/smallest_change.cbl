       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMALLEST-CHANGE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS.
           05 I PIC S9(10) VALUE 1.
           05 J PIC S9(10).
           05 CHANGES PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 7 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * Given an array arr of integers, find the minimum number of elements that
      * need to be changed to make the array palindromic. A palindromic array is an array that
      * is read the same backwards and forwards. In one change, you can change one element to any other element.
      * 
      * For example:
      * smallest_change([1, 2, 3, 4, 3, 2, 2]) == 1
      * smallest_change([1, 2, 3, 4, 3, 2, 1]) == 0
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE LENGTH OF L-ARR TO J.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > J / 2

               IF L-ARR(I) NOT = L-ARR(J - I + 1)
                   ADD 1 TO CHANGES
               END-IF

           END-PERFORM

           MOVE CHANGES TO RESULT

           GOBACK
           .

       END PROGRAM SMALLEST-CHANGE.
