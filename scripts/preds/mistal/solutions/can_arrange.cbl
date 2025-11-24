       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAN-ARRANGE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 I PIC S9(10) VALUE 1.
       01 FOUND-IND PIC S9(10) VALUE -1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * Create a function which returns the largest index of an element which
      * is not greater than or equal to the element immediately preceding it. If
      * no such element exists then return -1. The given array will not contain
      * duplicate values.
      * 
      * Examples:
      * can_arrange([1,2,4,3,5]) = 3
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 5
               IF L-ARR(I) < L-ARR(I - 1)
                   MOVE I TO FOUND-IND
                   EXIT PERFORM
               END-IF
           END-PERFORM

           MOVE FOUND-IND TO RESULT
           GOBACK.

       END PROGRAM CAN-ARRANGE.
