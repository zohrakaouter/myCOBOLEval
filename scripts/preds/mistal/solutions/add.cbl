       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 I PIC 9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * Given a non-empty list of integers lst. add the even elements that are at odd indices..
      * 
      * 
      * Examples:
      * add([4, 2, 6, 7]) ==> 2
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE 0 TO RESULT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               IF I IS ODD AND L-LST(I) IS EVEN
                   ADD L-LST(I) TO RESULT
               END-IF
           END-PERFORM
           GOBACK.
       END PROGRAM ADD.
