       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 TEMP-EVEN-SUM PIC S9(10) VALUE 0.
       01 I PIC S9(10).

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
       BEGIN.
           PERFORM VARYING NI FROM 1 BY 2 UNTIL NI GREATER THAN 3
               IF L-LST(NI) MOD 2 = 0
                   ADD L-LST(NI) TO TEMP-EVEN-SUM
               END-IF
           END-PERFORM
           MOVE TEMP-EVEN-SUM TO RESULT
           GOBACK.

       END PROGRAM ADD.
