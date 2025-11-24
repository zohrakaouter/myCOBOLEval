       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOLUTION.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 LST-SIZE        PIC S9(10) COMP-4.
       01 INDEX           PIC S9(10) COMP-4.
       01 ELEMENT         PIC S9(10).
       01 SUM             PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * Given a non-empty list of integers, return the sum of all of the odd elements that are in even positions.
      * 
      * 
      * Examples
      * solution([5, 8, 7, 1]) ==> 12
      * solution([3, 3, 3, 3, 3]) ==> 9
      * solution([30, 13, 24, 321]) ==>0
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           SET LST-SIZE TO FUNCTION LENGTH(L-LST)

           PERFORM VARYING INDEX FROM 1 BY 1 UNTIL INDEX > LST-SIZE
               IF INDEX MOD 2 = 0 THEN
                   MOVE L-LST(INDEX) TO ELEMENT
                   IF ELEMENT MOD 2 NOT = 0 THEN
                       ADD ELEMENT TO SUM
                   END-IF
               END-IF
           END-PERFORM

           MOVE SUM TO RESULT

           GOBACK.
       END PROGRAM SOLUTION.
