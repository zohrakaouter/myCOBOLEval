       IDENTIFICATION DIVISION.
       PROGRAM-ID. ISCUBE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-TEMP-VAR                  PIC S9(10).
       01 WS-ABS-A                    PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-A PIC S9(10).
           05 RESULT PIC 9.

      * '''
      * Write a function that takes an integer a and returns True
      * if this ingeger is a cube of some integer number.
      * Note: you may assume the input is always valid.
      * Examples:
      * iscube(1) ==> True
      * iscube(2) ==> False
      * iscube(-1) ==> True
      * iscube(64) ==> True
      * iscube(0) ==> True
      * iscube(180) ==> False
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       BEGIN-PROGRAM.
           MOVE ABS(L-A) TO WS-ABS-A
           COMPUTE WS-TEMP-VAR = FUNCTION INTEGER(SQRT(FUNCTION INTEGER(SQRT(WS-ABS-A))))
           IF WS-TEMP-VAR ** 3 = L-A THEN
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF.

       GOBACK.

       END PROGRAM ISCUBE.
