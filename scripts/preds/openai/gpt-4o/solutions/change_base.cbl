       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHANGE-BASE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 BASE-TABLE.
           05 DIGIT REDEFINES BASE-TABLE PIC X OCCURS 10 TIMES.
               88 VALID-DIGIT VALUE '0' THROUGH '9'.

       01 TEMP-RESULT PIC X(100).
       01 INDEX PIC 9(2) VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-X PIC S9(10).
           05 L-BASE PIC S9(10).
           05 RESULT PIC X(100).

      * Change numerical base of input number x to base.
      * return string representation after the conversion.
      * base numbers are less than 10.
      * >>> change_base(8, 3)
      * '22'
      * >>> change_base(8, 2)
      * '1000'
      * >>> change_base(7, 2)
      * '111'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       CHANGE-BASE-ROUTINE.
           MOVE SPACES TO RESULT
           PERFORM UNTIL L-X = 0
               DIVIDE L-X BY L-BASE GIVING L-X REMAINDER REM
               MOVE DIGIT (REM + 1) TO TEMP-RESULT (INDEX:1)
               ADD 1 TO INDEX
           END-PERFORM
           SUBTRACT 1 FROM INDEX
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > INDEX
               MOVE TEMP-RESULT (INDEX - I + 1:1) TO RESULT (I:1)
           END-PERFORM.

       GOBACK.
       END PROGRAM CHANGE-BASE.
