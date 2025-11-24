       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIX-SPACES.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 SPACE-COUNTER   PIC 9 VALUE 0.
       77 I               PIC 9(3) VALUE 1.
       77 J               PIC 9(3) VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-TEXT PIC X(100).
           05 RESULT PIC X(100).

      * 
      * Given a string text, replace all spaces in it with underscores,
      * and if a string has more than 2 consecutive spaces,
      * then replace all consecutive spaces with -
      * 
      * fix_spaces("Example") == "Example"
      * fix_spaces("Example 1") == "Example_1"
      * fix_spaces(" Example 2") == "_Example_2"
      * fix_spaces(" Example   3") == "_Example-3"
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF L-TEXT
               EVALUATE TRUE
                   WHEN L-TEXT(I:1) = " "
                       ADD 1 TO SPACE-COUNTER
                       
                       IF SPACE-COUNTER > 2
                           MOVE "-" TO RESULT(J:1)
                           SUBTRACT 1 FROM J
                       ELSE
                           MOVE "_" TO RESULT(J:1)
                       END-IF
                       
                   WHEN OTHER
                       IF SPACE-COUNTER > 2
                           SUBTRACT 1 FROM J
                       END-IF
                       
                       MOVE L-TEXT(I:1) TO RESULT(J:1)
                       SET SPACE-COUNTER TO 0
               END-EVALUATE
               ADD 1 TO J
           END-PERFORM.

           DISPLAY "RESULT: " RESULT.

           GOBACK.
       END PROGRAM FIX-SPACES.
