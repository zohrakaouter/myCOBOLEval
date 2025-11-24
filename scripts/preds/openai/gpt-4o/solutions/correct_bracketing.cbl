       IDENTIFICATION DIVISION.
       PROGRAM-ID. CORRECT-BRACKETING.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 STACK-COUNTER PIC 9(3) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-BRACKETS PIC X(100).
           05 RESULT PIC 9.

      * brackets is a string of "<" and ">".
      * return True if every opening bracket has a corresponding closing bracket.
      * 
      * >>> correct_bracketing("<")
      * False
      * >>> correct_bracketing("<>")
      * True
      * >>> correct_bracketing("<<><>>")
      * True
      * >>> correct_bracketing("><<>")
      * False
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
       PROCESS-BRACKETS.
           PERFORM VARYING INDEX FROM 1 BY 1 UNTIL INDEX > FUNCTION LENGTH(L-BRACKETS)
               EVALUATE L-BRACKETS(INDEX:1)
                   WHEN "<"
                       ADD 1 TO STACK-COUNTER
                   WHEN ">"
                       SUBTRACT 1 FROM STACK-COUNTER
                       IF STACK-COUNTER < 0 THEN
                           MOVE 0 TO RESULT
                           EXIT PERFORM
                       END-IF
               END-EVALUATE
           END-PERFORM
           
           IF STACK-COUNTER = 0
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF.

       END PROGRAM CORRECT-BRACKETING.
       GOBACK.
