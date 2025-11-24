       IDENTIFICATION DIVISION.
       PROGRAM-ID. CORRECT-BRACKETING.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTERS.
           05 WS-LENGTH PIC 999 VALUE 0.
           05 WS-I PIC 999 VALUE 0.
           05 WS-BALANCE PIC 99 VALUE 0.

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
           MOVE 0 TO RESULT
           INSPECT L-BRACKETS TALLYING WS-LENGTH FOR CHARACTERS
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LENGTH
               EVALUATE L-BRACKETS(WS-I:1)
                   WHEN "<"
                       ADD 1 TO WS-BALANCE
                   WHEN ">"
                       SUBTRACT 1 FROM WS-BALANCE
                       IF WS-BALANCE < 0
                           MOVE 0 TO RESULT
                           GOBACK
                       END-IF
               END-EVALUATE
           END-PERFORM

           IF WS-BALANCE = 0
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF

           GOBACK.
