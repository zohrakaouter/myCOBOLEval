       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAME-CHARS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 S0-COUNTS.
           05 S0-COUNT OCCURS 26 TIMES.
               10 S0-CHAR PIC 9(5) VALUE ZEROS.

       01 S1-COUNTS.
           05 S1-COUNT OCCURS 26 TIMES.
               10 S1-CHAR PIC 9(5) VALUE ZEROS.

       01 IDX PIC 9(2) VALUE ZEROS.
       01 CHAR-VAR PIC X.
       01 ASCII-VALUE PIC 9(3) VALUE ZEROS.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-S0 PIC X(100).
           05 L-S1 PIC X(100).
           05 RESULT PIC 9.

      * 
      * Check if two words have the same characters.
      * >>> same_chars('eabcdzzzz', 'dddzzzzzzzddeddabc')
      * True
      * >>> same_chars('abcd', 'dddddddabc')
      * True
      * >>> same_chars('dddddddabc', 'abcd')
      * True
      * >>> same_chars('eabcd', 'dddddddabc')
      * False
      * >>> same_chars('abcd', 'dddddddabce')
      * False
      * >>> same_chars('eabcdzzzz', 'dddzzzzzzzddddabc')
      * False
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PROCEDURE.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > FUNCTION LENGTH (L-S0)
               MOVE FUNCTION UPPER-CASE (L-S0(IDX:1)) TO CHAR-VAR
               COMPUTE ASCII-VALUE = FUNCTION ORD (CHAR-VAR) - FUNCTION ORD ('A') + 1
               IF ASCII-VALUE > 0 AND ASCII-VALUE <= 26 THEN
                  ADD 1 TO S0-CHAR (ASCII-VALUE)
               END-IF
           END-PERFORM

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > FUNCTION LENGTH (L-S1)
               MOVE FUNCTION UPPER-CASE (L-S1(IDX:1)) TO CHAR-VAR
               COMPUTE ASCII-VALUE = FUNCTION ORD (CHAR-VAR) - FUNCTION ORD ('A') + 1
               IF ASCII-VALUE > 0 AND ASCII-VALUE <= 26 THEN
                  ADD 1 TO S1-CHAR (ASCII-VALUE)
               END-IF
           END-PERFORM

           MOVE 1 TO RESULT
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 26
               IF S0-CHAR(IDX) > 0 AND S1-CHAR(IDX) = 0 THEN
                  MOVE 0 TO RESULT
                  EXIT PERFORM
               END-IF
               IF S1-CHAR(IDX) > 0 AND S0-CHAR(IDX) = 0 THEN
                  MOVE 0 TO RESULT
                  EXIT PERFORM
               END-IF
           END-PERFORM

       END-MAIN.
       GOBACK.
       END PROGRAM SAME-CHARS.
