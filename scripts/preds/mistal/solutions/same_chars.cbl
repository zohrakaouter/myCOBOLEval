       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAME-CHARS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTERS.
           05 WS-I PIC 9(3).
           05 WS-J PIC 9(3).
           05 WS-K PIC 9(3).
       01 WS-LENGTHS.
           05 WS-LEN1 PIC 9(3).
           05 WS-LEN2 PIC 9(3).
       01 WS-CHARACTERS.
           05 WS-CHAR1 PIC X.
           05 WS-CHAR2 PIC X.
       01 WS-CHAR-COUNT1.
           05 WS-COUNT1 OCCURS 256 TIMES PIC 9(5) VALUE 0.
       01 WS-CHAR-COUNT2.
           05 WS-COUNT2 OCCURS 256 TIMES PIC 9(5) VALUE 0.
       01 WS-MATCH PIC 9 VALUE 1.

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
           MOVE 0 TO WS-I WS-J WS-K
           MOVE 1 TO WS-MATCH

           COMPUTE WS-LEN1 = FUNCTION LENGTH(FUNCTION TRIM(L-S0))
           COMPUTE WS-LEN2 = FUNCTION LENGTH(FUNCTION TRIM(L-S1))

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN1
               MOVE L-S0(WS-I:1) TO WS-CHAR1
               COMPUTE WS-K = FUNCTION ORD(WS-CHAR1) + 1
               ADD 1 TO WS-COUNT1(WS-K)
           END-PERFORM

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-LEN2
               MOVE L-S1(WS-J:1) TO WS-CHAR2
               COMPUTE WS-K = FUNCTION ORD(WS-CHAR2) + 1
               ADD 1 TO WS-COUNT2(WS-K)
           END-PERFORM

           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 256
               IF WS-COUNT1(WS-K) NOT EQUAL TO WS-COUNT2(WS-K)
                   MOVE 0 TO WS-MATCH
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF WS-MATCH = 1
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF

           GOBACK.
       END PROGRAM SAME-CHARS.
