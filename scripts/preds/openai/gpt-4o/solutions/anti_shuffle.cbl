       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANTI-SHUFFLE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 WS-SPACE-COUNTER            PIC 9(3) VALUE 0.
       77 WS-CHAR-INDEX               PIC 9(3) VALUE ZERO.
       77 WS-ASCII-SWAP               PIC X VALUE SPACE.
       77 WS-TEMP-CHAR                PIC X VALUE SPACE.
       77 WS-WORD-END                 PIC 9(3).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-S PIC X(100).
           05 RESULT PIC X(100).

      * 
      * Write a function that takes a string and returns an ordered version of it.
      * Ordered version of string, is a string where all words (separated by space)
      * are replaced by a new word where all the characters arranged in
      * ascending order based on ascii value.
      * Note: You should keep the order of words and blank spaces in the sentence.
      * 
      * For example:
      * anti_shuffle('Hi') returns 'Hi'
      * anti_shuffle('hello') returns 'ehllo'
      * anti_shuffle('Hello World!!!') returns 'Hello !!!Wdlor'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           MOVE SPACES TO RESULT
           MOVE 1 TO WS-CHAR-INDEX

           PERFORM VARYING WS-CHAR-INDEX FROM 1 BY 1
                   UNTIL WS-CHAR-INDEX GREATER THAN LENGTH OF L-S

               IF L-S(WS-CHAR-INDEX:1) EQUAL " "
               
                   ADD 1 TO WS-SPACE-COUNTER

               ELSE
               
                   ADD 1 TO WS-CHAR-INDEX
                   MOVE WS-CHAR-INDEX TO WS-WORD-END

                   PERFORM VARYING WS-CHAR-INDEX FROM WS-CHAR-INDEX BY 1
                           UNTIL L-S(WS-CHAR-INDEX:1) EQUAL " "
                                 OR WS-CHAR-INDEX GREATER THAN LENGTH OF L-S
                       ADD 1 TO WS-WORD-END
                   END-PERFORM

                   PERFORM VARYING WS-CHAR-INDEX FROM WS-SPACE-COUNTER + 1 BY 1
                           UNTIL WS-CHAR-INDEX EQUAL WS-WORD-END - 1
                       PERFORM VARYING WS-ASCI-SWAP FROM WS-CHAR-INDEX + 1 BY 1
                               UNTIL WS-ASCI-SWAP EQUAL WS-WORD-END
                           IF L-S(WS-CHAR-INDEX:1) > L-S(WS-ASCI-SWAP:1)
                               MOVE L-S(WS-CHAR-INDEX:1) TO WS-TEMP-CHAR
                               MOVE L-S(WS-ASCI-SWAP:1) TO L-S(WS-CHAR-INDEX:1)
                               MOVE WS-TEMP-CHAR TO L-S(WS-ASCI-SWAP:1)
                           END-IF
                       END-PERFORM
                   END-PERFORM

                   MOVE L-S(WS-SPACE-COUNTER + 1: WS-WORD-END - WS-SPACE-COUNTER - 1) TO RESULT(WS-SPACE-COUNTER + 1: WS-WORD-END - WS-SPACE-COUNTER - 1)
                   
                   MOVE WS-WORD-END TO WS-SPACE-COUNTER 
               END-IF
           END-PERFORM.
   
           DISPLAY "RESULT: " RESULT
           GOBACK.
