       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-BORED.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-SENTENCE-ENDS PIC X VALUE ".?!". 
       01 WS-IN-BOREDOM PIC 9(2) BINARY VALUE 0.
       01 WS-INDEX        PIC 9(3) BINARY VALUE 1.
       01 WS-CURRENT-CHAR PIC X VALUE SPACE.
       01 WS-STATE        PIC 9 VALUE 0.
       01 WS-PREV-CHAR    PIC X VALUE SPACE.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-S PIC X(100).
           05 RESULT PIC S9(10).

      * 
      * You'll be given a string of words, and your task is to count the number
      * of boredoms. A boredom is a sentence that starts with the word "I".
      * Sentences are delimited by '.', '?' or '!'.
      * 
      * For example:
      * >>> is_bored("Hello world")
      * 0
      * >>> is_bored("The sky is blue. The sun is shining. I love this weather")
      * 1
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       BEGIN.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > LENGTH OF L-S

               MOVE L-S(WS-INDEX:1) TO WS-CURRENT-CHAR

               IF (WS-STATE = 0 AND WS-CURRENT-CHAR NOT EQUAL SPACE)
                   IF WS-CURRENT-CHAR = "I" AND
                      (WS-INDEX = 1 OR WS-PREV-CHAR = " " OR WS-PREV-CHAR IN WS-SENTENCE-ENDS)
                       MOVE 1 TO WS-STATE
                   ELSE
                       MOVE 0 TO WS-STATE
                   END-IF
               END-IF

               IF WS-STATE = 1 AND (WS-CURRENT-CHAR IN WS-SENTENCE-ENDS)
                   ADD 1 TO WS-IN-BOREDOM
                   MOVE 0 TO WS-STATE
               END-IF

               IF WS-CURRENT-CHAR NOT = SPACE
                   MOVE WS-CURRENT-CHAR TO WS-PREV-CHAR
               END-IF
               
           END-PERFORM

           MOVE WS-IN-BOREDOM TO RESULT

           GOBACK.
       END PROGRAM IS-BORED.
