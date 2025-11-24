       IDENTIFICATION DIVISION.
       PROGRAM-ID. WORDS-IN-SENTENCE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-SENTENCE.
           05 WS-WORDS PIC X(100) OCCURS 10 TIMES.

       01 WS-COUNTER          PIC 9(02) VALUE 0.
       01 WS-INDEX            PIC 9(02) VALUE 1.
       01 WS-WORD-INDEX       PIC 9(02) VALUE 1.
       01 WS-CHAR             PIC X.
       01 WS-OUTPUT-INDEX     PIC 9(02) VALUE 1.
       01 WS-TEMP             PIC X(100).
       01 PRIME-STATUS        PIC X VALUE 'N'.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-SENTENCE PIC X(100).
           05 RESULT PIC X(100).

      * 
      * You are given a string representing a sentence,
      * the sentence contains some words separated by a space,
      * and you have to return a string that contains the words from the original sentence,
      * whose lengths are prime numbers,
      * the order of the words in the new string should be the same as the original one.
      * 
      * Example 1:
      * Input: sentence = "This is a test"
      * Output: "is"
      * 
      * Example 2:
      * Input: sentence = "lets go for swimming"
      * Output: "go for"
      * 
      * Constraints:
      * * 1 <= len(sentence) <= 100
      * * sentence contains only letters
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM SPLIT-SENTENCE
           PERFORM GET-PRIME-WORDS
           
           STRING WS-WORDS(1:WS-OUTPUT-INDEX - 1) DELIMITED BY SPACE
              INTO RESULT
           END-STRING
           
           GOBACK.

       SPLIT-SENTENCE.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > LENGTH OF L-SENTENCE
               MOVE L-SENTENCE(WS-INDEX:1) TO WS-CHAR
               IF WS-CHAR NOT EQUAL TO ' '
                   MOVE WS-CHAR TO WS-TEMP(WS-COUNTER + 1:1)
                   ADD 1 TO WS-COUNTER
               ELSE
                   IF WS-COUNTER > 0
                       MOVE WS-TEMP(1:WS-COUNTER) TO WS-WORDS(WS-WORD-INDEX)
                       ADD 1 TO WS-WORD-INDEX
                       MOVE 0 TO WS-COUNTER
                       MOVE ALL ' ' TO WS-TEMP
                   END-IF
               END-IF
           END-PERFORM

           IF WS-COUNTER > 0
               MOVE WS-TEMP(1:WS-COUNTER) TO WS-WORDS(WS-WORD-INDEX)
           END-IF.

       GET-PRIME-WORDS.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > WS-WORD-INDEX
               EVALUATE TRUE
                   WHEN WS-WORDS(WS-INDEX) = SPACES
                       EXIT PERFORM
                   WHEN OTHER
                       COMPUTE WS-COUNTER = FUNCTION LENGTH(WS-WORDS(WS-INDEX))
                       CALL 'IS-PRIME' USING WS-COUNTER GIVING PRIME-STATUS
                       IF PRIME-STATUS = 'Y'
                           MOVE WS-WORDS(WS-INDEX) TO WS-WORDS(WS-OUTPUT-INDEX)
                           ADD 1 TO WS-OUTPUT-INDEX
                       END-IF
               END-EVALUATE
           END-PERFORM.

       IS-PRIME SECTION.
       IS-PRIME.
       PROCEDURE DIVISION USING LINKED-ITEMS.

           IF WS-COUNTER <= 1
               MOVE 'N' TO PRIME-STATUS
               GOBACK
           END-IF

           IF WS-COUNTER = 2
               MOVE 'Y' TO PRIME-STATUS
               GOBACK
           END-IF

           MOVE 2 TO WS-INDEX
           PERFORM VARYING WS-INDEX FROM 2 BY 1 UNTIL WS-INDEX * WS-INDEX > WS-COUNTER
               IF WS-COUNTER REM WS-INDEX = 0
                   MOVE 'N' TO PRIME-STATUS
                   GOBACK
               END-IF
           END-PERFORM

           MOVE 'Y' TO PRIME-STATUS

           GOBACK.

       END PROGRAM WORDS-IN-SENTENCE.
