       IDENTIFICATION DIVISION.
       PROGRAM-ID. WORDS-STRING.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS.
           05 WS-INDEX               PIC 9(03) VALUE 1.
           05 CHAR-INDEX             PIC 9(03) VALUE 1.
           05 CHAR-CURRENT           PIC X VALUE SPACE.
           05 WORD-CURRENT         PIC X(100) VALUE SPACES.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-S PIC X(100).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC X(100).

      * 
      * You will be given a string of words separated by commas or spaces. Your task is
      * to split the string into words and return an array of the words.
      * 
      * For example:
      * words_string("Hi, my name is John") == ["Hi", "my", "name", "is", "John"]
      * words_string("One, two, three, four, five, six") == ["One", "two", "three", "four", "five", "six"]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PROCEDURE.
           PERFORM VARYING CHAR-INDEX FROM 1 BY 1
               UNTIL CHAR-INDEX > LENGTH OF L-S
               MOVE L-S(CHAR-INDEX:1) TO CHAR-CURRENT
               EVALUATE TRUE
                   WHEN CHAR-CURRENT = ',' OR CHAR-CURRENT = SPACE
                       IF WORD-CURRENT NOT = SPACES
                           PERFORM ADD-WORD
                       END-IF
                   WHEN OTHER
                       MOVE CHAR-CURRENT TO WORD-CURRENT(CHAR-INDEX:1)
               END-EVALUATE
           END-PERFORM
           IF WORD-CURRENT NOT = SPACES
               PERFORM ADD-WORD
           END-IF
           GOBACK.

       ADD-WORD.
           MOVE WORD-CURRENT TO RESULT(WS-INDEX)
           ADD 1 TO WS-INDEX
           MOVE SPACES TO WORD-CURRENT.

       END PROGRAM WORDS-STRING.
