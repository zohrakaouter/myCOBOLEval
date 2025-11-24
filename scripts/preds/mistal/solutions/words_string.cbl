       IDENTIFICATION DIVISION.
       PROGRAM-ID. WORDS-STRING.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-INDEX          PIC 9(3) VALUE 1.
           05 WS-WORD-LEN       PIC 9(3) VALUE 0.
           05 WS-RESULT-INDEX   PIC 9(3) VALUE 1.
           05 WS-CHAR           PIC X.
           05 WS-DELIMITER      PIC X VALUE SPACE.
           05 WS-IN-WORD        PIC X VALUE 'N'.
               88 IN-WORD       VALUE 'Y'.
               88 NOT-IN-WORD   VALUE 'N'.

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
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > LENGTH OF L-S
               MOVE L-S(WS-INDEX:1) TO WS-CHAR

               EVALUATE TRUE
                   WHEN WS-CHAR = SPACE OR WS-CHAR = ','
                       IF IN-WORD
                           PERFORM STORE-WORD
                           MOVE 'N' TO WS-IN-WORD
                       END-IF
                   WHEN OTHER
                       IF NOT-IN-WORD
                           MOVE 'Y' TO WS-IN-WORD
                           MOVE 0 TO WS-WORD-LEN
                       END-IF
                       ADD 1 TO WS-WORD-LEN
               END-EVALUATE
           END-PERFORM

           IF IN-WORD
               PERFORM STORE-WORD
           END-IF.

       STORE-WORD.
           MOVE L-S(WS-INDEX - WS-WORD-LEN:WS-WORD-LEN)
             TO RESULT(WS-RESULT-INDEX)
           ADD 1 TO WS-RESULT-INDEX.

       END PROGRAM WORDS-STRING.
       GOBACK.
