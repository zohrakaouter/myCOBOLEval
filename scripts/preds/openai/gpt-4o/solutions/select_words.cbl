       IDENTIFICATION DIVISION.
       PROGRAM-ID. SELECT-WORDS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77  VOWELS        PIC X(10) VALUE 'AEIOUaeiou'.
       77  SPACE         PIC X VALUE ' '.
       77  S-INDEX       PIC S9(10) VALUE 1.
       77  WORD-INDEX    PIC S9(10) VALUE 1.
       77  WORD-CHAR     PIC X.
       77  CONSONANT-COUNT PIC S9(10) VALUE 0.
       77  WORD          PIC X(100).
      
       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-S PIC X(100).
           05 L-N PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC X(100).

      * Given a string s and a natural number n, you have been tasked to implement
      * a function that returns a list of all words from string s that contain exactly
      * n consonants, in order these words appear in the string s.
      * If the string s is empty then the function should return an empty list.
      * Note: you may assume the input string contains only letters and spaces.
      * Examples:
      * select_words("Mary had a little lamb", 4) ==> ["little"]
      * select_words("Mary had a little lamb", 3) ==> ["Mary", "lamb"]
      * select_words("simple white space", 2) ==> []
      * select_words("Hello world", 4) ==> ["world"]
      * select_words("Uncle sam", 3) ==> ["Uncle"]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-LOGIC.
           SET NI TO 1
           MOVE SPACE TO WORD

           PERFORM VARYING S-INDEX FROM 1 BY 1
               UNTIL S-INDEX > FUNCTION LENGTH(L-S)

               MOVE FUNCTION STRING(REFERENCE L-S, S-INDEX, 1) TO WORD-CHAR

               IF WORD-CHAR NOT = SPACE
                   THEN
                       STRING WORD DELIMITED BY SIZE
                         WORD-CHAR DELIMITED BY SIZE
                         INTO WORD
                   END-STRING

                   IF WORD-CHAR NOT CONTAINS VOWELS
                       ADD 1 TO CONSONANT-COUNT
                   END-IF
               ELSE
                   IF WORD NOT = SPACE
                       IF CONSONANT-COUNT = L-N
                           MOVE WORD TO RESULT(NI)
                           ADD 1 TO NI
                       END-IF
                   END-IF
                   MOVE SPACE TO WORD
                   MOVE 0 TO CONSONANT-COUNT
               END-IF
           END-PERFORM

           IF WORD NOT = SPACE
               IF CONSONANT-COUNT = L-N
                   MOVE WORD TO RESULT(NI)
                   ADD 1 TO NI
               END-IF
           END-IF

           GOBACK.
       END PROGRAM SELECT-WORDS.
