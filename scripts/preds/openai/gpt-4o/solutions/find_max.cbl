       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-MAX.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 INPUT-STRINGS.
           05 INPUT-WORD PIC X(30) OCCURS 3 TIMES.

       01 MAX-CHAR-COUNT PIC S9(3) VALUE -1.
       01 TEMP-CHAR-COUNT PIC S9(3).
       01 TEMP-WORD PIC X(30).
       01 BEST-WORD PIC X(30) VALUE SPACES.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-WORDS OCCURS 3 TIMES INDEXED BY NI PIC X(3).
           05 RESULT PIC X(100).

      * Write a function that accepts a list of strings.
      * The list contains different words. Return the word with maximum number
      * of unique characters. If multiple strings have maximum number of unique
      * characters, return the one which comes first in lexicographical order.
      * 
      * find_max(["name", "of", "string"]) == "string"
      * find_max(["name", "enam", "game"]) == "enam"
      * find_max(["aaaaaaa", "bb" ,"cc"]) == ""aaaaaaa"
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 3
               MOVE INPUT-STRINGS(NI) TO TEMP-WORD
               PERFORM CALCULATE-UNIQUE-CHARS
               IF TEMP-CHAR-COUNT > MAX-CHAR-COUNT
                   MOVE TEMP-CHAR-COUNT TO MAX-CHAR-COUNT
                   MOVE TEMP-WORD TO BEST-WORD
               ELSE IF TEMP-CHAR-COUNT = MAX-CHAR-COUNT
                   IF TEMP-WORD < BEST-WORD 
                       MOVE TEMP-WORD TO BEST-WORD
                   END-IF
               END-IF
           END-PERFORM
           MOVE BEST-WORD TO RESULT
           GOBACK.

       CALCULATE-UNIQUE-CHARS.
           DECLARE LET-CHARS TABLE SIZE 256 => VARYING OCCURS 256 TIMES DATA PIC X VALUE 'F'
           MOVE ZEROS TO TEMP-CHAR-COUNT
           PERFORM VARYING NI FROM 1 BY 1 UNTIL INPUT-STRINGS(NI:1) = SPACE
               MOVE INPUT-STRINGS(NI:1) TO TEMP-WORD
               IF LET-CHARS(ORD-FUNCTION(TEMP-WORD):1) = 'F'
                   ADD 1 TO TEMP-CHAR-COUNT
                   MOVE 'T' TO LET-CHARS(ORD-FUNCTION(TEMP-WORD):1)
               END-IF
           END-PERFORM.
