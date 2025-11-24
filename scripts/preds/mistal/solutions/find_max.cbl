       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-MAX.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-WORK-AREAS.
           05 WS-I PIC 9(2) VALUE 0.
           05 WS-J PIC 9(2) VALUE 0.
           05 WS-K PIC 9(2) VALUE 0.
           05 WS-MAX-UNIQUE PIC 9(2) VALUE 0.
           05 WS-CURRENT-UNIQUE PIC 9(2) VALUE 0.
           05 WS-TEMP-WORD PIC X(3).
           05 WS-CHAR PIC X.
           05 WS-UNIQUE-CHARS PIC X(3) VALUE SPACES.
           05 WS-FOUND-FLAG PIC X VALUE 'N'.
               88 FOUND VALUE 'Y'.
               88 NOT-FOUND VALUE 'N'.
           05 WS-RESULT-TEMP PIC X(3) VALUE SPACES.

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
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE L-WORDS(WS-I) TO WS-TEMP-WORD
               MOVE 0 TO WS-CURRENT-UNIQUE
               MOVE SPACES TO WS-UNIQUE-CHARS

               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > LENGTH OF L-WORDS(WS-I)
                   MOVE WS-TEMP-WORD(WS-J:1) TO WS-CHAR
                   SET NOT-FOUND TO TRUE
                   PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > WS-CURRENT-UNIQUE
                       IF WS-CHAR = WS-UNIQUE-CHARS(WS-K:1)
                           SET FOUND TO TRUE
                       END-IF
                   END-PERFORM
                   IF NOT-FOUND
                       ADD 1 TO WS-CURRENT-UNIQUE
                       MOVE WS-CHAR TO WS-UNIQUE-CHARS(WS-CURRENT-UNIQUE:1)
                   END-IF
               END-PERFORM

               IF WS-I = 1
                   MOVE WS-CURRENT-UNIQUE TO WS-MAX-UNIQUE
                   MOVE WS-TEMP-WORD TO WS-RESULT-TEMP
               ELSE
                   IF WS-CURRENT-UNIQUE > WS-MAX-UNIQUE
                       MOVE WS-CURRENT-UNIQUE TO WS-MAX-UNIQUE
                       MOVE WS-TEMP-WORD TO WS-RESULT-TEMP
                   ELSE
                       IF WS-CURRENT-UNIQUE = WS-MAX-UNIQUE
                           IF WS-TEMP-WORD < WS-RESULT-TEMP
                               MOVE WS-TEMP-WORD TO WS-RESULT-TEMP
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           MOVE WS-RESULT-TEMP TO RESULT
           GOBACK.
       END PROGRAM FIND-MAX.
