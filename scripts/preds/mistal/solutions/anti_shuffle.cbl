       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANTI-SHUFFLE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-WORD-COUNT          PIC 9(3) VALUE 0.
           05 WS-WORD-LEN            PIC 9(3) VALUE 0.
           05 WS-I                  PIC 9(3) VALUE 0.
           05 WS-J                  PIC 9(3) VALUE 0.
           05 WS-TEMP-CHAR          PIC X.
           05 WS-WORD               PIC X(100).
           05 WS-SORTED-WORD        PIC X(100).
           05 WS-SPACE-FLAG         PIC X VALUE 'N'.
               88 IS-SPACE          VALUE 'Y'.
               88 NOT-SPACE         VALUE 'N'.

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
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > LENGTH OF L-S
               IF L-S(WS-I:1) = SPACE
                   MOVE 'Y' TO WS-SPACE-FLAG
                   IF WS-WORD-LEN > 0
                       PERFORM SORT-WORD
                       STRING WS-SORTED-WORD(1:WS-WORD-LEN) DELIMITED BY SIZE
                           INTO RESULT WITH POINTER WS-J
                       MOVE 0 TO WS-WORD-LEN
                       MOVE SPACES TO WS-WORD
                   END-IF
                   STRING ' ' DELIMITED BY SIZE
                       INTO RESULT WITH POINTER WS-J
                   MOVE 'N' TO WS-SPACE-FLAG
               ELSE
                   IF NOT-SPACE
                       ADD 1 TO WS-WORD-LEN
                       MOVE L-S(WS-I:1) TO WS-WORD(WS-WORD-LEN:1)
                   END-IF
               END-IF
           END-PERFORM

           IF WS-WORD-LEN > 0
               PERFORM SORT-WORD
               STRING WS-SORTED-WORD(1:WS-WORD-LEN) DELIMITED BY SIZE
                   INTO RESULT WITH POINTER WS-J
           END-IF

           GOBACK.

       SORT-WORD.
           MOVE WS-WORD(1:WS-WORD-LEN) TO WS-SORTED-WORD(1:WS-WORD-LEN)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-WORD-LEN
               PERFORM VARYING WS-J FROM WS-I + 1 BY 1 UNTIL WS-J > WS-WORD-LEN
                   IF WS-SORTED-WORD(WS-I:1) > WS-SORTED-WORD(WS-J:1)
                       MOVE WS-SORTED-WORD(WS-I:1) TO WS-TEMP-CHAR
                       MOVE WS-SORTED-WORD(WS-J:1) TO WS-SORTED-WORD(WS-I:1)
                       MOVE WS-TEMP-CHAR TO WS-SORTED-WORD(WS-J:1)
                   END-IF
               END-PERFORM
           END-PERFORM.
       END PROGRAM ANTI-SHUFFLE.
