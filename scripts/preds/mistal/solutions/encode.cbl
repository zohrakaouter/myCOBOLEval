       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENCODE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-INDEXES.
           05 WS-I PIC 9(3) VALUE 0.
           05 WS-LEN PIC 9(3) VALUE 0.
       01 WS-CHAR PIC X.
       01 WS-VOWELS.
           05 FILLER PIC X(5) VALUE 'AEIOU'.
           05 FILLER PIC X(5) VALUE 'aeiou'.
       01 WS-VOWEL-FLAG PIC 9 VALUE 0.
       01 WS-NEW-CHAR PIC X.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-MESSAGE PIC X(100).
           05 RESULT PIC X(100).

      * 
      * Write a function that takes a message, and encodes in such a
      * way that it swaps case of all letters, replaces all vowels in
      * the message with the letter that appears 2 places ahead of that
      * vowel in the english alphabet.
      * Assume only letters.
      * 
      * Examples:
      * >>> encode('test')
      * 'TGST'
      * >>> encode('This is a message')
      * 'tHKS KS C MGSSCGG'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
               OR L-MESSAGE(WS-I:1) = SPACE
               MOVE 0 TO WS-VOWEL-FLAG
               MOVE L-MESSAGE(WS-I:1) TO WS-CHAR

               IF WS-CHAR = SPACE THEN
                   MOVE WS-CHAR TO RESULT(WS-I:1)
                   EXIT PERFORM CYCLE
               END-IF

               PERFORM VARYING WS-LEN FROM 1 BY 1 UNTIL WS-LEN > 10
                   IF WS-CHAR = WS-VOWELS(WS-LEN:1) THEN
                       MOVE 1 TO WS-VOWEL-FLAG
                       EXIT PERFORM
                   END-IF
               END-PERFORM

               IF WS-VOWEL-FLAG = 1 THEN
                   IF WS-CHAR IS LOWER-CASE THEN
                       COMPUTE WS-NEW-CHAR =
                           CHAR(ORD(WS-CHAR) + 2)
                       IF WS-NEW-CHAR > 'z' THEN
                           COMPUTE WS-NEW-CHAR =
                               CHAR(ORD(WS-NEW-CHAR) - 26)
                       END-IF
                   ELSE
                       COMPUTE WS-NEW-CHAR =
                           CHAR(ORD(WS-CHAR) + 2)
                       IF WS-NEW-CHAR > 'Z' THEN
                           COMPUTE WS-NEW-CHAR =
                               CHAR(ORD(WS-NEW-CHAR) - 26)
                       END-IF
                   END-IF
                   MOVE WS-NEW-CHAR TO RESULT(WS-I:1)
               ELSE
                   IF WS-CHAR IS LOWER-CASE THEN
                       MOVE FUNCTION UPPER-CASE(WS-CHAR)
                           TO RESULT(WS-I:1)
                   ELSE
                       MOVE FUNCTION LOWER-CASE(WS-CHAR)
                           TO RESULT(WS-I:1)
                   END-IF
               END-IF
           END-PERFORM.

           GOBACK.
       END PROGRAM ENCODE.
