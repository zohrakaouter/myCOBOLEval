       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-CLOSEST-VOWEL.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-INDEX              PIC 9(3) VALUE 0.
       01 WS-VOWEL-FOUND        PIC X VALUE 'N'.
       01 WS-LENGTH             PIC 9(3) VALUE 0.
       01 WS-CURRENT-CHAR       PIC X VALUE SPACE.
       01 VOWELS                PIC X(10) VALUE 'AEIOUaeiou'.
       
       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-WORD PIC X(100).
           05 RESULT PIC X(100).

      * You are given a word. Your task is to find the closest vowel that stands between
      * two consonants from the right side of the word (case sensitive).
      * 
      * Vowels in the beginning and ending doesn't count. Return empty string if you didn't
      * find any vowel met the above condition.
      * 
      * You may assume that the given string contains English letter only.
      * 
      * Example:
      * get_closest_vowel("yogurt") ==> "u"
      * get_closest_vowel("FULL") ==> "U"
      * get_closest_vowel("quick") ==> ""
      * get_closest_vowel("ab") ==> ""
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE FUNCTION LENGTH(L-WORD) TO WS-LENGTH

           PERFORM VARYING WS-INDEX FROM WS-LENGTH BY -1
               UNTIL WS-INDEX < 2 OR WS-VOWEL-FOUND = 'Y'
               MOVE FUNCTION REVERSE(L-WORD(WS-INDEX:1)) TO WS-CURRENT-CHAR
               IF WS-CURRENT-CHAR NOT EQUAL SPACES
                 AND WS-CURRENT-CHAR NOT IN VOWELS
                   AND FUNCTION REVERSE(L-WORD(WS-INDEX-1:1)) IN VOWELS
                   AND FUNCTION REVERSE(L-WORD(WS-INDEX-2:1)) NOT IN VOWELS
                   MOVE FUNCTION REVERSE(L-WORD(WS-INDEX-1:1)) TO RESULT
                   MOVE 'Y' TO WS-VOWEL-FOUND
               END-IF
           END-PERFORM

           IF WS-VOWEL-FOUND NOT = 'Y'
               MOVE SPACE TO RESULT
           END-IF

           GOBACK.
       END PROGRAM GET-CLOSEST-VOWEL.
