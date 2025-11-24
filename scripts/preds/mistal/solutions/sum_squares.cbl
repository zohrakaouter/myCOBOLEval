       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUM-SQUARES.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-TOTAL PIC S9(10) VALUE 0.
       01 WS-INDEX PIC 9(2).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 3 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * This function will take a list of integers. For all entries in the list, the function shall square the integer entry if its index is a
      * multiple of 3 and will cube the integer entry if its index is a multiple of 4 and not a multiple of 3. The function will not
      * change the entries in the list whose indexes are not a multiple of 3 or 4. The function shall then return the sum of all entries.
      * 
      * Examples:
      * For lst = [1,2,3] the output should be 6
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 3
               SET NI TO WS-INDEX
               EVALUATE TRUE
                   WHEN FUNCTION MOD(WS-INDEX, 3) = 0
                       COMPUTE L-LST(NI) = L-LST(NI) ** 2
                   WHEN FUNCTION MOD(WS-INDEX, 4) = 0 AND
                        FUNCTION MOD(WS-INDEX, 3) <> 0
                       COMPUTE L-LST(NI) = L-LST(NI) ** 3
               END-EVALUATE
               ADD L-LST(NI) TO WS-TOTAL
           END-PERFORM

           MOVE WS-TOTAL TO RESULT
           GOBACK.
       END PROGRAM SUM-SQUARES.
