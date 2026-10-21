       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECON02.
      *-----------------------------------------------------------*
      *  RECON02 - DB2 VERSION OF THE RECONCILIATION               *
      *  1. LOAD IN1 AND IN2 INTO DB2 SESSION TABLES               *
      *  2. MATCH THE KEYS WITH SQL (JOIN AND NOT EXISTS)          *
      *  3. WRITE THE SAME MATCHED / UNMATCH / ERRORS FILES        *
      *-----------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN1 ASSIGN TO IN1.
           SELECT IN2 ASSIGN TO IN2.
           SELECT MATCHED ASSIGN TO MATCHED.
           SELECT UNMATCH ASSIGN TO UNMATCH.
           SELECT ERRFILE ASSIGN TO ERRFILE.
       DATA DIVISION.
       FILE SECTION.
       FD  IN1 RECORDING MODE F BLOCK CONTAINS 0 RECORDS.
       01  IN1-REC.
           05 IN1-ID       PIC X(6).
           05 IN1-NAME     PIC X(20).
           05 IN1-AMT      PIC X(9).
           05 FILLER       PIC X(45).
       FD  IN2 RECORDING MODE F BLOCK CONTAINS 0 RECORDS.
       01  IN2-REC.
           05 IN2-REF      PIC X(10).
           05 IN2-ID       PIC X(6).
           05 IN2-NAME     PIC X(20).
           05 IN2-AMT      PIC X(9).
           05 FILLER       PIC X(35).
       FD  MATCHED RECORDING MODE F BLOCK CONTAINS 0 RECORDS.
       01  MATCHED-REC     PIC X(80).
       FD  UNMATCH RECORDING MODE F BLOCK CONTAINS 0 RECORDS.
       01  UNMATCH-REC     PIC X(80).
       FD  ERRFILE RECORDING MODE F BLOCK CONTAINS 0 RECORDS.
       01  ERR-REC         PIC X(80).
       WORKING-STORAGE SECTION.
       EXEC SQL INCLUDE SQLCA END-EXEC.
       01  OUT-LINE        PIC X(80).
       01  CNT-IN1         PIC 9(6) VALUE 0.
       01  CNT-IN2         PIC 9(6) VALUE 0.
       01  CNT-MATCH       PIC 9(6) VALUE 0.
       01  CNT-UNM         PIC 9(6) VALUE 0.
       01  CNT-ERR         PIC 9(6) VALUE 0.
       01  EOF-IN1         PIC X VALUE 'N'.
       01  EOF-IN2         PIC X VALUE 'N'.
       01  WS-SQLCODE      PIC -9(9).
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  H-ID            PIC X(6).
       01  H-NAME          PIC X(20).
       01  H-AMT           PIC X(9).
       01  H-AMT2          PIC X(9).
           EXEC SQL END DECLARE SECTION END-EXEC.
           EXEC SQL
              DECLARE CJOIN CURSOR FOR
                 SELECT A.KEYID, A.CNAME, A.AMT, B.AMT
                   FROM SESSION.T1 A, SESSION.T2 B
                  WHERE A.KEYID = B.KEYID
                  ORDER BY A.KEYID
           END-EXEC.
           EXEC SQL
              DECLARE CONLY1 CURSOR FOR
                 SELECT A.KEYID, A.CNAME
                   FROM SESSION.T1 A
                  WHERE NOT EXISTS
                     (SELECT 1 FROM SESSION.T2 B
                       WHERE B.KEYID = A.KEYID)
                  ORDER BY A.KEYID
           END-EXEC.
           EXEC SQL
              DECLARE CONLY2 CURSOR FOR
                 SELECT B.KEYID, B.CNAME
                   FROM SESSION.T2 B
                  WHERE NOT EXISTS
                     (SELECT 1 FROM SESSION.T1 A
                       WHERE A.KEYID = B.KEYID)
                  ORDER BY B.KEYID
           END-EXEC.
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT IN1 IN2
                OUTPUT MATCHED UNMATCH ERRFILE
           PERFORM DECLARE-TABLES
           PERFORM LOAD-IN1
           PERFORM LOAD-IN2
           PERFORM RECON-MATCHES
           PERFORM RECON-ONLY1
           PERFORM RECON-ONLY2
           CLOSE IN1 IN2 MATCHED UNMATCH ERRFILE
           DISPLAY 'RECON02 (DB2 VERSION)'
           DISPLAY 'FILE1 RECORDS : ' CNT-IN1
           DISPLAY 'FILE2 RECORDS : ' CNT-IN2
           DISPLAY 'MATCHED       : ' CNT-MATCH
           DISPLAY 'UNMATCHED     : ' CNT-UNM
           DISPLAY 'ERRORS        : ' CNT-ERR
           STOP RUN.
       DECLARE-TABLES.
           EXEC SQL
              DECLARE GLOBAL TEMPORARY TABLE SESSION.T1
                 (KEYID CHAR(6) NOT NULL,
                  CNAME CHAR(20) NOT NULL,
                  AMT   CHAR(9) NOT NULL)
                 ON COMMIT PRESERVE ROWS
           END-EXEC
           PERFORM CHECK-SQL
           EXEC SQL
              DECLARE GLOBAL TEMPORARY TABLE SESSION.T2
                 (KEYID CHAR(6) NOT NULL,
                  CNAME CHAR(20) NOT NULL,
                  AMT   CHAR(9) NOT NULL)
                 ON COMMIT PRESERVE ROWS
           END-EXEC
           PERFORM CHECK-SQL.
       LOAD-IN1.
           PERFORM READ-IN1
           PERFORM UNTIL EOF-IN1 = 'Y'
              PERFORM INSERT-IN1
              PERFORM READ-IN1
           END-PERFORM.
       READ-IN1.
           READ IN1
              AT END MOVE 'Y' TO EOF-IN1
              NOT AT END ADD 1 TO CNT-IN1
           END-READ.
       INSERT-IN1.
           MOVE IN1-ID   TO H-ID
           MOVE IN1-NAME TO H-NAME
           MOVE IN1-AMT  TO H-AMT
           EXEC SQL
              INSERT INTO SESSION.T1 (KEYID, CNAME, AMT)
                 VALUES (:H-ID, :H-NAME, :H-AMT)
           END-EXEC
           PERFORM CHECK-SQL.
       LOAD-IN2.
           PERFORM READ-IN2
           PERFORM UNTIL EOF-IN2 = 'Y'
              PERFORM INSERT-IN2
              PERFORM READ-IN2
           END-PERFORM.
       READ-IN2.
           READ IN2
              AT END MOVE 'Y' TO EOF-IN2
              NOT AT END ADD 1 TO CNT-IN2
           END-READ.
       INSERT-IN2.
           MOVE IN2-ID   TO H-ID
           MOVE IN2-NAME TO H-NAME
           MOVE IN2-AMT  TO H-AMT
           EXEC SQL
              INSERT INTO SESSION.T2 (KEYID, CNAME, AMT)
                 VALUES (:H-ID, :H-NAME, :H-AMT)
           END-EXEC
           PERFORM CHECK-SQL.
       RECON-MATCHES.
           EXEC SQL OPEN CJOIN END-EXEC
           PERFORM CHECK-SQL
           PERFORM FETCH-JOIN
           PERFORM UNTIL SQLCODE = 100
              PERFORM CHECK-MATCH
              PERFORM FETCH-JOIN
           END-PERFORM
           EXEC SQL CLOSE CJOIN END-EXEC.
       FETCH-JOIN.
           EXEC SQL
              FETCH CJOIN INTO :H-ID, :H-NAME, :H-AMT, :H-AMT2
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              PERFORM CHECK-SQL
           END-IF.
       CHECK-MATCH.
           IF H-AMT IS NOT NUMERIC OR H-AMT2 IS NOT NUMERIC
              MOVE SPACES TO OUT-LINE
              STRING 'BAD AMOUNT   : ' H-ID ' ' H-AMT ' ' H-AMT2
                 DELIMITED BY SIZE INTO OUT-LINE
              WRITE ERR-REC FROM OUT-LINE
              ADD 1 TO CNT-ERR
           ELSE
              IF H-AMT NOT = H-AMT2
                 MOVE SPACES TO OUT-LINE
                 STRING 'AMT MISMATCH : ' H-ID ' ' H-AMT ' ' H-AMT2
                    DELIMITED BY SIZE INTO OUT-LINE
                 WRITE ERR-REC FROM OUT-LINE
                 ADD 1 TO CNT-ERR
              ELSE
                 MOVE SPACES TO OUT-LINE
                 STRING H-ID ' ' H-NAME ' ' H-AMT
                    DELIMITED BY SIZE INTO OUT-LINE
                 WRITE MATCHED-REC FROM OUT-LINE
                 ADD 1 TO CNT-MATCH
              END-IF
           END-IF.
       RECON-ONLY1.
           EXEC SQL OPEN CONLY1 END-EXEC
           PERFORM CHECK-SQL
           PERFORM FETCH-ONLY1
           PERFORM UNTIL SQLCODE = 100
              MOVE SPACES TO OUT-LINE
              STRING 'ONLY IN FILE1: ' H-ID ' ' H-NAME
                 DELIMITED BY SIZE INTO OUT-LINE
              WRITE UNMATCH-REC FROM OUT-LINE
              ADD 1 TO CNT-UNM
              PERFORM FETCH-ONLY1
           END-PERFORM
           EXEC SQL CLOSE CONLY1 END-EXEC.
       FETCH-ONLY1.
           EXEC SQL
              FETCH CONLY1 INTO :H-ID, :H-NAME
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              PERFORM CHECK-SQL
           END-IF.
       RECON-ONLY2.
           EXEC SQL OPEN CONLY2 END-EXEC
           PERFORM CHECK-SQL
           PERFORM FETCH-ONLY2
           PERFORM UNTIL SQLCODE = 100
              MOVE SPACES TO OUT-LINE
              STRING 'ONLY IN FILE2: ' H-ID ' ' H-NAME
                 DELIMITED BY SIZE INTO OUT-LINE
              WRITE UNMATCH-REC FROM OUT-LINE
              ADD 1 TO CNT-UNM
              PERFORM FETCH-ONLY2
           END-PERFORM
           EXEC SQL CLOSE CONLY2 END-EXEC.
       FETCH-ONLY2.
           EXEC SQL
              FETCH CONLY2 INTO :H-ID, :H-NAME
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              PERFORM CHECK-SQL
           END-IF.
       CHECK-SQL.
           IF SQLCODE NOT = 0
              MOVE SQLCODE TO WS-SQLCODE
              DISPLAY 'SQL ERROR, SQLCODE = ' WS-SQLCODE
              DISPLAY 'SQLERRMC = ' SQLERRMC
              MOVE 12 TO RETURN-CODE
              STOP RUN
           END-IF.