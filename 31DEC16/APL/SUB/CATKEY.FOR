      SUBROUTINE CATKEY (OPCODE, IVOL, CNO, KEYWRD, NUMKEY, LOCS,
     *   VALUES, KEYTYP, BUFFER, IERR)
C-----------------------------------------------------------------------
C! reads/writes the Keyword section of an AIPS header file
C# Header Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2006
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Reads or writes KEYWORDs from or to an AIPS image (or uv) header.
C   The order of the keywords is arbitrary.  Uses LUN 15, so any CA or
C   CB files must be closed before calling this routine.
C   Inputs:
C      OPCODE   C*4        Operation desired, 'READ', 'WRIT',
C                             'ALL ' => Read all.
C                             'REED' => no error msg if some missing
C      IVOL     I          File disk number
C      CNO      I          File catalog block number
C   In/out:
C      KEYWRD   C(*)*8     Keywords to read/write: output on ALL
C      NUMKEY   I          Number of keywords to read/write.
C                             Input on OPCODE='ALL' = max. to read.
C                             Output on OPCODE='ALL' = no. read.
C      LOCS     I(NUMKEY)  The word offset of first short integer
C                          word of keyword value in array VALUES.
C                          Output on READ, input on WRIT.
C                          On READ this value will be -1 for keywords
C                          not found.
C      VALUES   I          The array of keyword values; due to word
C                          alignment problems on some machines double
C                          precision values should be copied,
C                          eg. if the  5th keyword (XXX) is a double
C                          precision:
C                               IPOINT = LOCS(5)
C                               CALL COPY (NWDPDP, VALUES(IPOINT), XXX)
C                          Output on READ, input on WRIT
C      KEYTYP   I(NUMKEY)  The type code of the keywords:
C                             1 = Double precision floating
C                             2 = Single precision floating
C                             3 = Character string (8 HOLLERITH chars)
C                             4 = integer
C                             5 = Logical
C   Output:
C      BUFFER   I(256)     Scratch buffer
C      IERR     I          Return code, 0=>OK,
C                             1-10 => ZFIO error
C                             19   => unrecognized data type.
C                             20   => bad OPCODE
C                             20+n => n keywords not found on READ.
C                                     This produces messages at level 6
C                                     suppress them w MSGSUP if needed
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, KEYWRD(*)*(*)
      INTEGER   IVOL, CNO, NUMKEY, BUFFER(256), LOCS(*), VALUES(*),
     *   KEYTYP(*), IERR
C
      INTEGER   J, IPOINT, NWORD, JSAV, JP, CLUN, CIND, MAXREC, LREC, I,
     *   CHWORD(5), MAXKEY, HEDKEY, RECNO, GOTIT(512)
      CHARACTER OP*4, CTEST*8, PHNAME*48
      LOGICAL   T, F, DOREAD, REDALL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA CLUN /15/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Check OPCODE
      OP = 'WRIT'
      REDALL = OPCODE.EQ.'ALL'
      DOREAD = (OPCODE.EQ.'READ') .OR. (OPCODE.EQ.'REED') .OR. (REDALL)
      IF (DOREAD) OP = 'READ'
C                                       Unknown opcode
      IF ((OPCODE.EQ.'WRIT') .OR. (DOREAD)) GO TO 10
         IERR = 20
         WRITE (MSGTXT,1000) OPCODE
         GO TO 990
C                                       check types
 10   IF ((NUMKEY.LE.0) .AND. (.NOT.REDALL)) THEN
         IERR = 18
         WRITE (MSGTXT,1010) NUMKEY
         GO TO 990
         END IF
      IF (.NOT.REDALL) THEN
         DO 15 I = 1,NUMKEY
            IF (KEYWRD(I).EQ.' ') THEN
               WRITE (MSGTXT,1011) I
               CALL MSGWRT (8)
               END IF
            IF (((KEYTYP(I).LT.1) .OR. (KEYTYP(I).GT.5)) .AND.
     *         (.NOT.DOREAD)) THEN
               WRITE (MSGTXT,1012) I, KEYTYP(I)
               IERR = 19
               CALL MSGWRT (8)
               END IF
 15         CONTINUE
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Open CB file
      CALL ZPHFIL ('CB', IVOL, CNO, 1, PHNAME, IERR)
      CALL ZOPEN (CLUN, CIND, IVOL, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR
         GO TO 990
         END IF
      LREC = 2
      CALL ZFIO ('READ', CLUN, CIND, LREC, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1016) IERR, 'READ', LREC
         GO TO 980
         END IF
      MAXREC = BUFFER(1)
      HEDKEY = BUFFER(2)
C                                       Fill LOCS with -1 on READ
      IF (DOREAD) CALL FILL (NUMKEY, -1, LOCS)
      IF (OPCODE.NE.'ALL') THEN
         IF (NUMKEY.GT.512) THEN
            CALL FILL (512, -1, GOTIT)
            MSGTXT = 'WARNING: CATKEY LIMITS CHECKING TO 512 KEYWORDS'
            CALL MSGWRT (6)
         ELSE
            CALL FILL (NUMKEY, -1, GOTIT )
            END IF
         END IF
      MAXKEY = NUMKEY
C                                       Initialize data type lengths
      CHWORD(1) = NWDPDP
      CHWORD(2) = 1
      CHWORD(3) = 2
      CHWORD(4) = 1
      CHWORD(5) = 1
      JSAV = 0
      IPOINT = 1
      IF (OPCODE.EQ.'ALL') NUMKEY = 0
      IF (HEDKEY.GT.0) THEN
         DO 50 I = 1,HEDKEY
            RECNO = I/51 + 2
            IF (RECNO.GT.LREC) THEN
               IF (OP.EQ.'WRIT') THEN
                  CALL ZFIO ('WRIT', CLUN, CIND, LREC, BUFFER, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1016) IERR, 'WRIT', LREC
                     GO TO 980
                     END IF
                  END IF
               LREC = RECNO
               CALL ZFIO ('READ', CLUN, CIND, LREC, BUFFER, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1016) IERR, 'READ', LREC
                  GO TO 980
                  END IF
               END IF
            JP = 5 * MOD (I,51) + 2
            CALL H2CHR (8, 1, BUFFER(JP), CTEST)
C                                       Search input list
            IF (.NOT.REDALL) THEN
               DO 20 J = 1,NUMKEY
                  IF (CTEST.EQ.KEYWRD(J)) GO TO 25
 20               CONTINUE
               GO TO 50
               END IF
C                                       Keyword found in file
 25         IF (OPCODE.EQ.'ALL') THEN
C                                       Reading all
C                                       Save it if not too many
               IF (NUMKEY.GE.MAXKEY) THEN
                  WRITE (MSGTXT,1025) CTEST, MAXKEY
                  CALL MSGWRT (6)
               ELSE
                  NUMKEY = NUMKEY + 1
                  KEYWRD(NUMKEY) = CTEST
                  KEYTYP(NUMKEY) = BUFFER(JP+4)
                  IF ((KEYTYP(NUMKEY).GE.1) .AND. (KEYTYP(NUMKEY).LE.5))
     *               THEN
                     NWORD = CHWORD(KEYTYP(NUMKEY))
                  ELSE
                     NWORD = 1
                     END IF
                  CALL RCOPY (NWORD, BUFFER(JP+2), VALUES(IPOINT))
                  LOCS(NUMKEY) = IPOINT
                  IPOINT = IPOINT + NWORD
                  END IF
            ELSE IF (OPCODE.EQ.'WRIT') THEN
C                                       Write new value
               IF (KEYTYP(J).NE.BUFFER(JP+4)) THEN
C                                       Types don't match - change
                  WRITE (MSGTXT,1026) CTEST, KEYTYP(J), BUFFER(JP+4)
                  CALL MSGWRT (6)
                  MSGTXT = 'NOTE: CHANGING KEYWORD DATA TYPE'
                  CALL MSGWRT (6)
                  NWORD = CHWORD(KEYTYP(J))
                  IF ((NWORD.LT.1) .OR. (NWORD.GT.2)) NWORD = 1
                  CALL RCOPY (NWORD, VALUES(LOCS(J)), BUFFER(JP+2))
                  BUFFER(JP+4) = KEYTYP(J)
                  IF (J.LE.512) GOTIT(J) = 1
                  JSAV = JSAV + 1
C                                       Type agree, replace value
               ELSE
                  IF ((KEYTYP(J).GE.1) .AND. (KEYTYP(J).LE.5)) THEN
                     NWORD = CHWORD(KEYTYP(J))
                  ELSE
                     NWORD = 1
                     END IF
                  CALL RCOPY (NWORD, VALUES(LOCS(J)), BUFFER(JP+2))
                  IF (J.LE.512) GOTIT(J) = 1
                  JSAV = JSAV + 1
                  END IF
            ELSE IF ((OPCODE.EQ.'READ') .OR. (OPCODE.EQ.'REED')) THEN
C                                       Reading, save value
               KEYTYP(J) = BUFFER(JP+4)
               IF ((KEYTYP(J).GE.1) .AND. (KEYTYP(J).LE.5)) THEN
                  NWORD = CHWORD(KEYTYP(J))
               ELSE
                  NWORD = 1
                  END IF
               LOCS(J) = IPOINT
               CALL RCOPY (NWORD, BUFFER(JP+2), VALUES(IPOINT))
               IPOINT = IPOINT + NWORD
               IF (J.LE.512) GOTIT(J) = 1
               JSAV = JSAV + 1
               END IF
 50         CONTINUE
         END IF
C                                       Write the rest
      IF ((OPCODE.EQ.'WRIT') .AND. (JSAV.LT.NUMKEY)) THEN
         DO 90 J = 1,NUMKEY
            IF ((J.LE.512) .AND. (GOTIT(J).LE.0)) THEN
               HEDKEY = HEDKEY + 1
               RECNO = HEDKEY/51 + 2
               IF (RECNO.GT.LREC) THEN
                  CALL ZFIO ('WRIT', CLUN, CIND, LREC, BUFFER, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1016) IERR, 'WRIT', LREC
                     GO TO 980
                     END IF
                  LREC = RECNO
                  CALL FILL (256, 0, BUFFER)
C                                       Expand the file
                  IF (LREC.GT.MAXREC) THEN
                     I = 2
                     CALL ZEXPND (CLUN, IVOL, PHNAME, I, IERR)
                     IF (IERR.NE.0) THEN
                        WRITE (MSGTXT,1050) IERR
                        GO TO 980
                        END IF
                     MAXREC = MAXREC + I
                     END IF
                  END IF
               JP = 5 * MOD (HEDKEY,51) + 2
               CALL CHR2H (8, KEYWRD(J), 1, BUFFER(JP))
               BUFFER(JP+4) = KEYTYP(J)
               IF ((KEYTYP(J).GE.1) .AND. (KEYTYP(J).LE.5)) THEN
                  NWORD = CHWORD(KEYTYP(J))
               ELSE
                  NWORD = 1
                  END IF
               CALL RCOPY (NWORD, VALUES(LOCS(J)), BUFFER(JP+2))
               END IF
 90         CONTINUE
         IF (LREC.NE.2) THEN
            CALL ZFIO ('WRIT', CLUN, CIND, LREC, BUFFER, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1016) IERR, 'WRIT', LREC
               GO TO 980
               END IF
            LREC = 2
            CALL ZFIO ('READ', CLUN, CIND, LREC, BUFFER, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1016) IERR, 'READ', LREC
               GO TO 980
               END IF
            END IF
         BUFFER(1) = MAXREC
         BUFFER(2) = HEDKEY
         END IF
      IF (OPCODE.EQ.'WRIT') THEN
         CALL ZFIO ('WRIT', CLUN, CIND, LREC, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1016) IERR, 'WRIT', LREC
            GO TO 980
            END IF
C                                       Reads
      ELSE IF ((OPCODE.EQ.'READ') .AND. (NUMKEY.GT.JSAV)) THEN
         IERR = 20 + (NUMKEY - JSAV)
         DO 95 J = 1,NUMKEY
            IF ((J.GT.512) .OR. (GOTIT(J).LT.0)) THEN
               IF (J.LE. 512) THEN
                  WRITE (MSGTXT,1095) KEYWRD(J), 'NOT'
               ELSE
                  WRITE (MSGTXT,1095) KEYWRD(J), 'MAYBE NOT'
                  END IF
               CALL MSGWRT (6)
               END IF
 95         CONTINUE
         END IF
      CALL ZCLOSE (CLUN, CIND, I)
      IF (IERR.EQ.0) IERR = I
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 980  CALL MSGWRT (8)
      CALL ZCLOSE (CLUN, CIND, I)
      GO TO 995
C
 990  CALL MSGWRT (8)
C
 995  WRITE (MSGTXT,1995) OPCODE, IVOL, CNO
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CATKEY: UNKNOWN OPCODE = ',A4)
 1010 FORMAT ('CATKEY: ILLEGAL NUMBER OF KEYWORDS =',I8)
 1011 FORMAT ('CATKEY: BLANK KEYWORD (NUMBER',I4,') MEANINGLESS')
 1012 FORMAT ('CATKEY: KEYWORD',I4,' ILLEGAL TYPE CODE =',I7)
 1015 FORMAT ('CATKEY: ERROR',I5,' OPENING THE HEADER FILE')
 1016 FORMAT ('CATKEY: ERROR',I5,1X,A4,'ING THE HEADER FILE RECORD',I5)
 1025 FORMAT ('CATKEY: KEYWORD ',A8,' IN EXCESS OF LIMIT',I4)
 1026 FORMAT ('CATKEY: EXISTING KEYWORD ',A8,' TYPES DON''T MATCH',2I3)
 1050 FORMAT ('CATKEY: ERROR EXPANDING CB FILE',I5)
 1095 FORMAT ('CATKEY: HEADER KEYWORD ',A8,1X,A,' FOUND')
 1995 FORMAT ('CATKEY: ERROR ON OPCODE=',A4,' VOL,CNO',I3,I6)
      END
