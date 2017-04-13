      SUBROUTINE ZTRLOG (LLEN, LOGNAM, XLEN, XLATED, XLNB, IERR)
C-----------------------------------------------------------------------
C! translate a logical name
C# Z2 System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Translate a logical name.
C   Inputs:
C      LLEN     I*2         Length of LOGNAM (i.e., as in LEN (LOGNAM))
C      LOGNAM   C*(*)       Logical name
C      XLEN     I*2         Length of XLATED (i.e., as in LEN (XLATED))
C   Output:
C      XLATED   C*(*)       Translation
C      XLNB     I*2         Position of the last non-blank character in
C                           the translation
C      IERR     I*2         Error return code: 0 => no error
C                              1 => no translation (no message)
C                              2 => other error
C                              3 => non-disk device
C   VMS version - calls ZERROR for system errors => should not be
C   called by routines called by MSGWRT.  In UNIX, no messages since
C   called by ZDAOPN,... which are called by MSGWRT.
C-----------------------------------------------------------------------
      INTEGER   LLEN, XLEN, XLNB, IERR
      CHARACTER LOGNAM*(*), XLATED*(*)
C
      CHARACTER TLATED*128, THOLD*256, CTEMP*256, TLOG*128
      INTEGER*2 ITEM2(8)
      INTEGER*4 SYS$TRNLNM, ITEM4(4), JERR, LLNB, LHOLD, TLNB, I, J,
     *   ID, IT, SYS$GETDVIW, DEVTYP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE '($LNMDEF)'
      INCLUDE '($DVIDEF)'
      INCLUDE '($DCDEF)'
      INCLUDE '($SSDEF)'
      EQUIVALENCE (ITEM2(1), ITEM4(1))
C-----------------------------------------------------------------------
      IERR = 0
      XLNB = 0
      XLATED = ' '
      LHOLD = 0
      TLOG = LOGNAM
      ID = 0
C                                       Set up item list.
      ITEM2(1) = LEN (TLATED)
      ITEM2(2) = LNM$_STRING
      ITEM4(2) = %LOC (TLATED)
      ITEM4(3) = %LOC (TLNB)
      ITEM4(4) = 0
C                                       Get last non-blank in TLOG.
 10   LLNB = INDEX (TLOG, ' ') - 1
      IF (LLNB.LE.0) LLNB = LEN (TLOG)
      IF (TLOG(LLNB:LLNB).EQ.':') LLNB = LLNB - 1
      TLNB = 0
      TLATED = ' '
C                                       Translate logical name.
      JERR = SYS$TRNLNM (LNM$M_CASE_BLIND, 'LNM$FILE_DEV',
     *   TLOG(1:LLNB), ,ITEM2)
C                                       Error or done
 20   IF (JERR.NE.1) THEN
         IERR = 0
         IF (JERR.NE.SS$_NOLOGNAM) THEN
            CALL ZERROR ('ZTRLOG', JERR, ' ', -999, .FALSE.)
            IERR = 2
C                                       is "logical" a disk device
         ELSE IF (ID.LE.0) THEN
C                                       Set up item list.
            ITEM2(1) = 4
            ITEM2(2) = DVI$_DEVCLASS
            ITEM4(2) = %LOC (DEVTYP)
            ITEM4(3) = 0
            ITEM4(4) = 0
            JERR = SYS$GETDVIW ( , , TLOG(1:LLNB), ITEM2, , , , )
            IF ((JERR.NE.1) .OR. (DEVTYP.NE.DC$_DISK)) THEN
               IERR = 1
               IF (JERR.EQ.1) THEN
                  IERR = 3
                  LLNB = MIN (LLNB, 50)
                  WRITE (MSGTXT,1020) TLOG(1:LLNB)
                  CALL MSGWRT (6)
                  END IF
               END IF
            END IF
         IF (IERR.EQ.0) THEN
            IF (LHOLD.GT.0) THEN
               XLATED = TLOG(1:LLNB) // THOLD(1:LHOLD)
            ELSE
               XLATED = TLOG(1:LLNB)
               END IF
            XLNB = LLNB + LHOLD
            END IF
C                                       prepare to try another layer
      ELSE
         ID = ID + 1
         IF (LHOLD.GT.0) THEN
            CTEMP = THOLD
            I = 1
            IF ((TLATED(TLNB:TLNB).EQ.':') .AND. (CTEMP(1:1).EQ.':'))
     *         I = 2
            IF ((TLATED(TLNB:TLNB).EQ.']') .AND. (CTEMP(1:1).EQ.':'))
     *         I = 2
            IF ((TLATED(TLNB:TLNB).EQ.']') .AND. (CTEMP(1:2).EQ.':['))
     *         THEN
               TLNB = TLNB - 1
               I = 3
               END IF
            THOLD = TLATED(1:TLNB) // CTEMP(I:LHOLD)
            LHOLD = LHOLD - I + 1 + TLNB
         ELSE
            THOLD = TLATED
            LHOLD = TLNB
            END IF
         I = INDEX (THOLD, ':')
         J = INDEX (THOLD, '[')
         IT = TLNB + 1
         IF (I.GT.0) THEN
            IF (J.GT.0) IT = MIN (I, J)
            IF (J.LE.0) IT = I
         ELSE
            IF (J.GT.0) IT = J
            END IF
         IF ((IT.EQ.1) .OR. (ID.GT.20)) THEN
            JERR = 999
            GO TO 20
         ELSE
            CTEMP = THOLD(IT:)
            TLOG = THOLD(:IT-1)
            THOLD = CTEMP
            LHOLD = LHOLD - IT + 1
            GO TO 10
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ZTRLOG: DEVICE ',A,' IS NOT A DISK')
      END
