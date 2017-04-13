      SUBROUTINE ZTAPE (OP, LUN, FIND, COUNT, IERR)
C-----------------------------------------------------------------------
C! mount, dismount, position, write EOF, etc. for tapes
C# Z Tape
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
C   Performs standard tape manipulating functions.
C   Inputs:
C       OP      C*4   Operation to be performed. 4 characters ASCII.
C                     'ADVF' = advance file marks
C                     'ADVR' = advance records
C                     'BAKF' = backspace file marks.
C                     'BAKR' = backspace records.
C                     'REWI' = rewind the tape on unit LUN
C                     'WEOF' = write end of file on unit LUN: writes 2
C                              EOFs, positions tape after first one
C                     'BEGW' = "begin write": backs just over the last
C                              EOF, writes 2 EOFs, backs over the 2nd -
C                              should be done before writing.  It is
C                              required on Exabyte 8200s anywhere except
C                              BOM and EOM.
C                     'AEOI' = Advance to end of information (2 EOFs)
C       LUN     I     logical unit number == 30 + DriveNumber
C       FIND    I     FTAB pointer.
C       COUNT   I     Number of records or file marks to skip.
C   Outputs:
C       IERR  I       Error return: 0 => ok
C                        1 = File not open
C                        2 = Input specification error.
C                        3 = I/O error.
C                        4 = End Of File
C                        5 = Beginning Of Medium
C                        6 = End Of Medium
C   Old Generic version - uses ZTAP2 and ZTAPR.  Now VMS version using
C   a solely elementary ZTAP2 below.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   LUN, FIND, COUNT, IERR
C
      LOGICAL   T, F, MAP
      CHARACTER ERRNAM(4)*8, OPTAB(8)*4, HER*4, LOP*4
      INTEGER   MTLUN1, IER, IOP, NOP, FOFF, IEREOF, IERBOM, IEREOM,
     *   IERIO, I, JC, NB
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA MTLUN1 /31/
      DATA IEREOF, IERBOM, IEREOM, IERIO /4, 5, 6, 3/
      DATA ERRNAM /'IO ERROR', 'END FILE','BEG.TAPE','END TAPE'/
      DATA T, F /.TRUE.,.FALSE./
      DATA NOP, OPTAB /8, 'ADVF','ADVR','BAKF','BAKR', 'REWI','WEOF',
     *   'BEGW','AEOI'/
C-----------------------------------------------------------------------
C                                       check input errors
      IF (FTAB(FIND).NE.LUN) THEN
         IERR = 1
         WRITE (MSGTXT,1000) LUN
         GO TO 990
         END IF
      IERR = 2
      IF ((LUN.LT.MTLUN1) .OR. (LUN.GE.(MTLUN1+NTAPED))) THEN
         WRITE (MSGTXT,1010) LUN
         GO TO 990
         END IF
      DO 20 IOP = 1,NOP
         IF (OP.EQ.OPTAB(IOP)) GO TO 25
 20      CONTINUE
      WRITE (MSGTXT,1020) OP
      GO TO 990
 25   IF ((IOP.LE.4) .AND. (COUNT.LE.0)) THEN
         WRITE (MSGTXT,1025) COUNT, OP
         GO TO 990
         END IF
C                                       input OK !
C                                       Remote tapes:
      IF (TPNAME(LUN+1-MTLUN1).NE.'LOCAL') THEN
         CALL ZTAPR (OP, LUN, FIND, COUNT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR
            IF (IERR.EQ.3) THEN
               CALL MSGWRT (7)
               MSGTXT = 'This usually means PARITY ERROR'
            ELSE IF (IERR.EQ.4) THEN
               CALL MSGWRT (7)
               MSGTXT = 'This usually means unexpected END-OF-FILE'
            ELSE IF (IERR.EQ.5) THEN
               CALL MSGWRT (7)
               MSGTXT = 'This usually means unexpected ' //
     *            'BEGINNING-OF-TAPE'
            ELSE IF (IERR.EQ.6) THEN
               CALL MSGWRT (7)
               MSGTXT = 'This usually means you''re at END-OF-TAPE'
               END IF
            GO TO 990
            END IF
         GO TO 999
         END IF
C                                       Local tapes
C                                       map or non-map file?
 50   I = (NBTB1*NTAB1 + NBTB2*NTAB2)/2
      MAP = FIND.GT.I
      FOFF = FIND + NMOFF
      IF (MAP) FOFF = FIND + MOFF
      IERR = 0
C                                       branch to OP
      GO TO (100, 200, 300, 400, 500, 600, 700, 800), IOP
C                                       Advance file
 100  DO 110 JC = 1,COUNT
         LOP = 'ADVF'
         CALL ZTAP2 (LOP, FTAB(FOFF), IER)
C                                       EOF condition normal: no error
         IF ((IER.NE.0) .AND. (IER.NE.IEREOF)) GO TO 900
 110     CONTINUE
      GO TO 999
C                                       Advance records; leave just
C                                       beyond EOF if encountered
 200  DO 210 JC = 1,COUNT
         LOP = 'ADVR'
         CALL ZTAP2 (LOP, FTAB(FOFF), IER)
C                                       No conditions normal
         IF (IER.NE.0) GO TO 900
 210     CONTINUE
      GO TO 999
C                                       Back COUNT file marks
C                                       then advance 1 unless BOM
 300  DO 310 JC = 1,COUNT
         LOP = 'BAKF'
         CALL ZTAP2 (LOP, FTAB(FOFF), IER)
C                                       EOF normal
         IF ((IER.NE.0) .AND. (IER.NE.IEREOF)) THEN
C                                       BOM not error on last
            IF ((JC.EQ.COUNT) .AND. (IER.EQ.IERBOM)) GO TO 999
            GO TO 900
            END IF
 310     CONTINUE
C                                       advance beyond tape mark
      LOP = 'ADVF'
      CALL ZTAP2 (LOP, FTAB(FOFF), IER)
      IF ((IER.EQ.0) .OR. (IER.EQ.IEREOF)) GO TO 999
         JC = 1
         GO TO 900
C                                       Back COUNT records
C                                       leave just beyond EOF if needed
 400  DO 410 JC = 1,COUNT
         LOP = 'BAKR'
         CALL ZTAP2 (LOP, FTAB(FOFF), IER)
         IF (IER.NE.0) THEN
            IF ((JC.EQ.COUNT) .AND. (IER.EQ.IERBOM)) GO TO 999
C                                       advance record on EOF
            IF (IER.EQ.IEREOF) THEN
               LOP = 'ADVR'
               CALL ZTAP2 (LOP, FTAB(FOFF), IER)
               IF ((JC.EQ.COUNT) .AND. ((IERR.EQ.0) .OR.
     *            (IERR.EQ.IEREOF))) GO TO 999
               END IF
            GO TO 900
            END IF
 410     CONTINUE
      GO TO 999
C                                       Rewind
 500  LOP = 'REWI'
      JC = 1
      CALL ZTAP2 (LOP, FTAB(FOFF), IER)
C                                       EOF + BOM normal
      IF ((IER.EQ.0) .OR. (IER.EQ.IERBOM)) GO TO 999
      GO TO 900
C                                       Write EOF
 600  DO 610 JC = 1,2
         LOP = 'WEOF'
         CALL ZTAP2 (LOP, FTAB(FOFF), IER)
         IF ((IER.NE.0) .AND. (IER.NE.IEREOF)) GO TO 620
 610     CONTINUE
      JC = 3
 620  NB = JC - 2
      IF (NB.LE.0) GO TO 900
      DO 630 JC = 1,NB
         LOP = 'BAKF'
         CALL ZTAP2 (LOP, FTAB(FOFF), IER)
         IF ((IER.NE.0) .AND. (IER.NE.IEREOF)) GO TO 900
 630     CONTINUE
      GO TO 999
C                                       Begin write to tape w BAKF
 700  LOP = 'BAKF'
      JC = -1
      CALL ZTAP2 (LOP, FTAB(FOFF), IER)
      NB = 1
C                                       BOM normal but special
      IF (IER.EQ.IERBOM) THEN
         NB = 2
C                                       EOF normal
      ELSE IF ((IER.NE.0) .AND. (IER.NE.IEREOF)) THEN
         GO TO 900
         END IF
      DO 710 JC = 1,2
         LOP = 'WEOF'
         CALL ZTAP2 (LOP, FTAB(FOFF), IER)
         IF ((IER.NE.0) .AND. (IER.NE.IEREOF)) GO TO 900
 710     CONTINUE
      DO 720 JC = 1,NB
         LOP = 'BAKF'
         CALL ZTAP2 (LOP, FTAB(FOFF), IER)
         IF ((IER.NE.0) .AND. (IER.NE.IEREOF) .AND. 
     *      ((IER.NE.IERBOM) .OR. (JC.NE.2))) GO TO 900
 720     CONTINUE
      GO TO 999
C                                       AEOI (old FNDEOT really)
 800  DO 810 JC =  1,10000
         LOP = 'ADVR'
         CALL ZTAP2 (LOP, FTAB(FOFF), IER)
         IF ((IER.NE.0) .AND. (IER.NE.IEREOF) .AND. (IER.NE.IEREOM))
     *      GO TO 900
         IF (IER.NE.0) GO TO 820
         LOP = 'ADVF'
         CALL ZTAP2 (LOP, FTAB(FOFF), IER)
         IF ((IER.NE.0) .AND. (IER.NE.IEREOF)) GO TO 900
 810     CONTINUE
      IERR = 3
      WRITE (MSGTXT,1810)
      GO TO 990
C                                       back over last EOF
 820  LOP = 'BAKF'
      CALL ZTAP2 (LOP, FTAB(FOFF), IER)
      IF ((IER.NE.0) .AND. (IER.NE.IEREOF) .AND. (IER.NE.IEREOM))
     *   GO TO 900
      JC = JC - 1
      WRITE (MSGTXT,1820) JC
      CALL MSGWRT (3)
      GO TO 999
C                                       handle errors 3 - 6.
 900  IERR = IER
      I = IERR - IERIO + 1
      IER = FTAB(FOFF+2)
      CALL ZHEX (IER, 4, HER)
      WRITE (MSGTXT,1900) LUN, ERRNAM(I), JC, OPTAB(IOP), LOP, HER
      CALL MSGWRT (7)
C                                       extra for unknowns
      IF (IERR.EQ.IERIO) THEN
         CALL ZERROR ('ZTAP2', IER, ' ', FTAB(FOFF), MAP)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTAPE: LUN ',I4,' NOT OPEN')
 1010 FORMAT ('ZTAPE: LUN',I4,' NOT ALLOWED FOR TAPES')
 1020 FORMAT ('ZTAPE: INVALID OPERATION: ',A4)
 1025 FORMAT ('ZTAPE: COUNT ',I6,' OP ',A4,' INVALID')
 1030 FORMAT ('REMOTE ZTAPE RETURNS ERROR',I6)
 1810 FORMAT ('ZTAPE: EOI NOT FOUND AFTER 10000 FILES!!')
 1820 FORMAT ('Advanced to end-of-information, skipping',I4,' files')
 1900 FORMAT ('ZTAPE: LUN',I3,1X,A8,' AT',I4,' OP OF ',A4,'/',A4,
     *   ' SYS ERR CODE ',A4)
      END
