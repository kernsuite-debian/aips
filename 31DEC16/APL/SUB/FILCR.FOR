      SUBROUTINE FILCR (NAMS, TYPE, NBLOCK, VER, ERROR)
C-----------------------------------------------------------------------
C! create associated or scratch non-map file
C# IO-WaWa
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   WaWa IO package:  Create an associated or scratch non-map file
C   Inputs:
C      NAMS      C*36  NAMESTRING specifying catalog block to which
C                      file is associated: NAME,CLASS,CATTYPE,SEQ,VOL,
C                      USID.  NAME,CLASS,USID ignored for scratch files.
C      TYPE      C*2   Associated file type for non-scratch files
C                      Ignored for scratch files
C   In/out:
C      NBLOCK    I      Number of 256-word blocks in file: in requested,
C                       out actual
C   Outputs:
C      VER       I      Version number of file created
C      ERROR     I      Error code: 0 => ok
C                             10 => catalog error
C                             12 => map not in catalog
C                             14 => no room for another ext. type
C                             21 => ZCREAT: file already exists
C                             22 => ZCREAT: volume unavailable
C                             23 => Disk space unavailable
C                             24 => Other create errors
C   Common: /MAPHDR/ modfified extensively for scratch file create
C      a little for associated file
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      CHARACTER NAMS*36, TYPE*2
      INTEGER   NBLOCK, VER, ERROR
C
      CHARACTER OPER*4, DNAM*36, STAT*4, PNAM*48, DTYPE*2, DNAME*12,
     *   DCLASS*6, DPTYPE*2, CTEMP*2
      HOLLERITH BL
      LOGICAL   LSCR, F, FCAT
      INTEGER   ISIZE, LSIZE, DVOL, SEQ, USID, CNO, CATER, NOEXT,
     *   EFIND, NOFIND, WARN, I, IERR, J, NSUB
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA F /.FALSE./
      DATA CATER, NOEXT, EFIND, NOFIND, WARN /10, 14, 12, 5, 6/
C-----------------------------------------------------------------------
C                                       Set up name parameters
      CALL CHR2H (2, '  ', 1, BL)
      FCAT = .FALSE.
      ERROR = CATER
      DTYPE = TYPE
      CALL PRENAM (NAMS, DNAM, LSCR)
C                                       Make Namestr intelligble
      CALL WAWA2A (DNAM, DNAME, DCLASS, SEQ, DPTYPE, DVOL, USID)
C                                       Add scratch file to Catalog
      IF (.NOT.LSCR) GO TO 100
         DTYPE = 'SC'
C                                       Find catalog slot
         OPER = 'OPEN'
         CNO = 1
         CALL CATDIR ('OPEN', DVOL, CNO, DNAME, DCLASS, SEQ, DPTYPE,
     *      USID, STAT, WBUFF, IERR)
         IF (IERR.EQ.0) GO TO 30
            NSUB = 1
            GO TO 980
 30      FCAT = .TRUE.
C                                       Fill in some parts of catalog
         CALL CATCLR (CATBLK)
         CALL CHR2H (12, DNAME, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, DCLASS, KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQ
         CATBLK(KIIMU) = USID
         CALL CHR2H (2, DPTYPE, KHPTYO, CATH(KHPTY))
         VER = 1
         GO TO 200
C                                       Add extension to reg map
 100  CONTINUE
C                                       Find catalog slot
         OPER = 'SRCH'
         CNO = 1
         CALL CATDIR ('SRCH', DVOL, CNO, DNAME, DCLASS, SEQ, DPTYPE,
     *      USID, STAT, WBUFF, IERR)
         IF (IERR.EQ.0) GO TO 120
            IF (IERR.EQ.NOFIND) GO TO 110
               NSUB = 1
               GO TO 980
 110        CONTINUE
               ERROR = EFIND
               WRITE (MSGTXT,1110)
               CALL MSGWRT (8)
               GO TO 985
C                                       Get block
 120     OPER = 'READ'
         CALL CATIO ('READ', DVOL, CNO, CATBLK, 'REST', WBUFF, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.WARN)) THEN
            NSUB = 2
            GO TO 980
            END IF
C                                       check format
         CALL FXHDEX (CATBLK)
C                                       Check if EXT type already exists
         DO 140 I = 1,KIEXTN
            CALL H2CHR (2, 1, CATH(KHEXT+I-1), CTEMP)
            IF (DTYPE.EQ.CTEMP) GO TO 170
 140        CONTINUE
C                                       is there room to add a new TYPE
         DO 150 I = 1,KIEXTN
            IF (CATH(KHEXT+I-1).EQ.0) GO TO 160
            CALL H2CHR (2, 1, CATH(KHEXT+I-1), CTEMP)
            IF (CTEMP.EQ.'  ') GO TO 160
 150        CONTINUE
         ERROR = NOEXT
         WRITE (MSGTXT,1150)
         CALL MSGWRT (8)
         GO TO 985
C                                       Add new type
 160     CONTINUE
            CALL CHR2H (2, DTYPE, 1, CATH(KHEXT+I-1))
            CATBLK(KIVER+I-1) = 1
            VER = 1
            GO TO 200
C                                       Update version no.
 170     CONTINUE
            J = KIVER + I - 1
            CATBLK(J) = CATBLK(J) + 1
            VER = CATBLK(J)
C                                       Update catalog block
 200  OPER = 'UPDT'
      CALL CATIO ('UPDT', DVOL, CNO, CATBLK, 'REST', WBUFF, IERR)
      IF (IERR.EQ.0) GO TO 210
         NSUB = 2
         GO TO 980
 210  FCAT = .TRUE.
C                                       Calculate file size
      ISIZE = NBLOCK
C                                       Actually create it
      ERROR = 0
      CALL ZPHFIL (DTYPE, DVOL, CNO, VER, PNAM, IERR)
      CALL ZCREAT (DVOL, PNAM, ISIZE, F, LSIZE, IERR)
      IF (IERR.EQ.0) NBLOCK = LSIZE
      IF (IERR.EQ.0) GO TO 999
         ERROR = 20 + IERR
         WRITE (MSGTXT,1210) IERR
         IF (IERR.EQ.5) WRITE (MSGTXT,1211) DVOL
         CALL MSGWRT (8)
         GO TO 985
C                                       Errors
 980  WRITE (MSGTXT,1980) SUBNAM(NSUB), OPER, IERR
      CALL MSGWRT (8)
C                                       Expand NAME, CLASS
 985  CALL A2WAWA (DNAME, DCLASS, SEQ, DPTYPE, DVOL, USID, DNAM)
      CALL PRTNAM (DNAM, 8)
      WRITE (MSGTXT,1985) DTYPE
      CALL MSGWRT (8)
      VER = 0
      IF (.NOT.FCAT) GO TO 999
C                                       Eliminate dummy catalog entry
         IF (.NOT.LSCR) GO TO 990
            CALL CATDIR ('CLOS', DVOL, CNO, DNAME, DCLASS, SEQ, DPTYPE,
     *          USID, STAT, WBUFF, IERR)
            GO TO 999
C                                       Reset catalog block
 990     CONTINUE
            J = KIVER + I - 1
            CATBLK(J) = CATBLK(J) - 1
            IF (CATBLK(J).EQ.0) CATH(KHEXT+I-1) = BL
            CALL CATIO ('UPDT', DVOL, CNO, CATBLK, 'REST', WBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1110 FORMAT ('FILCR: CAN''T FIND REQUESTED CATBLOCK ')
 1150 FORMAT ('FILCR: NO ROOM FOR NEW EXTENSION TYPE ')
 1210 FORMAT ('FILCR: ZCREAT ERROR OF ',I6)
 1211 FORMAT ('FILCR: PROHIBITED FROM CREATING FILE ON DISK',I4)
 1980 FORMAT ('FILCR: ',A6,2X,A4,'ERROR= ',I6)
 1985 FORMAT ('EXTYPE ',A2)
      END
