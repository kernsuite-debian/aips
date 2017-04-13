      SUBROUTINE FILOPN (LUN, NAMS, ASSOC, TYPE, VER, ERROR)
C-----------------------------------------------------------------------
C! open image, associated, or scratch file (WaWa system)
C# IO-WaWa
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   WaWa IO system: Open the file specified by NAMS and associate
C   it with Logical Unit number LUN.
C   Inputs:
C      LUN       I     Logical Unit Number
C      ASSOC     L     File is an associated file,i.e. not catalogd
C                      ASSOC will be taken as FALSE if NAMS(8)='SCxx'
C      TYPE      C*2   Associated file type; ignored if ASSOC is false
C      VER       I     Associated file version; ignored if ASSOC is fal
C   In/Out:
C      NAMS      C*36  NAMESTRING specifying catalog block to which
C                      file is associated: NAME,CLASS,CATTYPE,SEQ,VOL,
C                      USID.  NAME,CLASS,USID ignored for scratch files.
C   Outputs:
C      ERROR     I     Error code:  0 => o.k.
C                           2 => input error: bad or in use LUN
C                          10 => catalog error
C                          12 => map not found
C                          13 => extension file not in catalog
C                          14 => no room in FILTAB
C                          22 => volume not available
C                          26 => open error
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      CHARACTER NAMS*36, TYPE*2
      INTEGER   LUN, VER, ERROR
      LOGICAL   ASSOC
C
      CHARACTER DNAMS*36, STAT*4, PNAM*48, DTYPE*2, DNAME*12, DCLASS*6,
     *   DPTYPE*2
      INTEGER   DVER, IVER, SEQ, USID, BFIL, DFIL, I, J, BADLUN, DVOL,
     *   NOROOM, NOFIND, EFIND, CATER, OPNER, WARN, EXTER, IERR, IFIL
      LOGICAL   MAIN, NDISK, MAP, LSCR, F, T
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA EFIND, OPNER, BADLUN, NOROOM, CATER, EXTER, WARN, NOFIND
     *   /12, 26, 2, 14, 10, 13, 6, 5/
      DATA F, T /.FALSE.,.TRUE./
C-----------------------------------------------------------------------
C                                       Check if LUN in range
      ERROR = BADLUN
      IF ((LUN.LT.8) .OR. (LUN.GT.30)) GO TO 999
C                                       Get device type
      NDISK = DEVTAB(LUN).NE.0
C                                       Map or "file" i/o
      MAP = (LUN.GE.16) .AND. (LUN.LE.25)
      MAIN = .NOT.ASSOC
C                                       Search LUN table
      BFIL = 1
      DFIL = 5
      IF (.NOT.MAP) GO TO 20
         BFIL = 6
         DFIL = EFIL
C                                       Is LUN in use?
 20   DO 25 IFIL = BFIL,DFIL
         IF (FILTAB(POLUN,IFIL).EQ.LUN) GO TO 999
 25      CONTINUE
C                                       Is there room in table
      DO 30 IFIL = BFIL,DFIL
         IF (FILTAB(POLUN,IFIL).EQ.0) GO TO 40
 30      CONTINUE
      ERROR = NOROOM
      GO TO 999
C                                       Fill in some values
 40   FILTAB(POLUN,IFIL) = LUN
      FILTAB(POCAT,IFIL) = 1
      FILTAB(POIOP,IFIL) = 3
      FILTAB(POASS,IFIL) = 1
      IF (MAIN) FILTAB(POASS,IFIL) = 0
C                                       Disks: find in catalog
      IF (NDISK) GO TO 300
C                                       Copy NAMS into temporary area
         CALL PRENAM (NAMS, DNAMS, LSCR)
         IF (.NOT.LSCR) GO TO 100
            MAIN = T
            FILTAB(POASS,IFIL) = 0
C                                       Make Namestr intelligble
 100     CALL WAWA2A (DNAMS, DNAME, DCLASS, SEQ, DPTYPE, DVOL, USID)
C                                       find file in catalog
         CALL CATDIR ('SRCH', DVOL, FILTAB(POCAT,IFIL), DNAME, DCLASS,
     *      SEQ, DPTYPE, USID, STAT, WBUFF, IERR)
         CALL A2WAWA (DNAME, DCLASS, SEQ, DPTYPE, DVOL, USID, DNAMS)
         FILTAB(POVOL,IFIL) = DVOL
         IF (IERR.EQ.0) GO TO 120
            IF (IERR.EQ.NOFIND) GO TO 110
               ERROR = CATER
               WRITE (MSGTXT,1100) DVOL, IERR
               GO TO 980
 110        CONTINUE
               ERROR = EFIND
               WRITE (MSGTXT,1110) DVOL
               GO TO 980
C                                       Get Catalog block
 120     CALL CATIO ('READ', DVOL, FILTAB(POCAT,IFIL), CATBLK, 'REST',
     *      WBUFF, IERR)
         IF ((IERR.EQ.0) .OR. (IERR.EQ.WARN)) GO TO 140
            ERROR = CATER
            WRITE (MSGTXT,1100) DVOL, IERR
            GO TO 980
C                                       Check file type
 140     DVER = 1
         IVER = VER
         DTYPE = DPTYPE
C                                       Find extension file
         IF (MAIN) GO TO 200
            ERROR = EFIND
            CALL FNDEXT (TYPE, CATBLK, I)
            IF ((I.LE.0) .OR. (IVER.GT.I)) THEN
               WRITE (MSGTXT,1170) DVOL, TYPE, IVER
               ERROR = EXTER
               GO TO 980
               END IF
 180       DVER = IVER
           IF (IVER.LE.0) DVER = I
C                                       for maps set full window
 200  NAMS = DNAMS
      IF (.NOT.MAP) GO TO 300
         FILTAB(POBPX,IFIL) = 2
         FILTAB(PODIM,IFIL) = CATBLK(KIDIM)
         DO 210 I = 1,7
            J = I - 1
            FILTAB(PONAX+J,IFIL) = CATBLK(KINAX+J)
            FILTAB(POBLC+J,IFIL) = 1
            FILTAB(POTRC+J,IFIL) = CATBLK(KINAX+J)
            IF (I.GT.CATBLK(KIDIM)) FILTAB(POTRC+J,IFIL) = 1
            FILTAB(PODEP+J,IFIL) = 1
 210        CONTINUE
         FILTAB(POBL,IFIL) = 1
         FILTAB(POBL+1,IFIL) = 0
         FILTAB(PODEP+6,IFIL) = 0
C                                       Generate file name & open file
 300  CALL ZPHFIL (DTYPE, FILTAB(POVOL,IFIL), FILTAB(POCAT,IFIL),
     *   DVER, PNAM, IERR)
      CALL ZOPEN (LUN, FILTAB(POFIN,IFIL), FILTAB(POVOL,IFIL), PNAM,
     *   MAP, F, T, IERR)
      ERROR = 0
      IF (IERR.EQ.0) GO TO 999
         ERROR = OPNER
         IF (IERR.LE.3) ERROR = 10 * IERR - 8
         IF (IERR.EQ.5) ERROR = NOROOM
         WRITE (MSGTXT,1300) IERR, FILTAB(POVOL,IFIL), DTYPE, DVER
C                                       Errors
 980  FILTAB (POLUN,IFIL) = 0
      CALL MSGWRT (8)
      CALL PRTNAM (DNAMS, 8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('FILOPN: CATALOG I/O ERROR ON VOL',I6,' ERROR',I6)
 1110 FORMAT ('FILOPN: CATBLOCK NOT FOUND ON VOL ',I2)
 1120 FORMAT ('FILOPN: BAD STATUS CAN''T OPEN FOR WRITE ON VOL ',I2)
 1170 FORMAT ('FILOPN: CAN''T FIND EXT FILE ON VOL ',I2,' EXTYP ',A2,
     *   ' VER ',I4)
 1300 FORMAT ('FILOPN: OPEN ERROR',I6,' ON VOL ',I2,' EXTYP ',A2,
     *   ' VER ',I4)
      END
