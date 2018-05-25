LOCAL INCLUDE 'CCFND.INC'
      REAL      XSEQ, XDISK, XVER, XECNT, XFLUX, XFACT
      HOLLERITH XINNAM(3), XINCLS(2)
      CHARACTER INNAM*12, INCLS*6
      INTEGER   ECOUNT, DATP(128,2), BUFFER(512), INSEQ, INDISK, INVERS,
     *   CNO, IUSER, NKEY, NCOL, OUTLUN, OUTIND
C
      COMMON /INPARM/ XINNAM, XINCLS, XSEQ, XDISK, XVER, XECNT, XFLUX,
     *   XFACT
      COMMON /CHPARM / INNAM, INCLS
      COMMON /CCFNDC/ DATP, BUFFER, INSEQ, ECOUNT, INDISK, INVERS, CNO,
     *   IUSER, NKEY, NCOL, OUTLUN, OUTIND
LOCAL END
      PROGRAM CCFND
C-----------------------------------------------------------------------
C! Finds clean components satifying user request
C# EXT-util Map-util Printer
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2009
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
C   CCFND is an AIPS task which makes two passes searching for
C   Clean Components (CC) meeting FLUX density criteria.
C   The first pass finds the CC which is just brighter than the user
C   input FLUX.  The second pass finds the CC which is FACTOR times
C   the flux of the first CC.
C   AIPS adverbs:                     Use:
C     INNAME     INNAM(3)     Image name: standard defaults, wildcards
C     INCLASS    INCLS(2)     Image class: ditto
C     INSEQ      XSEQ (INSEQ) Image sequence number: ditto
C     INDISK     XDISK (INDISK)  Image disk number: 0 -> any
C     INVERS     XVER (INVERS)   Extension version number
C     ECOUNT     XECNT (ECOUNT)  Last row number
C     FLUX       XFLUX           Flux density limit to search
C     FACTOR     XFACT (FACTOR)  Fraction of FIRST NEGATIVE
C-----------------------------------------------------------------------
      INTEGER   IRET, IERR, NTHCC1, NTHCC2, RESULT(66)
      REAL      FLUX1, FLUX2, TOTAL1, TOTAL2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'CCFND.INC'
C-----------------------------------------------------------------------
C                                       Get parms, open things
      CALL CCFNIN (IRET)
C                                       if init was ok
      IF (IRET.EQ.0) THEN
         FLUX1 = XFLUX
C                                       Find CC just brighter than FLUX
         CALL CCFNDO (FLUX1, NTHCC1, TOTAL1, IRET)
         END IF
C                                       if not finding same CC
      IF (IRET.EQ.0 .AND. XFACT .NE. 1.0) THEN
C                                       find 2nd CC with fraction of 1st
         FLUX2 = ABS(FLUX1) * XFACT
C                                       Find CC brighter than FLUX2
         CALL CCFNDO (FLUX2, NTHCC2, TOTAL2, IRET)
         END IF
C                                       Close file opened in CCFNIN
      CALL TABIO ('CLOS', 3, 1, RESULT, BUFFER, IERR)
C                                       close down
      CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE CCFNIN (IRET)
C-----------------------------------------------------------------------
C   CCFNIN performs initialization for AIPS task CCFND.  It gets the
C   adverbs, opens the catalog file for 'READ', opens the table
C   extension
C   file, and opens the output device.
C   Output:
C      IRET    I      Error code: 0 => keep going, else quit.
C-----------------------------------------------------------------------
      CHARACTER INTYP*2, STAT*4, PRGN*6
      INTEGER   IRET, I4T, NPARM, IROUND, NREC, TABLUN, IERR,
     *   SCRBUF(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'CCFND.INC'
      DATA TABLUN /27/
      DATA PRGN /'CCFND '/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       get adverbs
      NPARM = 3 + 2 + 1 + 1 + 1 + 1 + 1 + 1
      IRET = 0
      CALL GTPARM (PRGN, NPARM, RQUICK, XINNAM, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRBUF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       find image file
      CALL H2CHR (12, 1, XINNAM, INNAM)
      CALL H2CHR (6, 1, XINCLS, INCLS)
      INSEQ = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      INVERS = IROUND (XVER)
      ECOUNT = IROUND (XECNT)
      IUSER = NLUSER
      IF (XFACT.EQ.0.0) XFACT = 1
      CNO = 1
      INTYP = ' '
      CALL CATDIR ('SRCH', INDISK, CNO, INNAM, INCLS, INSEQ, INTYP,
     *   IUSER, STAT, SCRBUF, IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IERR, INNAM, INCLS, INSEQ, INTYP,
     *      INDISK, IUSER
         GO TO 990
C                                       Get catblk, mark file read
 20   CALL CATIO ('READ', INDISK, CNO, CATBLK, 'READ', SCRBUF, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IERR
         GO TO 990
 30   NCFILE = 1
      FVOL(1) = INDISK
      FCNO(1) = CNO
      FRW(1) = 0
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      CALL TABINI ('READ', 'CC', INDISK, CNO, INVERS, CATBLK, TABLUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, 'CC', INVERS
         GO TO 990
         END IF
C                                       Get Max rows in table
      I4T = BUFFER(5)
C                                       Last is max, if out of limits
      IF ((ECOUNT.LE.0) .OR. (ECOUNT.GT.I4T)) ECOUNT = I4T
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' FINDING INPUT ADVERBS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   ' DISK=',I2,' USER=',I5)
 1020 FORMAT ('ERROR',I5,' READING CATBLK FROM CATALOG FILE')
 1030 FORMAT ('ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1040 FORMAT ('ERROR',I5,' OPENING OUTPUT DEVICE')
      END
      SUBROUTINE CCFNDO (FLUX, FIRSTC, SUMDEL, IRET)
C-----------------------------------------------------------------------
C   CCFNDO reads a cc table and finds the Nth CC brighter than FLUX
C   In/Output:
C      FLUX   R     Flux Limit for Clean Comp./Actual Flux above limit
C   Output:
C      FIRSTC I     Number of the CC which is just above Flux limit
C      SUMDEL R     Sum of Flux of components, up to Flux Limit
C      IRET   I     Error code: 0 => OK or user terminates,
C                      2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   FIRSTC, IRET
      REAL      FLUX, SUMDEL
C
      INTEGER   IRNO, RESULT(66),  RTYPE, IERR, KOLS(7), I, ISCR(256),
     *   LOCS(2), VALUES(2), LRNO
      REAL      RES4(66), LASTFL
      LOGICAL   T
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'CCFND.INC'
      EQUIVALENCE (RESULT, RES4)
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       init counters, line size
      IRET = 0
C                                       Find 'FLUX' column number
      CALL FNDCOL (1, 'FLUX    ', 8, T, BUFFER, KOLS(1), IERR)
C                                       Init CC number and Fluxs
      FIRSTC = 1
      LASTFL = 0.0
      SUMDEL = 0.0
C                                       Flux in the Ith col. of a row.
      I = KOLS(1)
C                                       Read LOOP
C                                       For All Clean component Rows
      LRNO = 0
      DO 140 IRNO = 1,ECOUNT
C                                       Get cc value from the row
         CALL GETCOL (IRNO, I, DATP, LRNO, BUFFER, RTYPE, RESULT, ISCR,
     *      IERR)
C                                       retry on a flagged row
         IF (IERR.LT.0) THEN
            GO TO 140
            END IF
C                                       Exit on Error
         IF (IERR.NE.0) GO TO 960
C                                       Store sum prior to limit
         IF (RES4(1).GT.FLUX) THEN
            FIRSTC = IRNO
C                                       Record next to last flux
            LASTFL = RES4(1)
C                                       Sum flux of components
            SUMDEL = SUMDEL + LASTFL
         ELSE
C                                       Jump out, if less than min
            GO TO 150
            END IF
C
 140     CONTINUE
C                                       Jump here on cc found
 150  CONTINUE
C                                       Tell user
      WRITE (MSGTXT,1150,ERR=999) FIRSTC, LASTFL, FLUX
      CALL MSGWRT(3)
      WRITE (MSGTXT,1160,ERR=999) FIRSTC, SUMDEL
      CALL MSGWRT(3)
C                                       reset input limit to actual
      FLUX = LASTFL
C                                       Add to Map Header
C                                       set Location to first in array
      LOCS(1) = 1
C                                       copy value to buffer
      VALUES(1) = FIRSTC
C                                       Record CC Number in header
      CALL CATKEY ('WRIT', INDISK, CNO, 'NTHCC   ', 1, LOCS,
     *         VALUES, 4, CATBLK, IERR)
C                                       Record CC Flux at Limit
      CALL RCOPY( 1, FLUX, VALUES(1))
C
      CALL CATKEY ('WRIT', INDISK, CNO, 'CCFLUX  ', 1, LOCS,
     *         VALUES, 2, CATBLK, IERR)
C                                       Record Total Flux at Limit
      CALL RCOPY( 1, SUMDEL, VALUES(1))
C
      CALL CATKEY ('WRIT', INDISK, CNO, 'SUMFLUX ', 1, LOCS,
     *         VALUES, 2, CATBLK, IERR)
      GO TO 999
C                                       errors
 960  IRET = 3
      WRITE (MSGTXT,1960) IERR
      GO TO 995
C
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1150 FORMAT ('Component ',I9,' has Flux',F13.6,' > ',F13.6,' Jy.')
 1160 FORMAT ('Sum up to ',I9,' is  Flux',F13.6,' Jy.')
 1960 FORMAT ('ERROR',I5,' READING TABLE DATA')
      END


