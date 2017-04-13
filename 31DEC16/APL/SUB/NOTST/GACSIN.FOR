      SUBROUTINE GACSIN (IERR)
C-----------------------------------------------------------------------
C! Initializes CS file, and prepares table to be applied.
C# EXT-appl Calibration Sdish
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2011
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
C   Single dish calibration routine - Initializes CS file, and prepares
C   table to be applied.  If there is no CS file DOCAL is set to .FALSE.
C   Inputs from common /SELCAL/
C      CLUSE    I   CS file version number to initialize.
C   Output:
C      IERR     I   Return code, 0 => ok, otherwise CS table exists but
C                      cannot be read.
C   Output to common /SELCAL/:
C      ICLRNO   I   Current CS record number
C      NCLINR   I   Number of gain records in file.
C      NUMPOL   I   Number of polarizations
C      NUMIF    I   Number of IFs.
C-----------------------------------------------------------------------
      CHARACTER COLHED(13)*24, KEYW(3)*8, TABTYP*2
      INTEGER   IERR, J, NKEY, NREC, NCOL, DATP(128,2), IPOINT,
     *   KOLS(39), KEYTYP(4), KLOCS(4), KEYVAL(6), KEY(2,2), KEYSUB(2,2)
      LOGICAL   T, F
      REAL      FKEY(2,2)
      DOUBLE PRECISION   FREQIF(1)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (FREQIF, UBUFF)
      DATA COLHED /'TIME ', 'RA ', 'DEC ', 'BEAM NO. ', 'SUBARRAY ',
     *   'FACTOR 1 ', 'OFFSET 1 ', 'RAOFF 1 ', 'DECOFF 1 ',
     *   'FACTOR 2 ', 'OFFSET 2 ', 'RAOFF 2 ', 'DECOFF 2 '/
      DATA KEYW /'NO_BEAM ', 'NO_POL  ', 'NO_IF   '/
      DATA T, F /.TRUE.,.FALSE./
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       Initialize cal table pointers
      ICALP1 = -1
      ICALP2 = -1
      IBLP1 = -1
      IBLP2 = -1
C                                       Initialize flag counts
      CNTREC(1,1) = 0
      CNTREC(2,1) = 0
      CNTREC(1,2) = 0
      CNTREC(2,2) = 0
      CNTREC(1,3) = 0
      CNTREC(2,3) = 0
C                                       Initialize time for cal. table.
      LCALTM = -1.0E20
      CALTIM(1) = -1.0E20
      CALTIM(2) = -1.0E20
      CALTIM(3) = -1.0E20
C                                       Set maximum size of
C                                       CURCAL and CALTAB
C                                       (number of gains
C                                        4 words ea.)
      LCUCAL = 4
      LCLTAB = 4
C                                       Fill in polarizarion offset
C                                       pointers.
      POLOFF(1,1) = 0
      POLOFF(2,1) = LCUCAL
      POLOFF(3,1) = 0
      POLOFF(4,1) = LCUCAL
      POLOFF(1,2) = 0
      POLOFF(2,2) = LCUCAL
      POLOFF(3,2) = LCUCAL
      POLOFF(4,2) = 0
C                                       Input in I, Q, U, V =>1 gain
C                                       Is this right?
      IF (ICOR0.GT.0) CALL FILL (8, 0, POLOFF)
C                                       Set bandwidth
      MAXCLR = XCTBSZ
      MAXCLR = MAXCLR / LCUCAL
 100  IF (.NOT.DOCAL) GO TO 999
C                                       Open Calibration table
      TABTYP = 'CS'
      NKEY = 0
      NREC = 0
      NCOL = 0
      ICLRNO = 1
      CALL TABINI ('READ', TABTYP, IUDISK, IUCNO, CLUSE, CATUV, ICLUN,
     *   NKEY, NREC, NCOL, DATP, CLBUFF, IERR)
      IF (IERR.LE.0) GO TO 150
         WRITE (MSGTXT,1100) IERR, TABTYP, CLUSE
         GO TO 990
C                                       Get number of records
 150  NCLINR = CLBUFF(5)
C                                       Check if empty
      IF (NCLINR.LE.0) GO TO 200
C                                       Get column pointers
      NKEY = 13
      CALL FNDCOL (NKEY, COLHED, 24, T, CLBUFF, KOLS, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
      IERR = 0
      CALL FILL (NKEY, 0, CLKOLS)
      CALL FILL (NKEY, 0, CLNUMV)
      DO 160 J = 1,NKEY
         IPOINT = KOLS(J)
         CLKOLS(J) = DATP(IPOINT,1)
         CLNUMV(J) = DATP(IPOINT,2) / 10
 160     CONTINUE
C                                       Table keywords
      NKEY = 3
      CALL TABKEY ('READ', KEYW, NKEY, CLBUFF, KLOCS, KEYVAL, KEYTYP,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Retrieve keyword values
C                                       No. antennas.
      IPOINT = KLOCS(1)
      IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. IFs per pair.
      IPOINT = KLOCS(2)
      IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF pairs.
      IPOINT = KLOCS(3)
      IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       Sort to time-ant if necessary
      IF ((CLBUFF(43).NE.KOLS(1)) .OR. (CLBUFF(44).NE.KOLS(4))) THEN
C                                       Close table
         CALL TABIO ('CLOS', 0, ICLRNO, UBUFF, CLBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         KEY(1,1) = KOLS(1)
         KEY(2,1) = 0
         KEY(1,2) = KOLS(4)
         KEY(2,2) = 0
C                                       Sort
         CALL TABSRT (IUDISK, IUCNO, TABTYP, CLUSE, CLUSE, KEY, KEYSUB,
     *      FKEY, CLBUFF, CATUV, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Reopen table
         CALL TABINI ('READ', TABTYP, IUDISK, IUCNO, CLUSE, CATUV,
     *      ICLUN, NKEY, NREC, NCOL, DATP, CLBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, TABTYP, CLUSE
            GO TO 990
            END IF
         END IF
      GO TO 999
C                                       No Cal file
 200  DOCAL = F
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('GACSIN: ERROR',I3,' OPENING ',A2,' TABLE, VERSION',I5)
      END
