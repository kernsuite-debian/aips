      SUBROUTINE CL2FO (DISKIN, CNOIN, CLVER, LUNI, CATIN, DISKOU,
     *   CNOOUT, LUNO, CATBLK, SOUIN, BUFFI, BUFFO, IERR)
C-----------------------------------------------------------------------
C! Copies the CL/FO table DOPOFF column etc to a new FO table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2009
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
C   Copies a CL table to an FO table directly dropping all data columns
C   except the Doppler offset.  Does this only if some of the Doppler
C   offsets are not zero.
C   Inputs:
C      DISKIN   I        Input disk number
C      CNOIN    I        Input catalog number
C      CLVER    I        Input CL table version number
C      LUNI     I        LUN to use for input
C      CATIN    I        File header of input file
C      DISKOU   I        Output disk number
C      CNOOUT   I        Output catalog number
C      LUNO     I        LUN to use for output
C      SOUIN    I        > 0 => restrict to source number SOUIN
C                          write source number out as 1.
C   In/out:
C      CATBLK   I(256)   Output file header
C   Output:
C      BUFFI    I(512)   Input file scratch buffer
C      BUFFO    I(512)   Output file scratch buffer
C      IERR     I        Return error code, 0=>OK, else TABINI or TABIO
C                           error.
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, CLVER, LUNI, CATIN(256), DISKOU, CNOOUT,
     *   LUNO, CATBLK(256), SOUIN, BUFFI(512), BUFFO(512), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXFOC
      PARAMETER (MAXFOC = 7)
      INTEGER   VER, IFORNO, FOKOLS(MAXFOC), FONUMV(MAXFOC), NUMANT,
     *   NUMIF, ICLRNO, CLKOLS(MAXCLC), CLNUMV(MAXCLC), NUMPOL, NTERM,
     *   SOURID, ANTNO, SUBA, FREQID, REFA(2,MAXIF), I, NREC, J, IVER,
     *   MSGSAV
      DOUBLE PRECISION TIME, GEODLY(20)
      REAL      TIMEI, IFR, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2), GMMOD,
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL FNDEXT ('FO', CATBLK, IVER)
      IF (IVER.GT.0) THEN
         IF (SOUIN.GT.0) THEN
C                                       Is need of source selection
C                                       open old FO table
            CALL FOINI ('READ', BUFFI, DISKOU, CNOOUT, IVER, CATBLK,
     *         LUNI, IFORNO, FOKOLS, FONUMV, NUMANT, NUMIF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN OLD FO TABLE'
               GO TO 980
               END IF
C                                       make output FO table
            VER = 0
            CALL FOINI ('WRIT', BUFFO, DISKOU, CNOOUT, VER, CATBLK,
     *         LUNO, ICLRNO, FOKOLS, FONUMV, NUMANT, NUMIF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN NEW FO TABLE'
               GO TO 980
               END IF
            NREC = BUFFI(5)
            DO 10 I = 1,NREC
               CALL TABFO ('READ', BUFFI, IFORNO, FOKOLS, FONUMV, TIME,
     *            TIMEI, SOURID, ANTNO, SUBA, FREQID, DOPOFF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ OLD FO TABLE'
                  GO TO 980
               ELSE IF ((IERR.EQ.0) .AND. ((SOUIN.EQ.SOURID) .OR.
     *            (SOURID.LE.0))) THEN
                  IF (SOURID.GT.0) SOURID = 1
                  CALL TABFO ('WRIT', BUFFO, ICLRNO, FOKOLS, FONUMV,
     *               TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, DOPOFF,
     *               IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1000) IERR, 'WRITE FO TABLE'
                     GO TO 980
                     END IF
                  END IF
 10            CONTINUE
            CALL TABFO ('CLOS', BUFFI, IFORNO, FOKOLS, FONUMV, TIME,
     *         TIMEI, SOURID, ANTNO, SUBA, FREQID, DOPOFF, I)
            CALL TABFO ('CLOS', BUFFO, ICLRNO, FOKOLS, FONUMV, TIME,
     *         TIMEI, SOURID, ANTNO, SUBA, FREQID, DOPOFF, I)
C                                       Delete the original file
            CALL RMEXT (DISKOU, CNOOUT, 'FO', IVER, CATBLK, BUFFI, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'DELETE OLD FO TABLE'
               GO TO 980
               END IF
C                                       Copy new file to place
C                                       occupied by old one
            MSGSAV = MSGSUP
            MSGSUP = 31999
            CALL TABCOP ('FO', VER, IVER, LUNI, LUNO, DISKOU, DISKOU,
     *         CNOOUT, CNOOUT, CATBLK, BUFFI, BUFFO, IERR)
            MSGSUP = MSGSAV
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'COPYING TEMP FO TO OLD'
               GO TO 980
               END IF
C                                       Delete the now defunct
C                                       original output file
            CALL RMEXT (DISKOU, CNOOUT, 'FO', VER, CATBLK, BUFFI, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'DELETE TEMP FO TABLE'
               GO TO 980
               END IF
            END IF
C                                       else get from CL table
      ELSE
         CALL CALINI ('READ', BUFFI, DISKIN, CNOIN, CLVER, CATIN, LUNI,
     *      ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN CL TABLE'
            GO TO 980
            END IF
         NREC = BUFFI(5)
         DO 30 I = 1,NREC
            CALL TABCAL ('READ', BUFFI, ICLRNO, CLKOLS, CLNUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *         GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *         DISP,DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA,
     *         IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ CL TABLE'
               GO TO 980
            ELSE IF ((IERR.EQ.0) .AND. ((SOUIN.LE.0) .OR.
     *         (SOUIN.EQ.SOURID) .OR. (SOURID.LE.0))) THEN
               DO 20 J = 1,NUMIF
                  IF (DOPOFF(J).NE.0.0) GO TO 50
 20               CONTINUE
               END IF
 30         CONTINUE
C                                       no offset found
         CALL TABCAL ('CLOS', BUFFI, ICLRNO, CLKOLS, CLNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *      DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, I)
         GO TO 999
C                                       found some - copy CL
 50      CALL TABCAL ('CLOS', BUFFI, ICLRNO, CLKOLS, CLNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, I)
C                                       open input CL table
         CALL CALINI ('READ', BUFFI, DISKIN, CNOIN, CLVER, CATIN, LUNI,
     *      ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN CL TABLE'
            GO TO 980
            END IF
C                                       make output FO table
         VER = 0
         CALL FOINI ('WRIT', BUFFO, DISKOU, CNOOUT, VER, CATBLK, LUNO,
     *      IFORNO, FOKOLS, FONUMV, NUMANT, NUMIF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN NEW FO TABLE'
            GO TO 980
            END IF
C                                       loop
         DO 60 I = 1,NREC
            CALL TABCAL ('READ', BUFFI, ICLRNO, CLKOLS, CLNUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *         GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *         DISP,DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA,
     *         IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ CL TABLE'
               GO TO 980
            ELSE IF ((IERR.EQ.0) .AND. ((SOUIN.LE.0) .OR.
     *         (SOUIN.EQ.SOURID) .OR. (SOURID.LE.0))) THEN
               IF ((SOUIN.GT.0) .AND. (SOURID.GT.0)) SOURID = 1
               CALL TABFO ('WRIT', BUFFO, IFORNO, FOKOLS, FONUMV, TIME,
     *            TIMEI, SOURID, ANTNO, SUBA, FREQID, DOPOFF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'WRITE FO TABLE'
                  GO TO 980
                  END IF
               END IF
 60         CONTINUE
         CALL TABCAL ('CLOS', BUFFI, ICLRNO, CLKOLS, CLNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF,ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL,CIMAG, DELAY, RATE, WEIGHT, REFA, I)
         CALL TABFO ('CLOS', BUFFO, IFORNO, FOKOLS, FONUMV, TIME, TIMEI,
     *      SOURID, ANTNO, SUBA, FREQID, DOPOFF, I)
         END IF
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CL2FO ERROR',I4,' DOING ',A)
      END
