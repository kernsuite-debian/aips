LOCAL INCLUDE 'SY2TY.INC'
      HOLLERITH XNAMIN(3), XCLSIN(2)
      REAL      DSKIN, SEQIN, XINVER, XOUVER
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, XINVER, XOUVER
      INTEGER   INDISK, INCNO, INVER, OUVER, INSEQ, IRNO, CDVER,
     *   SCRTCH(256), BUFFI(512), BUFFO(512)
      CHARACTER NAMEIN*12, CLASIN*6
      COMMON /PARMS/ INDISK, INCNO, INSEQ, INVER, OUVER, IRNO, CDVER,
     *   SCRTCH, BUFFI, BUFFO
      COMMON /PARMC/ NAMEIN, CLASIN
LOCAL END
      PROGRAM SY2TY
C-----------------------------------------------------------------------
C! Task converts SY calibration table (EVLA) to TY system temperatures
C# EXT-util Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 2011, 2017
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
C   SY2TY will convert an EVLA SY (SysPower) table plus the CalDevice
C   CD table into a Tsys column in a TY table.
C   Inputs:   (from AIPS)
C      INNAME    R(3)   name of primary file.
C      INCLASS   R(2)   class of primary file.
C      INSEQ     R      sequence number of primary file.
C      INDISK    R      disk volume number. 0 means try all.
C      INVERS    R      MF file version number
C      OUTVERS   R      ST file version number
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IRET
C
      INCLUDE 'SY2TY.INC'
      DATA PRGNAM /'SY2TY'/
C-----------------------------------------------------------------------
C                                       start up
      CALL SY2TYI (PRGNAM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       do it
      CALL SY2TYD (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       HIstory file
      CALL SY2TYH
C
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE SY2TYI (PRGNAM, IRET)
C-----------------------------------------------------------------------
C   Inputs
C      PRGNAM   C*6   Task name
C   Outputs:
C      IRET     I     > 0 +> die on error
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*(*)
      INTEGER   IRET
C
      INTEGER   NPARMS, IERR, I
      CHARACTER TYPIN*2, STAT*4
      INCLUDE 'SY2TY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       get parameters, resume aips
      NPARMS = 9
      IRET = 0
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XNAMIN, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRET = 8
         END IF
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 990
      IRET = 8
C                                       interpret parameters
      CALL H2CHR (12, 1, XNAMIN, NAMEIN)
      CALL H2CHR (6, 1, XCLSIN, CLASIN)
      INSEQ = SEQIN + 0.1
      INDISK = DSKIN + 0.1
      INVER = XINVER + 0.1
      OUVER = XOUVER + 0.1
      IRNO = 0
C                                       Get CATBLK from old file.
      INCNO = 1
      TYPIN = 'UV'
      CALL CATDIR ('SRCH', INDISK, INCNO, NAMEIN, CLASIN, INSEQ, TYPIN,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLASIN, INSEQ, INDISK, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', INDISK, INCNO, CATBLK, 'WRIT', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = 1
      FCNO(NCFILE) = INCNO
      FVOL(NCFILE) = INDISK
      FRW(NCFILE) = 1
C                                       version numbers
      CALL FNDEXT ('SY', CATBLK, I)
      IF ((INVER.LE.0) .OR. (INVER.GT.I)) INVER = I
      CALL FNDEXT ('CD', CATBLK, CDVER)
      CALL FNDEXT ('TY', CATBLK, I)
      IF ((OUVER.LE.0) .OR. (OUVER.GT.I)) OUVER = I + 1
      IF ((INVER.LE.0) .OR. (CDVER.LE.0)) THEN
         MSGTXT = 'YOU MUST HAVE AN SY AND CD TABLE TO RUN THIS TASK'
      ELSE
         IRET = 0
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SY2TYI: ERROR',I3,' GETTING ADVERBS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' GETTING CATBLK')
      END
      SUBROUTINE SY2TYD (IRET)
C-----------------------------------------------------------------------
C   SY2TYD copies the SY data to a TY table
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NREC, SUBA, FREQID, LUNI, LUNO, ISYRNO, SYKOLS(12),
     *   SYNUMV(12), NUMANT, NUMPOL, NUMIF, SOURID, ANTNO, LIF, LP,
     *   ITYRNO, TYKOLS(MAXTYC), TYNUMV(MAXTYC), IREC, CALTYP
      REAL      TCAL(4,MAXIF,MAXANT), RIME, TIMEI, PDIFF(2,MAXIF),
     *   PSUM(2,MAXIF), PGAIN(2,MAXIF), TANT(2,MAXIF), TSYS(2,MAXIF),
     *   TC
      DOUBLE PRECISION TIME
      INCLUDE 'SY2TY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI, LUNO /29, 30/
C-----------------------------------------------------------------------
C                                       get Tcal values
      SUBA = 1
      FREQID = 1
      CALL GETCDS (INDISK, INCNO, CDVER, SUBA, FREQID, CATBLK, TCAL,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING THE TCAL VALUES'
         GO TO 990
         END IF
C                                       open SY table to read
      CALL SYINI ('READ', BUFFI, INDISK, INCNO, INVER, CATBLK, LUNI,
     *   ISYRNO, SYKOLS, SYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE SY TABLE FOR READ'
         GO TO 990
         END IF
C                                       open TY table for output
      CALL TYINI ('WRIT', BUFFO, INDISK, INCNO, OUVER, CATBLK, LUNO,
     *   ITYRNO, TYKOLS, TYNUMV, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE TY TABLE FOR WRITE'
         GO TO 990
         END IF
      NREC = BUFFI(5)
      IREC = 2 * MAXIF
      CALL RFILL (IREC, FBLANK, TANT)
      CALL RFILL (IREC, FBLANK, TSYS)
C                                       read/write loop
      DO 50 IREC = 1,NREC
C                                       read
         ISYRNO = IREC
         CALL TABSY ('READ', BUFFI, ISYRNO, SYKOLS, SYNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA, FREQID,
     *      PDIFF, PSUM, PGAIN, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING THE SY TABLE'
            GO TO 980
            END IF
C                                       convert
         DO 30 LIF = 1,NUMIF
            DO 20 LP = 1,NUMPOL
               IF (CALTYP.EQ.1) THEN
                  TC = TCAL(LP+2,LIF,ANTNO)
               ELSE
                  TC = TCAL(LP,LIF,ANTNO)
                  END IF
               IF ((TC.NE.FBLANK) .AND. (TC.GT.0.0) .AND.
     *            (PDIFF(LP,LIF).NE.FBLANK) .AND.
     *            (PSUM(LP,LIF).NE.FBLANK) .AND. (PDIFF(LP,LIF).GT.0.0)
     *            .AND. (PDIFF(LP,LIF).LT.PSUM(LP,LIF))) THEN
                  TSYS(LP,LIF) = TC * PSUM(LP,LIF) / 2.0 / PDIFF(LP,LIF)
               ELSE
                  TSYS(LP,LIF) = FBLANK
                  END IF
               TANT(LP,LIF) = TSYS(LP,LIF)
 20            CONTINUE
 30         CONTINUE
C                                       write
         ITYRNO = IREC
         RIME = TIME
         CALL TABTY ('WRIT', BUFFO, ITYRNO, TYKOLS, TYNUMV, NUMPOL,
     *      NUMIF, RIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, TSYS, TANT,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING THE TY TABLE'
            GO TO 980
            END IF
         IRNO = IRNO + 1
 50      CONTINUE
      CALL TABIO ('CLOS', 0, ITYRNO, BUFFO, BUFFO, IREC)
      CALL TABIO ('CLOS', 0, ISYRNO, BUFFI, BUFFI, IREC)
      WRITE (MSGTXT,1050) IRNO, INVER, OUVER
      CALL MSGWRT (4)
      GO TO 999
C
 980  CALL MSGWRT (8)
      CALL TABIO ('CLOS', 0, ITYRNO, BUFFO, BUFFO, IREC)
      CALL TABIO ('CLOS', 0, ISYRNO, BUFFI, BUFFI, IREC)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SY2TYD: ERROR',I4,1X,A)
 1050 FORMAT ('Wrote',I8,' records from SY version',I4,' to TY version',
     *   I4)
      END
      SUBROUTINE SY2TYH
C-----------------------------------------------------------------------
C   SY2TYD writes some history
C-----------------------------------------------------------------------
C
      INTEGER   HLUN, ID(3), IT(3), IERR
      CHARACTER ATIME*8, ADATE*12, HILINE*72
      INCLUDE 'SY2TY.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      HLUN = 30
      CALL HIINIT (3)
      CALL HIOPEN (HLUN, INDISK, INCNO, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Prepare text and add to file
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (HILINE,1000) TSKNAM, OUVER, IRNO, ADATE, ATIME
      CALL HIADD (HLUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (HILINE,1010) TSKNAM, INVER, CDVER
      CALL HIADD (HLUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
C
 900  CALL HICLOS (HLUN, .TRUE., SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'VERSION=',I3,' LINES=',I8,'  / TY file created ',A,A)
 1010 FORMAT (A6,'SYVERS=',I3,' CDVERS=',I2,'  / SY and CD versions',
     *   ' read')
      END
