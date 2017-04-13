LOCAL INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MXVS
C                                       MXVS = maximum no. correlations
C                                       in a record.
      PARAMETER (MXVS = MAXCIF)
C                                       Local Info for uv util.
      REAL      RP(50), VS(3,MXVS)
      COMMON /UVULCM/ RP, VS
LOCAL END
LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
      PARAMETER (NPARMS=8)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                                       Uses PAOOF.INC
C                      1        2          3         4        5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'APARM',
C                      6        7          8
     *             'DOCRT', 'OUTPRINT', 'BADDISK'/
C                    1       2       3       4       5      6
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOARE, OOARE,
C                    7       8
     *             OOACAR, OOAINT/
C                   1    2    3    4    5      6   7     8
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 10,1, 1,1, 48,1, 10,1 /
LOCAL END
LOCAL INCLUDE 'LINEP.INC'
      CHARACTER TITL1*132, TITL2*132, LINE*132, LPNAME*48,
     *   SCRTCH*132
      REAL      DOCRT
      INTEGER   PAGE, IPCNT, NACROS, LUNP, FINDP, LPBUFF(256)
      COMMON /LINEC/ TITL1, TITL2, LINE, LPNAME, SCRTCH
      COMMON /LINEV/ DOCRT, PAGE, IPCNT, NACROS, LUNP, FINDP,
     *   LPBUFF
LOCAL END
      PROGRAM DTSUM
C-----------------------------------------------------------------------
C! Provides summary of dataset
C# UV Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2004, 2007, 2009, 2012, 2014-2016
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
C   Provides summary of dataset
C   INNAME                             Input UV data (name)
C   INCLASS                            Input UV data (class)
C   INSEQ                              Input UV data (seq. #)
C   INDISK                             Input UV data disk drive #
C   APARM                              (1) Print level
C                                         1=> condensed summary
C                                             for entire run.
C                                         else => full summary
C                                             for each scan.
C   BADDISK                            Disk drive #'s to avoid
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UVIN*36
      INTEGER   IRET, BUFF(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'DTSUM '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL DTSMIN (PRGM, UVIN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Summarize
      CALL DOCHK (UVIN, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL DOSUM (UVIN, IRET)
C                                       Close down files, etc.
 990  IRET = MAX (0, IRET)
      CALL DIE (IRET, BUFF)
C
 999  STOP
      END
      SUBROUTINE DTSMIN (PRGN, UVIN, IRET)
C-----------------------------------------------------------------------
C   DTSMIN gets input parameters for DTSUM and creates the input object
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      UVIN    C*?  Input uv data object
C      IRET    I    Error code: 0 => ok, else failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, UVIN*(*)
C
      INTEGER   NKEY1
C                                       NKEY1=no. adverbs to copy to
C                                       UVIN
      PARAMETER (NKEY1=4)
      REAL APARM(10)
      INTEGER   DIM(7), TYPE, DUMMY, IPRTLV, NCOLPV
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'LINEP.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to uv data
C                    1         2         3         4
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK'/
C                                       Rename to object
C                   1        2        3      4
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INT (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      RQUICK = .FALSE.
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check inputs and set defaults
C                                       Control array
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set default print level
      IF ((APARM(1).LT.1.0).OR.(APARM(1).GT.3.0)) APARM(1) = 1.0
      IF ((APARM(2).LT.2.0).OR.(APARM(2).GT.9.0)) APARM(2) = 4.0
C
      CALL OPUT ('Input', 'APARM', OOARE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
      CALL OGET ('Input', 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
      CALL OGET ('Input', 'OUTPRINT', TYPE, DIM, DUMMY, LPNAME, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create input uv data object
      UVIN = 'Input uv data'
      CALL CREATE (UVIN, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, UVIN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Add print level keyword to
C                                       input UV data object.
      IPRTLV = INT (APARM(1))
      DIM(1) = 1
      DIM(2) = 0
      CALL OPUT (UVIN, 'PRTLEV', OOAINT, DIM, IPRTLV, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NCOLPV = INT (APARM(2)) + 1
      CALL OPUT (UVIN, 'NCOLPV', OOAINT, DIM, NCOLPV, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE DOCHK (UVIN, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Can only process one subarray.
C   This routine uses an I/O buffer to hold the data being averaged.
C   Inputs:
C      UVIN    C*?   Name of input uvdata object.
C   Output:
C       IERR   I     Error code: 0 => ok
C-----------------------------------------------------------------------
C      CHARACTER UVIN*(*)
      CHARACTER UVIN*36
      INTEGER   IERR
C                                     MAXSCN = maximum number of scans
C                                     MAXSOU = maximum number of sources
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSCN, MAXSOU
      PARAMETER (MAXSCN = 10000)
      PARAMETER (MAXSOU = 10000)
C
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CDUMMY*1, SOURCE(MAXSCN)*16, SOUNAM*16, CALCOD*4,
     *   SUNAM(MAXSOU)*16, CCODE(MAXSOU)*4, SCCODE(MAXSCN)*4, VELDEF*8,
     *   VELTYP*8, CTEMP*132, UVTYPE*2
      REAL      TIMES(2,MAXSCN), TIME, DTIME, FLUX(4,MAXIF), TLO, THI
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, EPOCH,
     *   RAAPP, DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC,
     *   INTXC, INTAC, SCAL, RAOBS, DECOBS
      LOGICAL   NXEXIS, SUEXIS, ANEXIS, WASEOF, DOPRT, DOSCAL
      INTEGER   INDS(4,MAXSCN), NSCAN, NXROW, IDSOUR, SUBNX, VSTART,
     *   VEND, FREQID, DARRAY(MAXANT,MAXANT), ANTPRD, SCNCNT, J, TYPE,
     *   DIM(3), COUNT, LREC, NRPARM, NCOR, I, INDXB, LCNT, ANT1, ANT2,
     *   DUMMY, NSOU, QUAL, SUQUAL(MAXSOU), ANTPAS, IQUAL(MAXSCN), NUMB,
     *   INDXIT, NCOLPV, NPASS, NANTPP, IPASS, IANTLO, IANTHI, ANTMAX,
     *   SCNXC, SCNAC, II, TELNUM(MAXANT), LPASS, I1, I2, IPRTLV, NMAXV,
     *   DT13, MSGSAV, INDXT, IARR, IDS(MAXSCN), ANTM, ISAUTO, NDIG,
     *   INDXA1, INDXA2, INDXSA, NCOUNT, TTY(2)
      INTEGER   ANROW, NUMORB, NOPCAL, ANFQID, NANT, ANTNIF
      CHARACTER ANAME*8, TIMSYS*8, RDATE*8, SNAME*8, XYZHAN*8, TFRAME*8
      REAL      POLRXY(2), UT1UTC, DATUTC
      DOUBLE PRECISION  ARRAYC(3), GSTIA0, DEGPDY, SAFREQ
      INTEGER    NOSTA, MNTSTA
      CHARACTER  ANNAME*8, POLTYA*2, POLTYB*2, TELNAM(MAXANT)*8
      REAL       STAXOF, POLAA, POLCA(2*MAXIF), POLAB, POLCB(2*MAXIF),
     *   DIAMAN, FWHMAN(MAXIF)
      DOUBLE PRECISION  STAXYZ(3), ORBPRM(6)
      LOGICAL   DOACS, DOINT
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'LINEP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA DT13 /131072/
C-----------------------------------------------------------------------
      IERR = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
      NCOUNT = 0
C                                       Init printer
      TLO = 1.E10
      THI = 0.0
C                                       Allow reading AC data
      DIM(1) = 1
      DIM(2) = 1
      DOACS = .TRUE.
      CALL SECPUT (UVIN, 'DOACOR', OOALOG, DIM, DOACS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open input
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVIN, 'OBJECT', TYPE, DIM, DUMMY, SNAME, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get print level keyword/value
      CALL OGET (UVIN, 'PRTLEV', TYPE, DIM, IPRTLV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (UVIN, 'NCOLPV', TYPE, DIM, NCOLPV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       first page & page titles
      IF (DOCRT.LE.-2.5) NCOUNT = NCOUNT + 1
C                                       Is there an INDEX table
      CALL UV2TAB (UVIN, 'index table', 'NX', 1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABEXI ('index table', NXEXIS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open it
      IF (NXEXIS) THEN
         CALL ONXINI ('index table', 'READ', DUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TABGET ('index table', 'NROW', TYPE, DIM, NSCAN, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         IF (NSCAN.GT.MAXSCN) THEN
            WRITE (MSGTXT,1000) 'SCANS', MAXSCN, NSCAN
            CALL MSGWRT (6)
            NSCAN = MAXSCN
            END IF
      ELSE
         NSCAN = 1
         IPRTLV = 1
         END IF
C                                       Is there a SOURCE table
      CALL UV2TAB (UVIN, 'source table', 'SU', 1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABEXI ('source table', SUEXIS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open it
      IF (SUEXIS) THEN
         CALL OSUINI ('source table', 'READ', NUMB, VELTYP, VELDEF,
     *      FREQID, DUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TABGET ('source table', 'NROW', TYPE, DIM, NSOU, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         NSOU = 0
         END IF
C                                       Is there an AN table
      CALL UV2TAB (UVIN, 'antenna table', 'AN', 1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABEXI ('antenna table', ANEXIS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open it
      IF (ANEXIS) THEN
         CALL OANINI ('antenna table', 'READ', DUMMY, ARRAYC, GSTIA0,
     *      DEGPDY, SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS,
     *      ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TABGET ('antenna table', 'NROW', TYPE, DIM, NANT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         NANT = 0
         END IF
C                                       Load up source names
      IF (SUEXIS) THEN
         IF (NSOU.GT.MAXSOU) THEN
            WRITE (MSGTXT,1000) 'SOURCES', MAXSOU, NSOU
            CALL MSGWRT (6)
            NSOU = MAXSOU
            END IF
         DO 10 I = 1,NSOU
            NXROW = I
            CALL OTABSU ('source table', 'READ', NXROW, IDSOUR, SOUNAM,
     *         QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH,
     *         RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *         PMDEC, IERR)
            IF (IERR.NE.0) GO TO 990
            SUNAM(IDSOUR) = SOUNAM
            CCODE(IDSOUR) = CALCOD
            SUQUAL(IDSOUR) = QUAL
 10         CONTINUE
         CALL OTABSU ('source table', 'CLOS', NXROW, IDSOUR, SOUNAM,
     *      QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH,
     *      RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC,
     *      IERR)
         END IF
C                                       Load up antenna names
C                                       and print them
      IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
      NCOUNT = NCOUNT + 1
      ANTMAX = 0
      IF (ANEXIS) THEN
         DO 15 I = 1,MAXANT
            TELNAM(I) = ' '
 15         CONTINUE
         DO 20 I = 1,NANT
            ANROW = I
            CALL OTABAN ('antenna table', 'READ', ANROW, ANNAME, STAXYZ,
     *         ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA,
     *         POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (STAXYZ(1).NE.DBLANK) THEN
               TELNAM(NOSTA) = ANNAME
               ANTMAX = MAX (ANTMAX, NOSTA)
            ELSE
               TELNAM(NOSTA) = 'ABSENT'
               END IF
 20         CONTINUE
         CALL OTABAN ('antenna table', 'CLOS', ANROW, ANNAME, STAXYZ,
     *      ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA,
     *      POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) GO TO 990
C
         II = 0
         LINE = ' '
         DO 25 I = 1,MAXANT
            IF ((TELNAM(I).NE.'ABSENT') .AND. (TELNAM(I).NE.' ')) THEN
               II = II + 1
               END IF
            IF (II.EQ.3) THEN
               NCOUNT = NCOUNT + 1
               II = 0
               LINE = ' '
               END IF
 25         CONTINUE
         IF (II.NE.0) THEN
            NCOUNT = NCOUNT + 1
            IF (IERR.NE.0) GO TO 300
            II = 0
            END IF
         END IF
C                                       Get number of visibilities
      CALL UVDGET (UVIN, 'GCOUNT', TYPE, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Fill arrays with necessary
C                                       information
      IF (NXEXIS) THEN
         I2 = 0
         DO 30 I = 1,NSCAN
            NXROW = I
            CALL OTABNX ('index table', 'READ', NXROW, TIME, DTIME,
     *         IDSOUR, SUBNX, VSTART, VEND, FREQID, IERR)
            IF (IERR.NE.0) GO TO 990
            TIMES(1,I) = TIME - DTIME/2.0
            TIMES(2,I) = TIME + DTIME/2.0
C                                       current number of different
C                                       sources in the NX table
            DO 28 I1 = 1,I2
               IF (IDSOUR.EQ.IDS(I1)) GO TO 29
   28          CONTINUE
            I2 = I2 + 1
            IDS(I2) = IDSOUR
   29       CONTINUE
C
            IF ((SUEXIS) .AND. (IDSOUR .GE. 1) .AND. (I2 .LE. NSOU))
     *         THEN
               SOURCE(I) = SUNAM(IDSOUR)
               SCCODE(I) = CCODE(IDSOUR)
               IQUAL(I)  = SUQUAL(IDSOUR)
            ELSE
               SOURCE(I) = SNAME
               SCCODE(I) = ' '
               IQUAL(I)  = 0
               END IF
            INDS(1,I) = VSTART
            INDS(2,I) = VEND
            INDS(3,I) = SUBNX
            INDS(4,I) = FREQID
 30         CONTINUE
         CALL OTABNX ('index table', 'CLOS', NXROW, TIME, DTIME,
     *      IDSOUR, SUBNX, VSTART, VEND, FREQID, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         TIMES(1,1) = -1.0
         TIMES(2,1) = -1.0
         SOURCE(1) = SNAME
         SCCODE(1) = ' '
         IQUAL(1)  = 0
         INDS(1,1) = 1
         INDS(2,1) = COUNT
         INDS(3,1) = 1
         INDS(4,1) = -1
         END IF
C                                       Get info
C                                       LREC
      CALL UVDGET (UVIN, 'LREC', TYPE, DIM, LREC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NRPARM
      CALL UVDGET (UVIN, 'NRPARM', TYPE, DIM, NRPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NCORR
      CALL UVDGET (UVIN, 'NCORR', TYPE, DIM, NCOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       baseline index
      IF (UVTYPE(:1).EQ.'S') THEN
         CALL UVDFND (UVIN, 1, 'BEAM', INDXB, IERR)
      ELSE
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL UVDFND (UVIN, 1, 'BASELINE', INDXB, IERR)
         MSGSUP = MSGSAV
         IF (IERR.NE.0) THEN
            CALL UVDFND (UVIN, 1, 'SUBARRAY', INDXSA, IERR)
            IF (IERR.EQ.0) CALL UVDFND (UVIN, 1, 'ANTENNA1', INDXA1,
     *         IERR)
            IF (IERR.EQ.0) CALL UVDFND (UVIN, 1, 'ANTENNA2', INDXA2,
     *         IERR)
            END IF
         END IF
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER BASELINE OR BEAM'
         GO TO 990
         END IF
C                                       time index
      CALL UVDFND (UVIN, 1, 'TIME1', INDXT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER TIME'
         GO TO 990
         END IF
C                                       integ time index
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL UVDFND (UVIN, 1, 'INTTIM', INDXIT, IERR)
      MSGSUP = MSGSAV
      DOINT = IERR.EQ.0
      IERR = 0
      LPASS = 0
 90   LPASS = LPASS + 1
C                                       Loop through input data
      DO 200 I = 1,NSCAN
C                                       Write scan header for
C                                       full listing.
         IF ((IPRTLV.GE.2) .AND. (LPASS.EQ.2)) THEN
            IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
            NCOUNT = NCOUNT + 4
            END IF
C
         SCNCNT = INDS(2,I) - INDS(1,I) + 1
         ANTPRD = MAXANT * MAXANT
         FRQSEL = INDS(4,I)
C                                       Initialise counts either
C                                       once for a condensed listing
C                                       or else every scan.
         IF (((IPRTLV.GE.2) .AND. (LPASS.EQ.2)) .OR. (I.EQ.1)) THEN
            NMAXV = 0
            CALL FILL (ANTPRD, 0, DARRAY)
            INTXC = 0.0
            INTAC = 0.0
            SCNXC = 0
            SCNAC = 0
            END IF
C                                       Read uv records for this scan
         DO 100 J = 1,SCNCNT
            CALL UVREAD (UVIN, RP, VS, IERR)
            WASEOF = IERR.LT.0
            IF (WASEOF) GO TO 105
            IF (IERR.GT.0) GO TO 990
            TLO = MIN (TLO, RP(INDXT))
            THI = MAX (THI, RP(INDXT))
C                                       Crack baseline
            IF (INDXB.GE.1) THEN
               ANT1 = (RP(INDXB) / 256.0) + 0.001
               ANT2 = (RP(INDXB) - ANT1 * 256) + 0.001
C                                       subarray
               IARR = (RP(INDXB) - ANT1*256.0 - ANT2) * 100.0 + 1.1
            ELSE
               ANT1 = RP(INDXA1) + 0.001
               ANT2 = RP(INDXA2) + 0.001
               IARR = RP(INDXSA) + 0.001
               END IF
            ANTMAX = MAX (ANT1, ANTMAX)
            ANTMAX = MAX (ANT2, ANTMAX)
            IF (DOINT) THEN
               IF (ANT1.NE.ANT2) THEN
                  INTXC = INTXC + RP(INDXIT)
                  SCNXC = SCNXC + 1
                  END IF
               IF (ANT1.EQ.ANT2) THEN
                  INTAC = INTAC + RP(INDXIT)
                  SCNAC = SCNAC + 1
                  END IF
               END IF
            IF (IARR.EQ.INDS(3,I)) DARRAY(ANT1,ANT2) = DARRAY(ANT1,ANT2)
     *         + 1
            NMAXV = MAX (NMAXV, DARRAY(ANT1,ANT2))
 100        CONTINUE
C                                       If condensed listing then
C                                       print only on last scan.
C                                       Header for condensed listing
 105     DOPRT = LPASS.EQ.2
         IF ((LPASS.EQ.1) .AND. ((I.EQ.NSCAN) .OR. (WASEOF))) THEN
            IF (IPRTLV.LE.2) THEN
               DOPRT = .TRUE.
               IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
               NCOUNT = NCOUNT + 1
               IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
               NCOUNT = NCOUNT + 2
               END IF
C                                       find useful antennas
            ANTM = 0
            ISAUTO = 0
            DO 108 I1 = 1,ANTMAX
               DO 107 I2 = 1,ANTMAX
                  IF (I2.GE.I1) THEN
                     IF (DARRAY(I1,I2).GT.0) THEN
                        ANTM = ANTM + 1
                        TELNUM(ANTM) = I1
                        IF (I1.EQ.I2) ISAUTO = 1
                        GO TO 108
                        END IF
                  ELSE
                     IF (DARRAY(I2,I1).GT.0) THEN
                        ANTM = ANTM + 1
                        TELNUM(ANTM) = I1
                        GO TO 108
                        END IF
                     END IF
 107              CONTINUE
 108           CONTINUE
            END IF
C                                       bail if not print
         IF (.NOT.DOPRT) GO TO 200
C                                       Print integration times.
         IF (DOINT) THEN
            NCOUNT = NCOUNT + 1
            IF (SCNXC.GT.0) INTXC = INTXC / SCNXC
            IF (SCNAC.GT.0) INTAC = INTAC / SCNAC
            IF (INTXC.GT.0.0) NCOUNT = NCOUNT + 1
            IF (INTAC.GT.0.0) NCOUNT = NCOUNT + 1
            END IF
         IF (IERR.NE.0) GO TO 300
C                                       Determine number of passes
         NPASS = ((1.0 * NCOLPV * (ANTM-1+ISAUTO)) /
     *      (NACROS - NCOLPV)) + 0.999
         NANTPP = (NACROS - 5) / NCOLPV
         NDIG = 1
         IF (NMAXV.GT.9) NDIG = 2
         IF (NMAXV.GT.99) NDIG = 3
         IF (NMAXV.GT.999) NDIG = 4
         IF (NMAXV.GT.9999) NDIG = 5
         IF (NMAXV.GT.99999) NDIG = 6
         IF (NMAXV.GT.999999) NDIG = 7
         IF (NMAXV.GT.9999999) NDIG = 8
         IF (NMAXV.GT.99999999) NDIG = 9
         IF (NMAXV.GT.999999999) NDIG = 10
         DOSCAL = NCOLPV.LT.NDIG+1
         IF (LPASS.EQ.1) DOSCAL = NCOLPV.LT.NDIG+1
         IF (DOSCAL) THEN
            IF (LPASS.EQ.1) THEN
               NDIG = NDIG+1-NCOLPV
            ELSE
               NDIG = NDIG+1-NCOLPV
               END IF
            NCOUNT = NCOUNT + 1
            SCAL = 1.0D0 / (10.0D0 ** NDIG)
            END IF
C
         DO 150 IPASS = 1,NPASS
            IANTLO = (IPASS - 1) * NANTPP + 2 - ISAUTO
            IANTHI = IANTLO + NANTPP - 1
            IANTHI = MIN (IANTHI, ANTM)
            ANTPAS = IANTHI - IANTLO + 1
            ANTPAS = MIN (ANTPAS, ANTM)
            IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 1
            NCOUNT = NCOUNT + 1
C                                       spacing
            LCNT = NCOLPV*ANTPAS + 6
            NCOUNT = NCOUNT + 1
C
            NMAXV = MAX (1, NMAXV)
            NCOUNT = NCOUNT + ANTM+ISAUTO + 1
 150        CONTINUE
         IF (WASEOF) GO TO 250
 200     CONTINUE
C                                       reopen input
 250  IF ((IPRTLV.GT.1) .AND. (LPASS.EQ.1)) THEN
         CALL OUVCLO (UVIN, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVOPN (UVIN, 'READ', IERR)
         IF (IERR.NE.0) GO TO 990
         GO TO 90
         END IF
C                                       Close files
 300  CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         IF (NCOUNT.GT.1000) THEN
            IERR = -1
            IPCNT = -1
            END IF
      ELSE IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *      .TRUE., IERR)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IERR.GT.0) GO TO 980
         WRITE (SCRTCH,1300) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IERR)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IERR.GT.0) GO TO 980
         SCRTCH = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRTCH, 1, CTEMP, IERR)
         IF (IERR.GT.0) GO TO 980
         IF ((CTEMP(:1).NE.'y') .AND. (CTEMP(:1).NE.'Y')) THEN
            IPCNT = -1
            IERR = -1
            SCRTCH = 'Good choice - save trees'
         ELSE
            SCRTCH = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 999
C
 980  CALL MSGWRT (8)
      GO TO 999
C                                       Error
 990  MSGTXT = 'DOCHK: ERROR CHECKING PRINT COUNT '
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NUMBER OF ',A,' LIMITED TO',I5,' RATHER THAN',I6)
 1300 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE DOSUM (UVIN, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Can only process one subarray.
C   This routine uses an I/O buffer to hold the data being averaged.
C   Inputs:
C      UVIN    C*?   Name of input uvdata object.
C   Output:
C       IERR   I     Error code: 0 => ok
C-----------------------------------------------------------------------
c      CHARACTER UVIN*(*)
      CHARACTER UVIN*36
      INTEGER   IERR
C                                     MAXSCN = maximum number of scans
C                                     MAXSOU = maximum number of sources
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSCN, MAXSOU
      PARAMETER (MAXSCN = 10000)
      PARAMETER (MAXSOU = 10000)
C
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CDUMMY*1, SOURCE(MAXSCN)*16, SOUNAM*16, CALCOD*4,
     *   SUNAM(MAXSOU)*16, CCODE(MAXSOU)*4, SCCODE(MAXSCN)*4, VELDEF*8,
     *   VELTYP*8, NAMEIN*12, CLAIN*6, CTEMP*132, UVTYPE*2
      REAL      TIMES(2,MAXSCN), TIME, DTIME, FLUX(4,MAXIF), TLO, THI
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, EPOCH,
     *   RAAPP, DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC,
     *   INTXC, INTAC, SCAL, RAOBS, DECOBS
      LOGICAL   NXEXIS, SUEXIS, ANEXIS, WASEOF, DOPRT, DOSCAL
      INTEGER   INDS(4,MAXSCN), NSCAN, NXROW, IDSOUR, SUBNX, VSTART,
     *   VEND, FREQID, DARRAY(MAXANT,MAXANT), ANTPRD, SCNCNT, J, JJ,
     *   TYPE, DIM(3), COUNT, LREC, NRPARM, NCOR, I, INDXB, LCNT,
     *   ANT1, ANT2, DUMMY, NSOU, QUAL, SUQUAL(MAXSOU), ANTPAS,
     *   IQUAL(MAXSCN), NUMB, SEQIN, DISKIN, INDXIT, ITIME(8),
     *   NCOLPV, NPASS, NANTPP, IPASS, IANTLO, IANTHI, IANT, JANT,
     *   ANTMAX, SCNXC, SCNAC, II, KK, TELNUM(MAXANT), LPASS, I1, I2,
     *   IPRTLV, NMAXV, NOUT, DT13, NNTXC, NNTAC, MSGSAV, INDXT,
     *   IARR, IDS(MAXSCN), ANTM, ISAUTO, NDIG, INDXA1, INDXA2, INDXSA
      INTEGER   ANROW, NUMORB, NOPCAL, ANFQID, NANT, ANTNIF
      CHARACTER ANAME*8, TIMSYS*8, RDATE*8, SNAME*8, XYZHAN*8, TFRAME*8
      REAL      POLRXY(2), UT1UTC, DATUTC
      DOUBLE PRECISION  ARRAYC(3), GSTIA0, DEGPDY, SAFREQ
      INTEGER    NOSTA, MNTSTA
      CHARACTER  ANNAME*8, POLTYA*2, POLTYB*2, TELNAM(MAXANT)*8
      REAL       STAXOF, POLAA, POLCA(2*MAXIF), POLAB, POLCB(2*MAXIF),
     *   DIAMAN, FWHMAN(MAXIF)
      DOUBLE PRECISION  STAXYZ(3), ORBPRM(6)
      LOGICAL   DOACS, DOINT
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'LINEP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA DT13 /131072/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Init printer
      PAGE = 0
      IPCNT = 980
      TITL1 = ' '
      TITL2 = ' '
      LINE = ' '
      TLO = 1.E10
      THI = 0.0
C                                       Open output device
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, LPBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1080) IERR
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Allow reading AC data
      DIM(1) = 1
      DIM(2) = 1
      DOACS = .TRUE.
      CALL SECPUT (UVIN, 'DOACOR', OOALOG, DIM, DOACS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open input
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVIN, 'OBJECT', TYPE, DIM, DUMMY, SNAME, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get print level keyword/value
      CALL OGET (UVIN, 'PRTLEV', TYPE, DIM, IPRTLV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (UVIN, 'NCOLPV', TYPE, DIM, NCOLPV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Names for labels
C                                       first page & page titles
      CALL OGET (UVIN, 'NAME', TYPE, DIM, DUMMY, NAMEIN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (UVIN, 'CLASS', TYPE, DIM, DUMMY, CLAIN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (UVIN, 'IMSEQ', TYPE, DIM, SEQIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (UVIN, 'DISK', TYPE, DIM, DISKIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (LINE,1070) NAMEIN, CLAIN, SEQIN, DISKIN
      TITL1 = LINE
      IF (DOCRT.LE.-2.5) THEN
         CALL DOLP (IERR)
         IF (IERR.NE.0) GO TO 300
         END IF
C                                       Is there an INDEX table
      CALL UV2TAB (UVIN, 'index table', 'NX', 1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABEXI ('index table', NXEXIS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open it
      IF (NXEXIS) THEN
         CALL ONXINI ('index table', 'READ', DUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TABGET ('index table', 'NROW', TYPE, DIM, NSCAN, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         IF (NSCAN.GT.MAXSCN) THEN
            WRITE (MSGTXT,1000) 'SCANS', MAXSCN, NSCAN
            CALL MSGWRT (6)
            NSCAN = MAXSCN
            END IF
      ELSE
         NSCAN = 1
         IPRTLV = 1
         END IF
C                                       Is there a SOURCE table
      CALL UV2TAB (UVIN, 'source table', 'SU', 1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABEXI ('source table', SUEXIS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open it
      IF (SUEXIS) THEN
         CALL OSUINI ('source table', 'READ', NUMB, VELTYP, VELDEF,
     *      FREQID, DUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TABGET ('source table', 'NROW', TYPE, DIM, NSOU, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         NSOU = 0
         END IF
C                                       Is there an AN table
      CALL UV2TAB (UVIN, 'antenna table', 'AN', 1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABEXI ('antenna table', ANEXIS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open it
      IF (ANEXIS) THEN
         CALL OANINI ('antenna table', 'READ', DUMMY, ARRAYC, GSTIA0,
     *      DEGPDY, SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS,
     *      ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TABGET ('antenna table', 'NROW', TYPE, DIM, NANT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         NANT = 0
         END IF
C                                       Load up source names
      IF (SUEXIS) THEN
         IF (NSOU.GT.MAXSOU) THEN
            WRITE (MSGTXT,1000) 'SOURCES', MAXSOU, NSOU
            CALL MSGWRT (6)
            NSOU = MAXSOU
            END IF
         DO 10 I = 1,NSOU
            NXROW = I
            CALL OTABSU ('source table', 'READ', NXROW, IDSOUR, SOUNAM,
     *         QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH,
     *         RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *         PMDEC, IERR)
            IF (IERR.NE.0) GO TO 990
            SUNAM(IDSOUR) = SOUNAM
            CCODE(IDSOUR) = CALCOD
            SUQUAL(IDSOUR) = QUAL
 10         CONTINUE
         CALL OTABSU ('source table', 'CLOS', NXROW, IDSOUR, SOUNAM,
     *      QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH,
     *      RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC,
     *      IERR)
         END IF
C                                       Load up antenna names
C                                       and print them
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL DOLP (IERR)
         IF (IERR.NE.0) GO TO 300
         END IF
      WRITE (LINE,1160) ANAME
      CALL DOLP (IERR)
      IF (IERR.NE.0) GO TO 300
      ANTMAX = 0
      IF (ANEXIS) THEN
         DO 15 I = 1,MAXANT
            TELNAM(I) = ' '
 15         CONTINUE
         DO 20 I = 1,NANT
            ANROW = I
            CALL OTABAN ('antenna table', 'READ', ANROW, ANNAME, STAXYZ,
     *         ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA,
     *         POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (STAXYZ(1).NE.DBLANK) THEN
               TELNAM(NOSTA) = ANNAME
               ANTMAX = MAX (ANTMAX, NOSTA)
            ELSE
               TELNAM(NOSTA) = 'ABSENT'
               END IF
 20         CONTINUE
         CALL OTABAN ('antenna table', 'CLOS', ANROW, ANNAME, STAXYZ,
     *      ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA,
     *      POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) GO TO 990
C
         II = 0
         LINE = ' '
         DO 25 I = 1,MAXANT
            IF ((TELNAM(I).NE.'ABSENT') .AND. (TELNAM(I).NE.' ')) THEN
               II = II + 1
               IF (II.EQ.1) WRITE (LINE(13:),1180) I, TELNAM(I)
               IF (II.EQ.2) WRITE (LINE(29:),1180) I, TELNAM(I)
               IF (II.EQ.3) WRITE (LINE(45:),1180) I, TELNAM(I)
               END IF
            IF (II.EQ.3) THEN
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 300
               II = 0
               LINE = ' '
               END IF
 25         CONTINUE
         IF (II.NE.0) THEN
            CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 300
            II = 0
            END IF
         END IF
C                                       Get number of visibilities
      CALL UVDGET (UVIN, 'GCOUNT', TYPE, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Fill arrays with necessary
C                                       information
      IF (NXEXIS) THEN
         I2 = 0
         DO 30 I = 1,NSCAN
            NXROW = I
            CALL OTABNX ('index table', 'READ', NXROW, TIME, DTIME,
     *         IDSOUR, SUBNX, VSTART, VEND, FREQID, IERR)
            IF (IERR.NE.0) GO TO 990
            TIMES(1,I) = TIME - DTIME/2.0
            TIMES(2,I) = TIME + DTIME/2.0
C                                       current number of different
C                                       sources in the NX table
            DO 28 I1 = 1,I2
               IF (IDSOUR.EQ.IDS(I1)) GO TO 29
   28          CONTINUE
            I2 = I2 + 1
            IDS(I2) = IDSOUR
   29       CONTINUE
C
            IF ((SUEXIS) .AND. (IDSOUR .GE. 1) .AND. (I2 .LE. NSOU))
     *         THEN
               SOURCE(I) = SUNAM(IDSOUR)
               SCCODE(I) = CCODE(IDSOUR)
               IQUAL(I)  = SUQUAL(IDSOUR)
            ELSE
               SOURCE(I) = SNAME
               SCCODE(I) = ' '
               IQUAL(I)  = 0
               END IF
            INDS(1,I) = VSTART
            INDS(2,I) = VEND
            INDS(3,I) = SUBNX
            INDS(4,I) = FREQID
 30         CONTINUE
         CALL OTABNX ('index table', 'CLOS', NXROW, TIME, DTIME,
     *      IDSOUR, SUBNX, VSTART, VEND, FREQID, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         TIMES(1,1) = -1.0
         TIMES(2,1) = -1.0
         SOURCE(1) = SNAME
         SCCODE(1) = ' '
         IQUAL(1)  = 0
         INDS(1,1) = 1
         INDS(2,1) = COUNT
         INDS(3,1) = 1
         INDS(4,1) = -1
         END IF
C                                       Get info
C                                       LREC
      CALL UVDGET (UVIN, 'LREC', TYPE, DIM, LREC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NRPARM
      CALL UVDGET (UVIN, 'NRPARM', TYPE, DIM, NRPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NCORR
      CALL UVDGET (UVIN, 'NCORR', TYPE, DIM, NCOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       baseline index
      IF (UVTYPE(:1).EQ.'S') THEN
         CALL UVDFND (UVIN, 1, 'BEAM', INDXB, IERR)
      ELSE
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL UVDFND (UVIN, 1, 'BASELINE', INDXB, IERR)
         MSGSUP = MSGSAV
         IF (IERR.NE.0) THEN
            CALL UVDFND (UVIN, 1, 'SUBARRAY', INDXSA, IERR)
            IF (IERR.EQ.0) CALL UVDFND (UVIN, 1, 'ANTENNA1', INDXA1,
     *         IERR)
            IF (IERR.EQ.0) CALL UVDFND (UVIN, 1, 'ANTENNA2', INDXA2,
     *         IERR)
            END IF
         END IF
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER BASELINE OR BEAM'
         GO TO 990
         END IF
C                                       time index
      CALL UVDFND (UVIN, 1, 'TIME1', INDXT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER TIME'
         GO TO 990
         END IF
C                                       integ time index
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL UVDFND (UVIN, 1, 'INTTIM', INDXIT, IERR)
      MSGSUP = MSGSAV
      DOINT = IERR.EQ.0
      IERR = 0
      LPASS = 0
 90   LPASS = LPASS + 1
C                                       Loop through input data
      DO 200 I = 1,NSCAN
C                                       Write scan header for
C                                       full listing.
         IF ((IPRTLV.GE.2) .AND. (LPASS.EQ.2)) THEN
            IF (DOCRT.GT.-2.5) THEN
               LINE = ' '
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 300
               END IF
            WRITE (LINE,1100) SOURCE(I), IQUAL(I), SCCODE(I)
            CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 300
            CALL TODHMS (TIMES(1,I), ITIME(1))
            CALL TODHMS (TIMES(2,I), ITIME(5))
            WRITE (LINE,1130) ITIME
            CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 300
            WRITE (LINE,1110) INDS(1,I), INDS(2,I)
            CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 300
            WRITE (LINE,1120) INDS(3,I), INDS(4,I)
            CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 300
            END IF
C
         SCNCNT = INDS(2,I) - INDS(1,I) + 1
         ANTPRD = MAXANT * MAXANT
         FRQSEL = INDS(4,I)
C                                       Initialise counts either
C                                       once for a condensed listing
C                                       or else every scan.
         IF (((IPRTLV.GE.2) .AND. (LPASS.EQ.2)) .OR. (I.EQ.1)) THEN
            NMAXV = 0
            CALL FILL (ANTPRD, 0, DARRAY)
            INTXC = 0.0
            INTAC = 0.0
            SCNXC = 0
            SCNAC = 0
            END IF
C                                       Read uv records for this scan
         DO 100 J = 1,SCNCNT
            CALL UVREAD (UVIN, RP, VS, IERR)
            WASEOF = IERR.LT.0
            IF (WASEOF) GO TO 105
            IF (IERR.GT.0) GO TO 990
            TLO = MIN (TLO, RP(INDXT))
            THI = MAX (THI, RP(INDXT))
C                                       Crack baseline
            IF (INDXB.GE.1) THEN
               ANT1 = (RP(INDXB) / 256.0) + 0.001
               ANT2 = (RP(INDXB) - ANT1 * 256) + 0.001
C                                       subarray
               IARR = (RP(INDXB) - ANT1*256.0 - ANT2) * 100.0 + 1.1
            ELSE
               ANT1 = RP(INDXA1) + 0.001
               ANT2 = RP(INDXA2) + 0.001
               IARR = RP(INDXSA) + 0.001
               END IF
            ANTMAX = MAX (ANT1, ANTMAX)
            ANTMAX = MAX (ANT2, ANTMAX)
            IF (DOINT) THEN
               IF (ANT1.NE.ANT2) THEN
                  INTXC = INTXC + RP(INDXIT)
                  SCNXC = SCNXC + 1
                  END IF
               IF (ANT1.EQ.ANT2) THEN
                  INTAC = INTAC + RP(INDXIT)
                  SCNAC = SCNAC + 1
                  END IF
               END IF
            IF (IARR.EQ.INDS(3,I)) DARRAY(ANT1,ANT2) = DARRAY(ANT1,ANT2)
     *         + 1
            NMAXV = MAX (NMAXV, DARRAY(ANT1,ANT2))
 100        CONTINUE
C                                       If condensed listing then
C                                       print only on last scan.
C                                       Header for condensed listing
 105     DOPRT = LPASS.EQ.2
         IF ((LPASS.EQ.1) .AND. ((I.EQ.NSCAN) .OR. (WASEOF))) THEN
            IF (IPRTLV.LE.2) THEN
               DOPRT = .TRUE.
               IF (DOCRT.GT.-2.5) THEN
                  LINE = ' '
                  CALL DOLP (IERR)
                  IF (IERR.NE.0) GO TO 300
                  END IF
               LINE = 'SUMMARY LISTING FOR ENTIRE UV-DATA FILE'
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 300
               IF (DOCRT.GT.-2.5) THEN
                  LINE = ' '
                  CALL DOLP (IERR)
                  IF (IERR.NE.0) GO TO 300
                  END IF
               IF (NXEXIS) THEN
                  CALL TODHMS (TIMES(1,1), ITIME(1))
                  CALL TODHMS (TIMES(2,NSCAN), ITIME(5))
               ELSE
                  CALL TODHMS (TLO, ITIME(1))
                  CALL TODHMS (THI, ITIME(5))
                  END IF
               WRITE (LINE,1130) ITIME
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 300
               WRITE (LINE,1110) INDS(1,1), INDS(2,NSCAN)
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 300
               END IF
C                                       find useful antennas
            ANTM = 0
            ISAUTO = 0
            DO 108 I1 = 1,ANTMAX
               DO 107 I2 = 1,ANTMAX
                  IF (I2.GE.I1) THEN
                     IF (DARRAY(I1,I2).GT.0) THEN
                        ANTM = ANTM + 1
                        TELNUM(ANTM) = I1
                        IF (I1.EQ.I2) ISAUTO = 1
                        GO TO 108
                        END IF
                  ELSE
                     IF (DARRAY(I2,I1).GT.0) THEN
                        ANTM = ANTM + 1
                        TELNUM(ANTM) = I1
                        GO TO 108
                        END IF
                     END IF
 107              CONTINUE
 108           CONTINUE
            END IF
C                                       bail if not print
         IF (.NOT.DOPRT) GO TO 200
C                                       Print integration times.
         IF (DOINT) THEN
            WRITE (LINE,1139) INTXC, INTAC
            CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 300
            IF (SCNXC.GT.0) INTXC = INTXC / SCNXC
            IF (SCNAC.GT.0) INTAC = INTAC / SCNAC
            NNTXC = (INTXC * 1.0D6 + 1) / DT13
            NNTAC = (INTAC * 1.0D6 + 1) / DT13
            WRITE (LINE,1140) INTXC, NNTXC
            IF (INTXC.GT.0.0) CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 300
            WRITE (LINE,1145) INTAC, NNTAC
            IF (INTAC.GT.0.0) CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 300
            END IF
C                                       Determine number of passes
         NPASS = ((1.0 * NCOLPV * (ANTM-1+ISAUTO)) /
     *      (NACROS - NCOLPV)) + 0.999
         NANTPP = (NACROS - 5) / NCOLPV
         NDIG = 1
         IF (NMAXV.GT.9) NDIG = 2
         IF (NMAXV.GT.99) NDIG = 3
         IF (NMAXV.GT.999) NDIG = 4
         IF (NMAXV.GT.9999) NDIG = 5
         IF (NMAXV.GT.99999) NDIG = 6
         IF (NMAXV.GT.999999) NDIG = 7
         IF (NMAXV.GT.9999999) NDIG = 8
         IF (NMAXV.GT.99999999) NDIG = 9
         IF (NMAXV.GT.999999999) NDIG = 10
         DOSCAL = NCOLPV.LT.NDIG+1
         IF (LPASS.EQ.1) DOSCAL = NCOLPV.LT.NDIG+1
         IF (DOSCAL) THEN
            IF (LPASS.EQ.1) THEN
C               NDIG = 10 ** (NCOLPV-3)
               NDIG = NDIG+1-NCOLPV
            ELSE
               NDIG = NDIG+1-NCOLPV
C               NDIG = 10 ** (NCOLPV-2)
               END IF
C            WRITE (LINE,1115) NDIG, NMAXV
            WRITE (LINE,1115) 10**NDIG
            CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 300
            SCAL = 1.0D0 / (10.0D0 ** NDIG)
            END IF
C
         DO 150 IPASS = 1,NPASS
            IANTLO = (IPASS - 1) * NANTPP + 2 - ISAUTO
            IANTHI = IANTLO + NANTPP - 1
            IANTHI = MIN (IANTHI, ANTM)
            ANTPAS = IANTHI - IANTLO + 1
            ANTPAS = MIN (ANTPAS, ANTM)
            IF (DOCRT.GT.-2.5) THEN
               LINE = ' '
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 300
               END IF
            LINE = ' ANTS '
            II = IANTLO - 1
            DO 110 J = 1,ANTPAS
               LCNT = NCOLPV*J + 1
               II = II + 1
               IANT = TELNUM(II)
               WRITE (CTEMP,1030) IANT
               LINE(LCNT:) = CTEMP(11-NCOLPV:10)
 110           CONTINUE
            CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 300
C                                       spacing
            LINE = ' '
            WRITE (CTEMP(6:), 1050)
            LCNT = NCOLPV*ANTPAS + 6
            LINE(6:LCNT) = CTEMP(6:)
            CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 300
C
            NMAXV = MAX (1, NMAXV)
            DO 130 J = 1,ANTM-1+ISAUTO
               KK = IANTLO - 1
               IANT = TELNUM(J)
               WRITE (LINE(1:),1060) IANT
               DO 120 JJ = 1, ANTPAS
                  LCNT = NCOLPV*JJ + 1
                  KK = KK + 1
                  JANT = TELNUM(KK)
C                                       Print scaled vis. counts
                  IF (DOSCAL) THEN
                     NOUT = SCAL * DARRAY(IANT,JANT) + 0.5
C                                       Print actual vis. counts
                  ELSE
                     NOUT = DARRAY(IANT,JANT)
                     END IF
                  WRITE (CTEMP,1030) NOUT
                  LINE(LCNT:) = CTEMP(11-NCOLPV:10)
 120              CONTINUE
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 300
 130           CONTINUE
            LINE = ' '
            WRITE (CTEMP(6:),1050)
            LCNT = NCOLPV*ANTPAS + 6
            LINE(6:LCNT) = CTEMP(6:)
            CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 300
 150        CONTINUE
         IF (WASEOF) GO TO 250
 200     CONTINUE
C                                       reopen input
 250  IF ((IPRTLV.GT.1) .AND. (LPASS.EQ.1)) THEN
         CALL OUVCLO (UVIN, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVOPN (UVIN, 'READ', IERR)
         IF (IERR.NE.0) GO TO 990
         GO TO 90
         END IF
C                                       Close files
 300  CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'DOSUM: ERROR SUMMARIZING ' // UVIN
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NUMBER OF ',A,' LIMITED TO',I5,' RATHER THAN',I6)
 1030 FORMAT (I10)
 1050 FORMAT (126('-'))
 1060 FORMAT (I3,' | ')
 1070 FORMAT ('File = ',A12,'.',A6,'.',I4,' Vol =',I2)
 1080 FORMAT ('DOSUM: ERROR ',I3,' OPENING OUTPUT ''PRINT'' DEVICE')
 1100 FORMAT ('Sourcename = ',A16,':',I4,'    Calcode = ',A4)
 1110 FORMAT ('Visibility numbers ',I6,' - ',I6)
C1115 FORMAT ('Count of ',I10,' = ',I10,' actual visibilities')
 1115 FORMAT ('Counts have been divided by ',I10)
 1120 FORMAT ('Subarray = ',I2,'      Freqid = ',I2)
 1130 FORMAT ('Timerange: ',I3,'/',I2.2,2(':',I2.2),
     *   ' - ',I3,'/',I2.2,2(':',I2.2))
 1139 FORMAT ('Total itegration cross-power',F9.1,' total-power',F9.1,
     *   ' seconds')
 1140 FORMAT ('Data integration time (cross-power) = ',F9.6,
     *   ' = ', I4, ' * 0.131072 seconds')
 1145 FORMAT ('Data integration time (total-power) = ',F9.6,
     *   ' = ', I4, ' * 0.131072 seconds')
 1160 FORMAT ('Array name = ''',A8,'''   includes antennas:')
 1180 FORMAT (I2,' (',A8,')')
      END
      SUBROUTINE DOLP (IERR)
C-----------------------------------------------------------------------
C  Write lines to LP file or printer
C-----------------------------------------------------------------------
      INTEGER IERR
C
      INCLUDE 'LINEP.INC'
C-----------------------------------------------------------------------
      CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *   IPCNT, PAGE, SCRTCH, IERR)
C
 999  RETURN
      END
