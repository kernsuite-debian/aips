LOCAL INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MXVS
C                                       MXVS = maximum np. correlations
C                                       in a record.
      PARAMETER (MXVS = MAXCIF)
C                                        Local Info for uv util.
      REAL     RP(50), VS(3,MXVS)
      COMMON /UVULCM/ RP, VS
LOCAL END
LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
      PARAMETER (NPARMS=17)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                                       Uses PAOOF.INC
C                      1        2          3         4        5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'APARM',
C            6         7       8       9        10        11
     *   'BADDISK', 'TMRNG', 'BIF', 'BCHAN', 'ECHAN', 'ANTENNAS',
C             12         13        14        15        16
     *    'BASELINE', 'SOURCE', 'FREQID', 'DOCRT', 'OUTPRINT',
C             17
     *    'SUBARRAY'/
C                    1       2       3       4       5       6
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOARE, OOAINT,
C           7       8      9       10      11      12      13
     *    OOARE, OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOACAR,
C           14      15     16      17
     *    OOAINT, OOARE, OOACAR, OOAINT/
C                   1    2    3    4    5     6     7    8    9
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 10,1, 10,1, 8,1, 1,1, 1,1,
C         10    11    12    13    14   15    16   17
     *    1,1, 50,1, 50,1, 16,30, 1,1, 1,1, 48,1, 1,1/
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
LOCAL INCLUDE 'SUB.INC'
      INTEGER   SUBARY, NA
      COMMON /SUBB/ SUBARY, NA
LOCAL END
      PROGRAM COHER
C-----------------------------------------------------------------------
C! Baseline dependent time averaging of uv data to find coherence time
C# UV Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2004, 2007, 2012, 2015
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
C   Baseline dependent time averaging of uv data
C   INNAME                             Input UV data (name)
C   INCLASS                            Input UV data (class)
C   INSEQ                              Input UV data (seq. #)
C   INDISK                             Input UV data disk drive #
C   APARM                              (1) max. time (sec)
C                                      (2) min. SNR acceptable
C                                      (3) Vector/Scalar Limit
C   BADDISK                            Disk drive #'s to avoid
C   TIMERANG                           Timerange: daystart, hourstart,
C                                      minstart, secstart, daystop,
C                                      hourstop, minstop, secstop.
C   BIF                                IF selected
C   BCHAN                              Beginning channel
C   ECHAN                              End channel
C   ANTENNAS                           Antennas selected
C   BASELINE                           Baseline with the antennas
C   SOURCE                             Sources selected
C   FREQID                             Freq. ID to select. 0 =. 1
C   DOCRT                              > 0 => use CRT, else line printer
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UVIN*36
      INTEGER  IRET, BUFF(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'COHER '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL COHEIN (PRGM, UVIN, IRET)
C                                       Average
      IF (IRET.EQ.0) CALL UVCOHE (UVIN, IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF)
C
 999  STOP
      END
      SUBROUTINE COHEIN (PRGN, UVIN, IRET)
C-----------------------------------------------------------------------
C   COHEIN gets input parameters for COHER and creates the input and
C   output objects
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
C                                       Data to read AN table
      INCLUDE 'SUB.INC'
      INTEGER   NKEYS
      PARAMETER (NKEYS = 4)
      CHARACTER INKEY(NKEYS)*8, OUTKEY(NKEYS)*32
      INTEGER I
      CHARACTER TABLE*5
      PARAMETER (TABLE = 'table')
C
C                                       NKEY1=no. adverbs to copy to
C                                       UVIN
      PARAMETER (NKEY1=4)
      INTEGER   IERR, DIM(7), TYPE, BIF, EIF, BCHAN, ECHAN,
     *   ANTEN(50), BASL(50), FREQID, DUMMY, DELAYS
      REAL      APARM(10), TMRNG(8), TSTAR, TSTOP
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, CDUMMY*1,
     *   SOURCE(30)*16
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'LINEP.INC'
      INCLUDE 'INPUTDATA.INC'

C                                       Rename to object
C                   1        2       3      4
      DATA OUTK1 /'NAME', 'CLASS', 'SEQ', 'DISK'/
C                                       AN table
      DATA INKEY  /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  '/
      DATA OUTKEY /'NAME    ', 'CLASS   ', 'IMSEQ   ', 'DISK    '/
C-----------------------------------------------------------------------
C                                       Copy Name adverbs
      CALL CCOPY ( NKEY1, AVNAME, INK1)
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check inputs and set defaults
C                                       Control array
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Max coherence time
      IF (APARM(8).LE.0.0001) APARM(8) = 5
C                                       Max analysis time
      IF (APARM(1).LE.0.0001) APARM(1) = 10 * APARM(8)
C                                       Min SNR default to reject
      IF (APARM(2).LE.0.0001) APARM(2) = 5.0
C                                       Vector/Scalar Cutoff
      IF (APARM(3).LE.0.0001) APARM(3) = .8
C                                       Step in time;
      IF (APARM(5).LE.0.0001) APARM(5) = 1.0E20
C                                       force aparm(5)=aparm(1)
C                                       if aparm(7)>0 (BLING)
      IF (APARM(7) .GT. 0.1) APARM(5) = APARM(1)
C                                       search delay
      DELAYS = APARM(6) + 0.01
C
      CALL OPUT ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Timerang
      CALL OGET ('Input', 'TMRNG', TYPE, DIM, TMRNG, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      TSTAR = TMRNG(1) + TMRNG(2)/24.0 + TMRNG(3)/(24.0*60.0) +
     *   TMRNG(4)/(24.0*60.0*60.0)
      TSTOP = TMRNG(5) + TMRNG(6)/24.0 + TMRNG(7)/(24.0*60.0) +
     *   TMRNG(8)/(24.0*60.0*60.0)
      IF (TSTOP .LE. TSTAR) TSTOP = 1.0E10
C                                       BIF
      CALL OGET ('Input', 'BIF', TYPE, DIM, BIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (BIF .EQ. 0) BIF = 1
      EIF = BIF
C                                       Docrt
      CALL OGET ('Input', 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BCHAN
      CALL OGET ('Input', 'BCHAN', TYPE, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (BCHAN .EQ. 0) BCHAN = 1
C                                       ECHAB
      CALL OGET ('Input', 'ECHAN', TYPE, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Antennas
      CALL OGET ('Input', 'ANTENNAS', TYPE, DIM, ANTEN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Baseline
      CALL OGET ('Input', 'BASELINE', TYPE, DIM, BASL, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Source
      CALL OGET ('Input', 'SOURCE', TYPE, DIM, DUMMY, SOURCE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Freqid
      CALL OGET ('Input', 'FREQID', TYPE, DIM, FREQID, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (FREQID .EQ. 0) FREQID = 1
C                                       Outprint
      CALL OGET ('Input', 'OUTPRINT', TYPE, DIM, DUMMY, LPNAME, IRET)
      IF (IRET.NE.0) GO TO 999

      CALL OGET ('Input', 'SUBARRAY', TYPE, DIM, SUBARY, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Fill in default subarray value
C                                        if necessary:
      IF (SUBARY.LE.0) SUBARY = 1
C                                        Find the number of antennae
C                                        in the subarray: NA
      CALL TABCRE (TABLE, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IN2OBJ ('Input', NKEYS, INKEY, OUTKEY, TABLE, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 2
      DIM(2) = 1
      CALL TABPUT (TABLE, 'TBLTYPE', OOACAR, DIM, I, 'AN', IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 1
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, 0, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL ANTNO (TABLE, SUBARY, NA, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL TABDES (TABLE, IRET)
      IF (IRET.NE.0) GO TO 999
C
C                                       Create input uv data object
      UVIN = 'Input uv data'
      CALL CREATE (UVIN, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, UVIN, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 2
C                                       Max time of analysis
      CALL OPUT (UVIN, 'MAXATIME', OOARE, DIM, APARM(1), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Max time interval for a source
      CALL OPUT (UVIN, 'TIMESTEP', OOARE, DIM, APARM(5), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Minimum SNR to pass
      CALL OPUT (UVIN, 'MINSNR', OOARE, DIM, APARM(2), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       vector/scalar
      CALL OPUT (UVIN, 'VECSCL', OOARE, DIM, APARM(3), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Print level
      CALL OPUT (UVIN, 'PRTLEV', OOARE, DIM, APARM(4), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Output type
      CALL OPUT (UVIN, 'OUTTYP', OOARE, DIM, APARM(7), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Max coherence time
      CALL OPUT (UVIN, 'MAXCOH', OOARE, DIM, APARM(8), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Delay search
      CALL OPUT (UVIN, 'DELAYS', OOAINT, DIM, DELAYS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       tstar
      CALL OPUT (UVIN, 'TSTAR', OOARE, DIM, TSTAR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       tstop
      CALL OPUT (UVIN, 'TSTOP', OOARE, DIM, TSTOP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       bif
      CALL OPUT (UVIN, 'BIF', OOAINT, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       eif
      CALL OPUT (UVIN, 'EIF', OOAINT, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       bchan
      CALL OPUT (UVIN, 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       echan
      CALL OPUT (UVIN, 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Freqid
      CALL OPUT (UVIN, 'FREQID', OOAINT, DIM, FREQID, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       antennas
      DIM(1) = 20
      CALL OPUT (UVIN, 'ANTEN', OOAINT, DIM, ANTEN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       baseline
      CALL OPUT (UVIN, 'BASL', OOAINT, DIM, BASL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       source
      DIM(1) = 16
      DIM(2) = 30
      CALL OPUT (UVIN, 'SOURCE', OOACAR, DIM, DUMMY, SOURCE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       outprint
      DIM(1) = 48
      DIM(2) = 1
      CALL OPUT (UVIN, 'OUTPRINT', OOACAR, DIM, DUMMY, LPNAME, IERR)
      IF (IERR .NE. 0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE UVCOHE (UVIN, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Baseline dependent time averaging of a uv data set.
C   Inputs:
C      UVIN    C*?   Name of input uvdata object.
C   Inputs from UVIN Object.
C      MAXATIME R    Maximum time interval for a source
C      MINSNR   I    Minimum SNR to pass
C   Output:
C       IERR    I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), INNAME*12, CLAI*6
      INTEGER   IERR
      INTEGER MXINTR
C                                       Max number of coherent intervals
      PARAMETER (MXINTR = 200)
      REAL COHERT(MXINTR), TSTA(MXINTR), TEND(MXINTR), COH, COHMEA,
     *   TLE, TRI, DCOH
      INTEGER   IAVERA, SUBA, CAT(256)
C
      INCLUDE 'UVSTUFF.INC'
C                                       Small Vis Amplitude
      REAL    EPSILN
C                                       Max time bins, Uv storage buff
      INTEGER   MXTIME, MXBUFF, MXANT, MXBAS, MXSOU, MXRP
      PARAMETER (EPSILN=1.0E-10, MXTIME=2000, MXANT=30,
     *   MXBAS=(MXANT*(MXANT-1))/2, MXBUFF=MXTIME*3*MXBAS, MXSOU=30,
     *   MXRP=MXTIME*MXBAS)
      CHARACTER SRTORD*2, CDUMMY*1, SOLIST(MXSOU)*16, SELSO*16
      INTEGER   TYPE, DIM(3), COUNT, LREC, NRPARM, ILOCU, ILOCV,
     *   ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ, ILOCA1, ILOCA2, ILOCSA,
     *   JLOCC, JLOCS, JLOCF, JLOCIF, JLOCR, JLOCD, INCS, INCF, INCIF,
     *   ANTCNT(MXANT), NTIME(MXBAS),  I, ANTONE, ANTTWO, PRTCNT, ANT1,
     *   ANT2, NANT, DUMMY, LOOP, FREQID, NAVERA, ANTOT(MXANT),
     *   ANTIN(MXANT), ANTOUT(MXANT)
      INTEGER   BIF, BCHAN, ECHAN, NCHAN, INDEX,
     *   ANTEN(MXANT), BASL(MXANT), ANTS1(MXBAS), ANTS2(MXBAS), MXAN,
     *   NBASE, NANTS, ANTNS(MXANT), MXSOUR, SID, NSOU, SOUID(MXSOU),
     *   QUAL(MXSOU), BUFFER(512), DISK, CNO, BASCNT(MXBAS), IBASE,
     *   NTIMEM, IND, IND1, NTIM, IDSAV, BASTOT(MXBAS), DL, HL, ML,
     *   DR, HR, MR, NNSOU, DELAYS
      CHARACTER SOURCE(MXSOU)*16
      CHARACTER OBSDAT*8
      INTEGER   OBSDMY(3), OBSDNO, CAL(12), SECLIN, SECRIN
      REAL   MXATIM, MINSNR, TIME, COTIME, COTIM2, MINTIM, TIMEST,
     *   VECSCL, PRTLEV, ANTBIN(MXANT), VISBUF(MXBUFF), TIMBIN(MXRP),
     *   ANTB(MXANT), OUTTYP, MAXCOH, TFACT
      REAL      VIS(3,MXBUFF), TIME0(MXBAS), DTIME, TSTAR, TSTOP,
     *   AMPRMS, BASTIM(MXBAS), COHANT, AMPSCL, TLEFT, TRIGHT, SECR,
     *   SECL
      LOGICAL   NEXT, DOSU, DOFQ, EOF, START
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'LINEP.INC'
      INCLUDE 'SUB.INC'
      DATA CAL /0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Init printer
      PAGE = 0
      IPCNT = 980
      TITL1 = ' '
      TITL2 = ' '
      LINE = ' '
C                                       Open output device
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, LPBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1080) IERR
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       docrt
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVIN, 'DOCRT', OOARE, DIM, DOCRT, CDUMMY, IERR)
      IF (IERR .NE. 0) GO TO 999
C
      MXAN = MXANT
C                                       take maximum number of antennas
C                                       from AN table
      MXAN = NA
C
      MXSOUR = MXSOU
      NTIMEM = MXTIME
C                                       NTIME is a number of
C                                       visibilities found for the given
C                                       baseline for the current source
      DO 40 LOOP = 1, MXBAS
         NTIME(LOOP) = 0
   40    CONTINUE
      PRTCNT = 1
C                                       Request Stokes I
      DIM(1)=4
      DIM(2)=1
      DIM(3)=0
      CALL SECPUT (UVIN, 'STOKES   ', OOACAR, DIM, DUMMY, 'I   ', IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'SECPUT ERROR SELECTING STOKES = I'
         CALL MSGWRT (5)
         GO TO 990
         END IF
C                                       Open input
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check that sort = B*
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SRTORD, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SRTORD.NE.'BT' .AND. SRTORD.NE.'TB') THEN
         MSGTXT = 'UVCOHE: DATA NOT IN BT,TB ORDER, USE UVSRT'
         IERR = 5
         GO TO 995
         END IF
C                                       Zero number of visibilities
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
C                                       Maximum time for coherent average
      CALL OUVGET (UVIN, 'MAXATIME', TYPE, DIM, MXATIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Maximum time for coherent average
      CALL OUVGET (UVIN, 'TIMESTEP', TYPE, DIM, TIMEST, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get SNR level to flag
      CALL OUVGET (UVIN, 'MINSNR', TYPE, DIM, MINSNR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get Vector/Scalar ratio limit
      CALL OUVGET (UVIN, 'VECSCL', TYPE, DIM, VECSCL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Determine amount to print
      CALL OUVGET (UVIN, 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Determine type of print
      CALL OUVGET (UVIN, 'OUTTYP', TYPE, DIM, OUTTYP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Determine max coherence time
      CALL OUVGET (UVIN, 'MAXCOH', TYPE, DIM, MAXCOH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Determine if search delay
      CALL OUVGET (UVIN, 'DELAYS', TYPE, DIM, DELAYS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Determine TSTAR
      CALL OUVGET (UVIN, 'TSTAR', TYPE, DIM, TSTAR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Determine TSTOP
      CALL OUVGET (UVIN, 'TSTOP', TYPE, DIM, TSTOP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Determine bif
      CALL OUVGET (UVIN, 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)

      IF (IERR.NE.0) GO TO 990
C                                       Determine bchan
      CALL OUVGET (UVIN, 'BCHAN', TYPE, DIM, BCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Determine echan
      CALL OUVGET (UVIN, 'ECHAN', TYPE, DIM, ECHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      NCHAN = ECHAN - BCHAN + 1
C                                       Determine FREQID
      CALL OUVGET (UVIN, 'FREQID', TYPE, DIM, FREQID, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      DIM(1) = 20
C                                       Determine antennas
      CALL OUVGET (UVIN, 'ANTEN', TYPE, DIM, ANTEN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Determine baseline
      CALL OUVGET (UVIN, 'BASL', TYPE, DIM, BASL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Determine sources
      CALL OUVGET (UVIN, 'SOURCE', TYPE, DIM, DUMMY, SOURCE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       docrt
      CALL OUVGET (UVIN, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       outprint
      CALL OUVGET (UVIN, 'OUTPRINT', TYPE, DIM, DUMMY, LPNAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Determine inname
      CALL FNAGET (UVIN, 'NAME', TYPE, DIM, DUMMY, INNAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Determine class
      CALL FNAGET (UVIN, 'CLASS', TYPE, DIM, DUMMY, CLAI, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Determine Disk
      CALL FNAGET (UVIN, 'DISK', TYPE, DIM, DISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Determine Cat. number
      CALL FNAGET (UVIN, 'CNO', TYPE, DIM, CNO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       find the first and second
C                                       antennas at selected baselines
      CALL ONETWO (ANTEN, BASL, MXAN, NBASE, ANTS1, ANTS2,
     *   ANTNS, NANTS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Tell User
      WRITE (MSGTXT,1105) MINSNR
      CALL MSGWRT (3)
      WRITE (MSGTXT,1110) VECSCL
      CALL MSGWRT (3)
C
      COUNT = 0
C                                       Zero number of visibilities
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
C                                       Convert time (minutes) to days
      MXATIM = MXATIM / (24.*60.)
      TIMEST = TIMEST / (24.*60.)
      MAXCOH = MAXCOH / (24.*60.)
      TFACT = MXATIM / MAXCOH
C                                       Get Uv data pointers
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 990
      INDEX = 1
C                                       Init antenna count bins
      CALL FILL (MXANT, 0, ANTOT)
      CALL FILL (MXANT, 0, ANTOUT)
      CALL FILL (MXANT, 0, ANTB)
C                                       Init baselines count bins
      CALL RFILL (MXBAS, 0.0, BASTIM)
      CALL FILL (MXBAS, 0, BASCNT)
      CALL FILL (MXBAS, 0, BASTOT)
C                                       Which random parameters to check
      DOSU = ILOCSU.GE.1
      DOFQ = ILOCFQ.GE.1
C                                       find ID list for selected
C                                       sources
      IF (DOSU) THEN
         NSOU = MXSOUR
         DO 80 LOOP = 1, NSOU
            QUAL(LOOP) = -1
   80       CONTINUE
         CALL SOURNU (SOURCE, QUAL, MXSOUR, DISK, CNO, NSOU, NNSOU,
     *      BUFFER, SOUID, SOLIST, IERR)
      ELSE
         NSOU = 0
         END IF
C                                       Get catalog header.
      CALL CATIO ('READ', DISK, CNO, CAT, 'REST', BUFFER, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 995
         END IF
      SUBA = 1
C                                       read the antenna table to get
C                                       the list of antennas names
      CALL GETANT (DISK, CNO, SUBA, CAT, BUFFER, IERR)
C                                       Find the annual day number
C                                       corresponding to the reference
C                                       date for the observations.
C
      CALL UVDGET (UVIN, 'DATE-OBS', TYPE, DIM, DUMMY, OBSDAT, IERR)
      IF (IERR .NE. 0) GO TO 999
      CALL DATEST (OBSDAT, OBSDMY)
      OBSDNO = CAL(OBSDMY(2)) + OBSDMY(3)
      IF ((MOD(OBSDMY(1),4).EQ.0) .AND. (OBSDMY(2).GT.2))
     *   OBSDNO = OBSDNO + 1
C                                       LREC
      CALL UVDGET (UVIN, 'LREC', TYPE, DIM, LREC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NRPARM
      CALL UVDGET (UVIN, 'NRPARM', TYPE, DIM, NRPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check that it fits
      IF (NCHAN.GT.MXVS) THEN
         WRITE (MSGTXT,1000) NCHAN, MXVS
         IERR = 5
         GO TO 995
         END IF
C                                       take a negative IDSAV to be sure
C                                       it is different from any ID
      IDSAV = -2
      START = .TRUE.
C                                       Loop through input data
 100     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.GT.0) GO TO 990
         TIME = RP(ILOCT)
C                                       Note end of File
         EOF = (IERR .LT. 0 .OR. TIME .GT. TSTOP)
         IF (TIME .LT. TSTAR) GO TO 100
C                                       find the first time
         IF (START) THEN
            TLEFT = TIME
            TRIGHT = TLEFT + TIMEST
            IF (TRIGHT .GE. TSTOP) TRIGHT = TSTOP
            START = .FALSE.
            END IF

C                                       Freqid selection
         IF ((DOFQ .AND. (RP(ILOCFQ) .NE. FREQID)) .AND. .NOT.EOF)
     *      GO TO 100
C                                       Antennas (baselines) selection
C                                       Decode Antenna numbers
         IF (ILOCB.GE.1) THEN
            ANT1 = ANTONE(RP(ILOCB))
            ANT2 = ANTTWO(RP(ILOCB))
         ELSE
            ANT1 = RP(ILOCA1)  + 0.1
            ANT2 = RP(ILOCA2)  + 0.1
            END IF
         DO 120 LOOP = 1, NBASE
            IF ((ANT1 .EQ. ANTS1(LOOP) .AND. ANT2.EQ.ANTS2(LOOP))
     *          .OR. EOF) GO TO 130
  120    CONTINUE
C                                       Not wanted
         GO TO 100
  130    CONTINUE
         IBASE = LOOP
C                                       at least one baseline presented
C                                       at the current source
         NEXT = .FALSE.
         DO 160 LOOP = 1, NBASE
            NEXT = NEXT .OR. (NTIME(LOOP) .GT. 0)
  160       CONTINUE
C                                       New source
         IND = NTIMEM*(IBASE-1) + 1
         NEXT = NEXT .AND. (DOSU .AND. (RP(ILOCSU).NE.IDSAV))
C                                       New time interval
         NEXT = NEXT .OR. TIME .GT. TRIGHT
C                                       If new or end of file and data

         IF (NEXT .OR. EOF) THEN
            CALL DHMS(TLEFT, DL, HL, ML, SECL)
            CALL DHMS(TLEFT + MXATIM, DR, HR, MR, SECR)
            IF (OUTTYP .LT. 0.9) THEN
C                                       timerange at the title
               WRITE (LINE,1310) DL, HL, ML, SECL, DR, HR, MR, SECR
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 500
C                                       source at the title
               IF (NSOU.GT.0) THEN
                  WRITE (LINE,1320) SELSO
                  CALL DOLP (IERR)
                  IF (IERR.NE.0) GO TO 500
                  END IF
               WRITE (LINE,1402)
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 500
               LINE = 'I  BASELINE  I Number of averaging I ' //
     *            'Coherence time, min I'
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 500
               WRITE (LINE,1402)
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 500
               END IF
C                                       Init antenna count bins
            CALL RFILL (MXANT, 0.0, ANTBIN)
            CALL FILL (MXANT, 0, ANTCNT)
            CALL FILL (MXANT, 0, ANTIN)
C                                       We reach a following source,
C                                       So finalize the previous one
            DO 180 LOOP = 1, NBASE
C                                       Do not do anything if the
C                                       baseline absent at previous
C                                       source
               IF (NTIME(LOOP) .EQ. 0  .OR.
     *            NTIME(LOOP) .GE. NTIMEM) GO TO 180
C                                       Decode Antenna numbers
               ANT1 = ANTS1(LOOP)
               ANT2 = ANTS2(LOOP)
C                                       Get average, RMS Vis
               CALL RMS (1, NTIME(LOOP), LOOP, NTIMEM, VISBUF,
     *            AMPSCL, AMPRMS)
               CALL VFLAG (1, 1, NTIME(LOOP), LOOP, NTIMEM,
     *            MINSNR, AMPSCL, AMPRMS, VISBUF, DELAYS)
C                                       Get cooherence time
               CALL VCOHER (1, NTIME(LOOP), TFACT, LOOP, VECSCL,
     *            NTIMEM, TIMBIN, VISBUF, COTIME, NAVERA,
     *            COHERT, TSTA, TEND, IERR)
C               IF (IERR .EQ. -1) GO TO 180
               IF (IERR .NE. 0) GO TO 180
C                                       Fix No Coherence time case
               IF (COTIME.GT.0.0) THEN
                  COTIM2 = COTIME
               ELSE
C                                       Else use integration as coher.

                  IND = NTIMEM*(LOOP-1) + MIN(2,NTIME(LOOP))
                  IND1 = NTIMEM*(LOOP-1) + 1
                  COTIM2 = TIMBIN(IND) - TIMBIN(IND1)
                  END IF
C                                       Report baselines coherence times
               IF (OUTTYP .GT. 0.01) THEN
                  WRITE (LINE,1406) STNNAM(ANT1)(1:2), STNNAM(ANT2)(1:2)
C                                       fix IPCNT=50 to prevent print
C                                       the pages number
                  IF (LPNAME .NE. ' ') IPCNT = 50
                  CALL DOLP (IERR)
                  IF (IERR.NE.0) GO TO 500
                  COHMEA = COHERT(1)
                  TLE = TSTA(1)
                  DO 165 IAVERA = 1, NAVERA
                     IF (IAVERA .LT. NAVERA .AND. COHMEA .GT. 0) THEN
                        DCOH = ABS(COHMEA - COHERT(IAVERA + 1)) / COHMEA
                     ELSE
                        DCOH = 1
                        END IF
                     IF (DCOH .LT. 0.2) THEN
C                                       the current interval is close
C                                       to the mean of previous ones
                        COHMEA = (COHMEA + COHERT(IAVERA + 1)) / 2
                        GO TO 165
                     ELSE
                        TRI = TEND(IAVERA)
                        CALL DHMS(TLE, DL, HL, ML, SECL)
                        CALL DHMS(TRI, DR, HR, MR, SECR)
                        COH = 24.0 * 60.0 * COHMEA
                        SECLIN = SECL + 0.5
                        SECRIN = SECR + 0.5
                        WRITE (LINE,1408)
     *                     DL + OBSDNO, HL, ML, SECLIN,
     *                     DR + OBSDNO, HR, MR, SECRIN, COH
C                                       fix IPCNT=50 to prevent print
C                                       the pages number
                        IF (LPNAME .NE. ' ') IPCNT = 50
                        CALL DOLP (IERR)
                        IF (IERR.NE.0) GO TO 500
                        IF (IAVERA .LT. NAVERA) THEN
                           TLE = TSTA(IAVERA + 1)
                           COHMEA = COHERT(IAVERA + 1)
                           END IF
                        END IF
  165                CONTINUE
               ELSE

                  WRITE (LINE,1410) ANT1, ANT2, NAVERA,
     *               24.0 * 60.0 * COTIM2
                  CALL DOLP (IERR)
                  IF (IERR.NE.0) GO TO 500
C                                       Debug
                  IF (PRTLEV .GE. PRTCNT) THEN
                     WRITE (MSGTXT,1300) ANT1, ANT2, NTIME(LOOP), COTIME
     *                  *24*60,COTIM2*24.*60.
                     CALL MSGWRT (3)
                     END IF
                  END IF
C                                       Save conherence time for
C                                       antennas
               ANTB(ANT1) = ANTB(ANT1) + COTIM2
               ANTBIN(ANT1) = ANTBIN(ANT1) + COTIM2
               ANTCNT(ANT1) = ANTCNT(ANT1) + NAVERA
               ANTIN(ANT1) = ANTIN(ANT1) + 1
               ANTOUT(ANT1) = ANTOUT(ANT1) + 1
C
               ANTB(ANT2) = ANTB(ANT2) + COTIM2
               ANTBIN(ANT2) = ANTBIN(ANT2) + COTIM2
               ANTCNT(ANT2) = ANTCNT(ANT2) + NAVERA
               ANTIN(ANT2) = ANTIN(ANT2) + 1
               ANTOUT(ANT2) = ANTOUT(ANT2) + 1
C
               ANTOT(ANT1) = ANTOT(ANT1) + NAVERA
               ANTOT(ANT2) = ANTOT(ANT2) + NAVERA
C                                       Save coherence time for
C                                       baselines
               BASTIM(LOOP) = BASTIM(LOOP) + COTIM2
               BASCNT(LOOP) = BASCNT(LOOP) + 1
               BASTOT(LOOP) = BASTOT(LOOP) + NAVERA
C                                       count number of unflaged point
C                                       for the current baseline
               DO 170 NTIM = 1, NTIME(LOOP)
C                                       Get vis
                  CALL VGET (NTIM, LOOP, NTIMEM, VISBUF, VIS,
     *               COUNT)
  170             CONTINUE
C                                       End if Finished average
C                                       Re init time bins
               IF (.NOT. EOF) NTIME(LOOP) = 0
C
               IF (OUTTYP.GT.0.01) THEN
                  WRITE (LINE,1420)
                  CALL DOLP (IERR)
                  IF (IERR.NE.0) GO TO 500
                  END IF
  180          CONTINUE
C                                       Report antenna coherence times
            IF (OUTTYP .LT. 0.9) THEN
               WRITE (LINE,1402)
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 500
               LINE = 'I  ANTENNA   I Number of averaging I ' //
     *            'Coherence time, min I'
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 500
               WRITE (LINE,1402)
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 500
C
               DO 190 I = 1, MXANT
C                                       If counts for this antenna
                  IF (ANTIN(I).GT.0) THEN
                     COHANT = ANTBIN(I)/ANTIN(I)
C
                     WRITE (LINE,1400)  I, ANTCNT(I), 24.0*60.*COHANT
                     CALL DOLP (IERR)
                     IF (IERR.NE.0) GO TO 500
                     END IF
  190             CONTINUE
               WRITE (LINE,1402)
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 500
               END IF
C                                       Reinit new time interval
            TLEFT = TIME
            TRIGHT = TLEFT + TIMEST
            IF (TRIGHT .GE. TSTOP) TRIGHT = TSTOP
            END IF
C                                       we are still at the same source
C                                       but maybe time interval exceeds
C                                       max
C
C                                       Exit on End of File
         IF (EOF) GO TO 200
C                                       Accumulate
         IF (NTIME(IBASE) .EQ. 0) TIME0(IBASE) = TIME
         DTIME = TIME - TIME0(IBASE)
         IF (DTIME .GT. MXATIM) GO TO 100
C                                       save source ID and time
         IF (DOSU) IDSAV = RP(ILOCSU)
C                                       source selection
         SID = 1
         IF (DOSU) SID = RP(ILOCSU)
         IF (NSOU .EQ. 0) GO TO 150
         DO 140 LOOP = 1, NSOU
            IF (SID .EQ. SOUID(LOOP) .OR. EOF) THEN
               SELSO = SOLIST(LOOP)
               GO TO 150
               END IF
  140       CONTINUE
C                                       Not wanted
         GO TO 100
  150    CONTINUE
         IF (NTIME(IBASE) .LT. NTIMEM) THEN
            NTIME(IBASE) = NTIME(IBASE) + 1
            IF (NTIME(IBASE) .GT. 0) THEN
               TIMBIN(NTIMEM*(IBASE-1) + NTIME(IBASE)) = TIME
               END IF
C                                       Save visibility
            CALL VPUT (INDEX, NCHAN, NTIME(IBASE), IBASE, NTIMEM,
     *            DELAYS, VS, VISBUF)
         ELSE
            IERR = 1
C                                       Buffer's size is not enough.
            WRITE (MSGTXT,1220)
            CALL MSGWRT (7)
            GO TO 999
            END IF
C                                       Loop for next vis
         GO TO 100
C                                       Done
 200  IERR = 0
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IERR = 7
         MSGTXT = 'UVCOHE: NO DATA SELECTED'
         GO TO 995
      ELSE
         WRITE (MSGTXT,1250) COUNT
         CALL MSGWRT (4)
         END IF
C                                       Prepare to calculate average T
      NANT = 0
      COTIME = 0.0
      MINTIM = 1E20
C                                       Report averaged baselines
C                                       coherence times
      IF (OUTTYP.LT.0.9) THEN
         LINE = ' Averaged coherence times'
         CALL DOLP (IERR)
         IF (IERR.NE.0) GO TO 500
         LINE = 'I  BASELINE  I Number of averaging I ' //
     *      'Coherence time, min I'
         CALL DOLP (IERR)
         IF (IERR.NE.0) GO TO 500
C
         WRITE (LINE,1402)
         CALL DOLP (IERR)
         IF (IERR.NE.0) GO TO 500
C
         DO 300 I = 1, NBASE
            IF (BASCNT(I) .GT. 0.0) THEN
               WRITE (LINE,1410) ANTS1(I), ANTS2(I), BASTOT(I),
     *            24.0*60.*(BASTIM(I)/BASCNT(I))
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 500
               END IF
  300       CONTINUE
C
C                                       Report antenna coherence times
         WRITE (LINE,1402)
         CALL DOLP (IERR)
         IF (IERR.NE.0) GO TO 500
         LINE = 'I  ANTENNA   I Number of averaging I ' //
     *      'Coherence time, min I'
         CALL DOLP (IERR)
         IF (IERR.NE.0) GO TO 500
         WRITE (LINE,1402)
         CALL DOLP (IERR)
         IF (IERR.NE.0) GO TO 500
C
         DO 400 I = 1,MXANT
C                                       If counts for this antenna
            IF (ANTCNT(I).GT.0) THEN
               COHANT = ANTB(I) / ANTOUT(I)
C
               WRITE (LINE,1400)  I, ANTOT(I), 24.0*60.*COHANT
               CALL DOLP (IERR)
               IF (IERR.NE.0) GO TO 500
C
               NANT = NANT + 1
               COTIME = COTIME + COHANT
               MINTIM = MIN (MINTIM, COHANT)
C                                       COTIME is avereged for all
C                                       antennas
C                                       MINTIM is minimum for all
C                                       antennas
               END IF
 400        CONTINUE
         WRITE (LINE,1402)
         CALL DOLP (IERR)
         IF (IERR.NE.0) GO TO 500
C                                       Report Average coherence time
         IF (NANT.GT.0) THEN
            WRITE (LINE, 1450) NANT, 24.0*60.*COTIME/NANT
            CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 500
C                                       Report minimum coherence time
            WRITE (LINE,1460) NANT, 24.0*60.*MINTIM
            CALL DOLP (IERR)
            IF (IERR.NE.0) GO TO 500
            END IF
         END IF
 500  CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'UVCOHE: ERROR CLOSING ' // UVIN
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('UVCOHE: ERROR ',I3,' READING CATBLK FOR AN TABLE')
 1000 FORMAT ('UVCOHE: TOO MANY CORRELATIONS ', I6,' > ', I6)
 1080 FORMAT ('UVCOHE: ERROR ',I3,' OPENING OUTPUT ''PRINT'' DEVICE')
 1105 FORMAT ('UVCOHE: Flagging vis with Deviation        >',F11.2,
     *   ' Sigma')
 1110 FORMAT ('UVCOHE: Coherence from Vector/Scalar Ratio <',F11.2)
 1220 FORMAT ('Buffer size is not enough. Decrease APARM(1)')
 1250 FORMAT ('UVCOHE: ',I7,' visibilities have been used')
 1300 FORMAT (I2,'-',I2,' (',I4,') T=',F6.2,',',F6.2)
 1310 FORMAT (' Time range  ',3I3, F5.1, ' -', 3I3, F5.1)
 1320 FORMAT (' Source ', A16)
 1400 FORMAT ('I',4X,I2,6X, 'I',7X, I4,10X,'I',3X, F10.3,
     *       8X, 'I')
 1402 FORMAT (58('-'))
 1406 FORMAT (A2, 1X, A2, ' /')
 1408 FORMAT (I3, 1X, 2(I2.2,':'), I2.2, '.', 1X, I3, 1X, 2(I2.2,':'),
     *   I2.2, '.', 1X, F5.2, 9(' 0.0'))
 1410 FORMAT ('I',2X,I2,' - ',I2, 3X, 'I',7X, I4,10X,'I',3X, F10.3,
     *       8X, 'I')
 1420 FORMAT ('/')
 1450 FORMAT ('UVCOHE:',I3,' antennas have ',F10.3,' Average Coherence',
     *       ' Time (Min)')
 1460 FORMAT ('UVCOHE:',I3,' antennas have ',F10.3,' Minimum Coherence',
     *       ' Time (Min)')
      END
      SUBROUTINE VPUT (INDEX, NCHAN, ITIME, IBASE, NTIMEM, DELAYS, VIS,
     *   VISBUF)
C-----------------------------------------------------------------------
C! copies one visability to buffer
C   Inputs:
C      INDEX   I          Index of point at a visibility array
C                         corresponding to the beginning channel
C                         INDEX = BCHAN + INCIF*(JIF-1) + INCS*(JS-1)
C      NCHAN   I          Number of channels to average
C      ITIME   I          Time index for the given baseline
C      IBASE   I          Baseline number
C      NTIMEM  I          Maximum time's points for a baseline
C      DELAYS  I          0 => search delay in frequency averaging
C                         1 not search the delay
C      VIS     R(3,NCHAN) Input  visibility
C   Output:
C      VISBUF  R(*)       Output array
C-----------------------------------------------------------------------
      INTEGER   NCHAN, ITIME, IBASE, INDEX, NTIMEM
      REAL      VIS(3,*), VISBUF(*)
C
      INCLUDE 'UVSTUFF.INC'
      REAL      DATA(2*MXVS), AMPP, RATE, FI, DDAMP, DDRATE, DDFI,
     *   DATRE, DATIM
      INTEGER   I, IOFF, DELAYS, IERR
C-----------------------------------------------------------------------
C                                       If valid vis and time
      IF (((NCHAN.GT.0).AND.(ITIME.GT.0)) .AND. ITIME.LT.NTIMEM) THEN
C                                       Calc time&baseline offset
         IOFF = (NTIMEM*(IBASE-1) + ITIME-1)*3
         DO 20 I = 1,NCHAN
C                                       store complex amplitudes
            DATA(2*I-1) = VIS(1, INDEX+I-1)
            DATA(2*I)   = VIS(2, INDEX+I-1)
   20       CONTINUE
         IF (NCHAN .GT. 2) THEN
C                                       estimate amplitide averaging
C                                       channels including FFT and least
C                                       square
            IF (DELAYS .EQ. 0) THEN
               CALL AFRFI (DATA, NCHAN, AMPP, RATE, FI, DDAMP,
     *            DDRATE, DDFI, IERR)
               VISBUF(IOFF+1) = AMPP
               VISBUF(IOFF+2) = FI
C                                       estimate amplitide using simple
C                                       vector averaging of channels
            ELSE
               DATRE = 0.0
               DATIM   = 0.0
               DO 40 I = 1,NCHAN
C                                       store complex amplitudes
                  DATRE = DATRE + VIS(1, INDEX+I-1)
                  DATIM = DATIM + VIS(2, INDEX+I-1)
   40             CONTINUE
               VISBUF(IOFF+1) = SQRT (DATRE*DATRE +
     *               DATIM*DATIM) / NCHAN
               VISBUF(IOFF+2) = ATAN2 (DATIM, DATRE)
               END IF
         ELSE
            VISBUF(IOFF+1) = SQRT (DATA(1)*DATA(1) + DATA(2)*DATA(2))
            VISBUF(IOFF+2) = ATAN2 (DATA(2), DATA(1))
            END IF
C                                       store in buffer the weight
C                                       considering weight of all
C                                       chanels are identical
         VISBUF(IOFF+3) = VIS(3,INDEX)
         END IF
C
 999  RETURN
      END
      SUBROUTINE VGET (ITIME, IBASE, NTIMEM, VISBUF, VIS, COUNT)
C-----------------------------------------------------------------------
C   copies one visibility from buffer
C
C   Inputs:
C      ITIME   I          Time index for the given baseline
C      IBASE   I          Baseline number
C      NTIMEM  I          Maximum time's points for a baseline
C      VISBUF  R(*)       Buffer
C   Outputs:
C      VIS     R(3)       Gotten visibility: amplitude, phase, weight
C      COUNT   I          Count of gotten visibilities
C-----------------------------------------------------------------------
      INTEGER  ITIME, IBASE, NTIMEM, COUNT
      INCLUDE 'UVSTUFF.INC'
      REAL      VIS(3), VISBUF(*)
C
      INTEGER   IOFF
      LOGICAL   FLAGED
C-----------------------------------------------------------------------
C                                       If channels in data
      IF (ITIME.GT.0 .AND. ITIME.LT.NTIMEM) THEN
C                                       Assume flagged
         FLAGED = .TRUE.


C                                       Calc time&baseline offset
         IOFF = (NTIMEM*(IBASE-1) + ITIME-1)*3
C                                       get amplitude&phase
         VIS(1) = VISBUF(IOFF+1)
         VIS(2) = VISBUF(IOFF+2)
C                                       get the weight
         VIS(3) = VISBUF(IOFF+3)
         END IF
      IF (VIS(3) .GT. 0.0) COUNT = COUNT + 1
C
 999  RETURN
      END
      SUBROUTINE RMS (ITIME1, ITIME2, IBASE, NTIMEM, VISBUF,
     *    AMPSCL, AMPRMS)
C-----------------------------------------------------------------------
C! Estimate vector and scalar average and rms of the amplitude
C   Inputs:
C      ITIME1   I          Start Time index for vis
C      ITIME2   I          Stop  Time index for vis
C      IBASE    I          Baseline number
C      NTIMEM   I          Maximum time points at the baseline
C      VISBUF   R(*)       Input array
C   Output:
C      AMPSCL   R          Scalar average of amplitude
C      AMPRMS   R          RMS of the scalar amplitude
C-----------------------------------------------------------------------
      INTEGER   ITIME1, ITIME2, IBASE, NTIMEM
      INCLUDE   'UVSTUFF.INC'
      REAL      VISBUF(*), AMPSCL, AMPRMS, AMPP, AMPP2, AMPSQR
C
      INTEGER   J, VISCNT, COUNT
      REAL  VIS(3)
      LOGICAL FLAG
C-----------------------------------------------------------------------
      IF ((ITIME1.GT.0) .AND. (ITIME2.GE.ITIME1)) THEN
C                                       Init correlator counts
         VISCNT = 0
         AMPSCL = 0.0
         AMPSQR = 0.0
C                                       For all vis.
         DO 200 J = ITIME1,ITIME2
C                                       Get vis
            CALL VGET (J, IBASE, NTIMEM, VISBUF, VIS, COUNT)
            FLAG = VIS(3) .LT. 0
            IF (.NOT. FLAG) THEN
               VISCNT = VISCNT + 1
               AMPP = VIS(1)
               AMPP2 = AMPP * AMPP
               AMPSCL = AMPSCL + AMPP
               AMPSQR = AMPSQR + AMPP2
               END IF
 200        CONTINUE
C                                       estimate amplitude and its rms
         IF (VISCNT .GT. 0) AMPSCL = AMPSCL / VISCNT
         IF (VISCNT .GT. 1) THEN
            AMPRMS = AMPSQR / VISCNT - (AMPSCL * AMPSCL)
            AMPRMS = SQRT(AMPRMS)
         ELSE
            AMPRMS = 1.E20
            END IF
         END IF
      RETURN
      END
      SUBROUTINE RATIO (ITIME1, ITIME2, COHTIM, VECSCL, IBASE, NTIMEM,
     *   VISBUF, TIMBIN, IERR)
C-----------------------------------------------------------------------
C! Vector/Scalar average ratio limit.  This routine finds the time index
C  at which the Vector/Scalar amplitude ratio drops below VECSCL
C   Inputs:
C      ITIME1  I          Start Time index for vis
C      ITIME2  I          Stop  Time index for vis
C      VECSCL  I          Vector/scalar average limit
C      IBASE   I          Baseline number
C      NTIMEM  I          Maximum time points at the baseline
C      VISBUF  R(*)       Buffer of visibilities to examine
C      TIMBIN  R(*)       Array of times
C   Output:
C      COHTIM  I          Coherence time limit
C-----------------------------------------------------------------------
      INTEGER MXPOIN, MXPOI2
      PARAMETER (MXPOIN = 8192, MXPOI2 = MXPOIN * 2)
      INTEGER   ITIME1, ITIME2, COHTIM, IBASE, NTIMEM, IERR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UVSTUFF.INC'
      REAL      VECSCL, VISBUF(*), TIMBIN(*)
C
      INTEGER   J, VISCNT, COUNT, IND, NTOTAL, LEFT, RIGHT,
     *   NPOINT
      REAL      VIS(3), AMPVEC, AMPSCL, AMPP, FI, AMPSCT, DATA(MXPOI2),
     *   ARG(MXPOIN), DARGM, DARG, FII, RATE, DRATE, DFI, AMPRMS,
     *   AMPSC(MXPOIN)
      LOGICAL   FLAG, ACHIEV
C-----------------------------------------------------------------------
      IERR = 0
C                                       Init time index to last time
      COHTIM = 5 + ITIME1 - 1
C                                       Init correlator counts
      VISCNT = 0
      AMPSCL = 0.0
      DO 40 J = ITIME1, ITIME2
         IND = NTIMEM * (IBASE - 1) + J
C                                       Get vis
         CALL VGET (J, IBASE, NTIMEM, VISBUF, VIS, COUNT)
         FLAG = VIS(3) .LT. 0
         IF (.NOT. FLAG) THEN
            VISCNT = VISCNT + 1
            AMPP = VIS(1)
            FI = VIS(2)
            AMPSCL = AMPSCL + AMPP
            AMPSC(VISCNT) = AMPSCL
            DATA(2*VISCNT - 1) =  AMPP * COS(FI)
            DATA(2*VISCNT) = AMPP * SIN(FI)
            ARG(VISCNT) = TIMBIN(IND)
C                                       Find a minimum spacing in time
            IF (VISCNT .EQ. 2) DARGM = ARG(2) - ARG(1)
            IF (VISCNT .GT. 2)
     *         DARGM = MIN (DARGM, (ARG(VISCNT) - ARG(VISCNT - 1)))
            END IF
 40     CONTINUE
C
      DARG = 2 * DARGM
      NTOTAL = VISCNT
C                                       check the maximum number of the
C                                       points
      IF (NTOTAL .GT. MXPOIN) THEN
         IERR = 1
         WRITE (MSGTXT,1000) NTOTAL, MXPOIN
         CALL MSGWRT (7)
         GO TO 999
         END IF
      LEFT = 1
      RIGHT = NTOTAL
      ACHIEV = .TRUE.
C                                       cycle of time deviding by two at
C                                       each step and analysing which
C                                       halve the coherence time belongs
C                                       to.
  100 CONTINUE
         NPOINT = RIGHT - LEFT + 1
         IF (NPOINT .LE. 2) THEN
C                                       right achieved; left not
Cnew
C            COHTIM = LEFT + ITIME1 - 1
            COHTIM = LEFT + ITIME1
C                                       Exit, limit found
            GO TO 999
            END IF
C                                       provide comparison of vector and
C                                       scalar averaging for the
C                                       number of visibilities .GE. 6
         IF (RIGHT .GE. 6) THEN
C
            CALL AFRF(DATA, RIGHT, ARG, DARG, AMPVEC, RATE, FII,
     *         AMPRMS, DRATE, DFI, IERR)
C                                       estimate scalar averaging

            AMPSCT = AMPSC(RIGHT) / RIGHT
            ACHIEV = AMPVEC .LT. VECSCL*AMPSCT
C                                       See if limit is exceeded.
            IF (ACHIEV) THEN
               RIGHT = LEFT + NPOINT / 2
               GO TO 100
            ELSE
               IF (RIGHT .EQ. NTOTAL) THEN
                  COHTIM = NTOTAL + ITIME1 - 1
C                                       Exit, limit found
                  GO TO 999
               ELSE
                  LEFT = RIGHT
                  RIGHT = LEFT + NPOINT / 2
                  GO TO 100
                  END IF
               END IF
            END IF
C                                       Jump here on limit determined
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Number of points', I5, 'exceeds ', I4,
     *   ' Decrease APARM(1)')
      END
C
      INTEGER FUNCTION ANTONE(BASELN)
      REAL BASELN
C                                       Crack First antenna number
      ANTONE = INT(BASELN)/256
      RETURN
C-----------------------------------------------------------------------
      END
      INTEGER FUNCTION ANTTWO(BASELN)
C-----------------------------------------------------------------------
      REAL BASELN
C                                       Crack second antenna number
      ANTTWO = MOD(INT(BASELN),256)
      RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE VFLAG (INDEX, ITIME1, ITIME2, IBASE, NTIMEM, MINSNR,
     *   AMPSCL, AMPRMS, VISBUF, DELAYS)
C-----------------------------------------------------------------------
C! Flags the point with deviation from mean more than a given value
C   Inputs:
C      INDEX   I          Index of point at a visibility array
C      ITIME1  I          Start Time index
C      ITIME2  I          Stop  Time index
C      IBASE   I          baseline number
C      NTIMEM  I          Maximum time foints for the baseline
C      MINSNR  I          Minimum SNR to pass
C      AMPSCL  R          Scalar average of amplitude
C      AMPRMS  R          RMS of amplitude
C      VISBUF  R(*)   Buffer of visibilities to examine
C      DELAYS  I          Input for VPUT
C---------------------------------------------------------------------
      INTEGER  ITIME1, ITIME2, INDEX
      INCLUDE 'UVSTUFF.INC'
      REAL      MINSNR, VISBUF(*)
C
      REAL      EPSILN
C                                       Small Vis Amplitude
      PARAMETER (EPSILN=1.0E-10)
      INTEGER   LOOP, COUNT, IBASE, NTIMEM, DELAYS
      LOGICAL   FLAGED
      REAL      VIS(3), AMPSCL, AMPRMS, AMPP
C-----------------------------------------------------------------------
C                                       For all visibilities
      DO 20 LOOP = ITIME1,ITIME2
C                                       Get vis
         CALL VGET (LOOP, IBASE, NTIMEM, VISBUF, VIS, COUNT)
         FLAGED = VIS(3) .LT. 0.0
         AMPP = VIS(1)
C                                       If RMS known and unflagged
         IF (AMPRMS .GT. EPSILN .AND. .NOT.FLAGED) THEN
C                                       If visibility exceeds minrms
            IF (ABS(AMPP - AMPSCL) .GT. MINSNR*AMPRMS) THEN
               VIS(3) = -ABS(VIS(3))
               FLAGED = .TRUE.
               END IF
            END IF
      IF (FLAGED) CALL VPUT (INDEX, 1, LOOP, IBASE, NTIMEM, DELAYS,
     *         VIS, VISBUF)
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE VCOHER (ITIME1, ITIME2, TFACT, IBASE, VECSCL, NTIMEM,
     *   TIMBIN, VISBUF, COTIME, NAVERA, COHERT, TSTA, TEND, IERR)
C-----------------------------------------------------------------------
C! Get average coherence time
C   Inputs:
C      ITIME1  I          Start Time index
C      ITIME2  I          Stop  Time index
C      IBASE   I          Baseline number
C      VECSCL  R          Vector/Scalar Average limit
C      NTIMEM  I          Maximum time points for the baseline
C      TIMBIN  R(*)       Array of times for baselines/time indexes
C      VISBUF  R(NREAL)   Buffer of visibilities to examine
C   Output:
C      COTIME  R          Average coherence time
C      NAVERA  I          Number of averaging
C      COHERT  R(*)       Array of sequence found time intervals
C      IERR    I          Error
C---------------------------------------------------------------------
      INTEGER   ITIME1, ITIME2, ITIME3, IBASE, NTIMEM, IERR
      INCLUDE 'UVSTUFF.INC'
      REAL   COHERT(*), TSTA(*), TEND(*)
      REAL      TFACT, VECSCL, TIMBIN(*), COTIME, VISBUF(*)
C
      INTEGER   LOOP, NCOTIM, ICOTIM, ILAST, COUNT, IND, IND1,
     *   NAVERA, DII
      LOGICAL   FLAGED
      REAL      VIS(3)
C-----------------------------------------------------------------------
C                                       Init count of coherence times
      NCOTIM = 0
      COTIME = 0.
      ILAST  = 0
      DII = ITIME2 / TFACT
C                                       For all visibilities
      DO 200 LOOP = ITIME1,ITIME2
         IERR = 0
C                                       Get vis
         CALL VGET (LOOP, IBASE, NTIMEM, VISBUF, VIS, COUNT)
C                                       Assume flagged
         FLAGED = VIS(3) .LT. 0.0
C                                       If unflagged element
         IF ((.NOT.FLAGED) .AND. (LOOP.GT.ILAST)) THEN
C                                       Get coherence time
            ITIME3 = LOOP + DII -1
            IF (ITIME3 .GE. ITIME2) ITIME3 = ITIME2
            CALL RATIO (LOOP, ITIME3, ICOTIM, VECSCL, IBASE, NTIMEM,
     *         VISBUF, TIMBIN, IERR)
            IF (IERR .NE. 0) GO TO 200
C                                       The following condition (.LT.)
C                                       excludes the last interval,
C                                       which can be small.
C                                       IF the last interval is the only
C                                       one it is included
C            IF ((ICOTIM .LT. ITIME2) .OR. (NCOTIM .EQ. 0)) THEN
            IF ((ICOTIM .LE. ITIME2) .OR. (NCOTIM .EQ. 0)) THEN
C                                       Add coherence time to average
               IND = NTIMEM*(IBASE-1) + ICOTIM
               IND1 = NTIMEM*(IBASE-1) + LOOP
               COTIME = TIMBIN(IND) - TIMBIN(IND1) +
     *                  COTIME
               NCOTIM = NCOTIM + 1
C
               TSTA(NCOTIM) = TIMBIN(IND1)
               TEND(NCOTIM) = TIMBIN(IND)
               COHERT(NCOTIM) = TIMBIN(IND) - TIMBIN(IND1)
               END IF
C                                       Record end of last Coherence
            ILAST = ICOTIM
C                                       End if unflagged vis
            END IF
 200     CONTINUE
C                                       If points in average
      IF (NCOTIM.GT.0) THEN
         COTIME = COTIME / NCOTIM
      ELSE
C                                       Else use preaverage time
         IF (ITIME2.GT.ITIME1) THEN
            COTIME = TIMBIN(NTIMEM*(IBASE-1) + ITIME1+1) -
     *         TIMBIN(NTIMEM*(IBASE-1) + ITIME1)
            END IF
         END IF
      NAVERA = NCOTIM
 999  RETURN
      END
      SUBROUTINE ONETWO (ANTEN, BASL, MXANT, NBASE, ANT1, ANT2, ANTNS,
     *   NANTS, IERR)
C-----------------------------------------------------------------------
C   Fills arrays of the first and second  antennas for selected baselines
C   Find  number and list of selected antennas.
C-----------------------------------------------------------------------
C   Inputs:
C      ANTEN    I(*)    List of user supplied antennas
C      BASL     I(*)    List of user baselines to match ANTEN
C      MXANT    I       Max number of enrties at ANTEN and BASL
C   Output:
C      NBASE    I       Max # baselines
C      ANT1     I(*)    1st antenna number of baseline pairs selected
C      ANT2     I(*)    2nd antenna number of baseline pairs selected
C      ANTNS    I(*)    Array of selected antennas
C      NANTS    I       Number of selected antennas
C      IERR     I       Error
C-----------------------------------------------------------------------
      INTEGER   ANTEN(*), BASL(*), NANT, NBASL, NBASE, ANT1(*),
     *   ANT2(*), ANTNS(*), K, I, MXANT, NANTS, IERR
      LOGICAL   DESEL, FOUND, ACCEPT, REQBAS
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IANT(MAXANT), IBAS(MAXANT), II
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find baselines to plot
      NANT = 0
      NBASL = 0
      DESEL = .FALSE.
      DO 25 I = 1,MXANT
         II = ABS (ANTEN(I))
         IF (II.NE.0) THEN
            DO 10 K = 1,NANT
               IF (II.EQ.IANT(K)) GO TO 15
 10            CONTINUE
            NANT = NANT + 1
            IANT(NANT) = II
            IF (ANTEN(I).LT.0) DESEL = .TRUE.
            END IF
 15      II = ABS (BASL(I))
         IF (II.NE.0) THEN
            DO 20 K = 1,NBASL
               IF (II.EQ.IBAS(K)) GO TO 25
 20            CONTINUE
            NBASL = NBASL + 1
            IBAS(NBASL) = II
            IF (BASL(I).LT.0) DESEL = .TRUE.
            END IF
 25      CONTINUE
C                                       find the first and second
C                                       antennas arrays of selected
C                                       baselines
      NBASE = 0
      DO 60 I = 1,MXANT
         DO 40 K = I+1,MXANT
            ACCEPT = REQBAS (I, K, DESEL, IANT, NANT, IBAS, NBASL)
            IF (ACCEPT) THEN
               NBASE = NBASE + 1
               ANT1(NBASE) = I
               ANT2(NBASE) = K
               END IF
 40         CONTINUE
 60      CONTINUE
C                                       Find number of selected antennas
C                                       and their list
      II = 0
      DO 100 I = 1, MXANT
         FOUND = .FALSE.
         DO 80 K = 1, NBASE
            IF (FOUND) GO TO 100
            IF (I .EQ. ANT1(K)) THEN
               FOUND = .TRUE.
               II = II + 1
               ANTNS(II) = ANT1(K)
               END IF
            IF (I.EQ.ANT2(K)) THEN
               FOUND = .TRUE.
               II = II + 1
               ANTNS(II) = ANT2(K)
               END IF
   80       CONTINUE
  100    CONTINUE
      NANTS = II
C
 999  RETURN
      END
      SUBROUTINE SOURNU (SOURCE, QUAL, NSOUR, DISK, CNO, NID, NNID,
     *   BUFFER, ID, SOLIST, IRET)
C-----------------------------------------------------------------------
C   Determines the source numbers of a list of source names from the
C   source table associated with a specified catalog entry.
C   Inputs:
C      SOURCE  C*16(*)   List of source names.
C                        If the first character of any source names
C                        begins with a "-", all sources EXCEPT those
C                        named will be returned ( the "-" will be
C                        ignored in determining the source name).
C                        Blank source names are ignored.  Names should
C                        be left justified, blank filled
C      QUAL      I(*)    SOURCE qualifiers, .lt. 0 => any.
C      NSOUR     I       Number of entries in SOURCE, may include
C                        blank names.
C      DISK      I       Disk number of the data set.
C      CNO       I       Catalog slot number of data set.
C   Input/Output:
C      NID       I       On input the maximum number of elements to be
C                        filled in ID; on output, the number of elements
C                        in ID.
C      BUFFER    I(512)  Work buffer, used for I/O and manipulating
C                        source lists, should be at least min (512,NID)
C   Output:
C      ID        I(*)    Source ID numbers of selected sources,
C                        If ID(1)=0 then all sources are selected.
C      SOLIST  C*16(*)   List of selected sources' names
C      NNID      I       If all selected .EQ. 0; If not .EQ. 1
C      IRET      I       Return code. 0 => OK; else failed.
C   Usage notes:
C       This routine uses AIPS LUN 27 which will be closed on normal
C   return.
C       Version 1 of the source table is assumed.
C-----------------------------------------------------------------------
      CHARACTER SOURCE(*)*16, SOLIST(*)*16
      INTEGER   QUAL(*), NSOUR, DISK, CNO, NID, BUFFER(*), ID(*),
     *   IRET, Q, NNID
C
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   CAT(256), VER, LUN, IDKOL, SUKOL, I, IDSOU, SQUAL, J,
     *   MAXID, NUMIF, ISURNO, NUMREC, I4, SUFQID
      LOGICAL   EQUAL, DESEL, ALLSEL, GOTIT, ALLBLN
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, OBSRA, OBSDEC
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC)
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IDKOL, SUKOLS(1)),   (SUKOL, SUKOLS(2))
      DATA VER, LUN /1, 27/
C-----------------------------------------------------------------------
C                                       Setup
      MAXID = NID
      NID = 0
      IRET = 0
C                                       Check if sources deselected or
C                                       if all sources are selected.
      DESEL = .FALSE.
      ALLSEL = .TRUE.
      ALLBLN = .TRUE.
      DO 10 I = 1,NSOUR
C                                       Check deselection
         DESEL = DESEL .OR. SOURCE(I)(1:1).EQ.'-'
C                                       Check if all blank (GvM, 1/93)
         ALLBLN = ALLBLN .AND. (SOURCE(I).EQ.'                ')
C                                       Check if all blank, no qual
         ALLSEL = ALLSEL .AND. (SOURCE(I).EQ.'                ')
     *              .AND. (QUAL(I) .LT. 0)
C
 10      CONTINUE
C                                       Check all selected case.
C      ID(1) = 0
C      IF (ALLSEL) GO TO 999
      NNID = 1
      IF (ALLSEL) NNID = 0
C                                       Get catalog header.
      CALL CATIO ('READ', DISK, CNO, CAT, 'REST', BUFFER, IRET)
      IF ((IRET.GT.0) .AND. (IRET.LT.5)) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Initialize SOURCE table.
      CALL SOUINI ('READ', BUFFER, DISK, CNO, VER, CAT, LUN, NUMIF,
     *   VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
         END IF
C                                       Get number of entries
      NUMREC = BUFFER(5)
C                                       Loop through source records.
      DO 500 ISURNO = 1,NUMREC
C                                       Read record
         I4 = ISURNO
         CALL TABSOU ('READ', BUFFER, I4, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, SQUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, OBSRA, OBSDEC, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IRET)
C                                       See is source record turned off
         IF (IRET.LT.0) GO TO 500
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
            END IF
C                                       Check if in list SOURCE.
         GOTIT = .FALSE.
         DO 300 J = 1,NSOUR
C                                       See if deselected.
            IF (.NOT.DESEL) THEN
C                                       Sources selected.
               EQUAL = SOURCE(J).EQ.SOUNAM
C                                       Check QUAL
               Q = QUAL(J)
C                                       is qualifier OK, too?
               EQUAL = EQUAL .AND. ((QUAL(J).LT.0) .OR.
     *            (QUAL(J).EQ.SQUAL))
C                                       cover blank source and
C                                       given qualifier
               EQUAL = EQUAL .OR. ALLSEL
               IF (EQUAL) THEN
                  IF ((NID+1).GT.MAXID) GO TO 310
                  NID = NID + 1
                  ID(NID) = IDSOU
                  SOLIST(NID) = SOUNAM
                  GO TO 320
                  END IF
            ELSE
C                                       Deselected
C                                       Check for leading "-"
               IF (SOURCE(J)(1:1).EQ.'-') THEN
                  EQUAL = SOURCE(J)(2:16).EQ.SOUNAM(1:15)
               ELSE
                  EQUAL = SOURCE(J).EQ.SOUNAM
                  END IF
C                                       Check QUAL
C               EQUAL = EQUAL .AND. ((QUAL(J).EQ.0) .OR.
               EQUAL = EQUAL .AND. ((QUAL(J).LT.0) .OR.
     *            (QUAL(J).EQ.SQUAL))
               GOTIT = GOTIT .OR. EQUAL
               END IF
 300        CONTINUE
            IF (DESEL .AND. (.NOT.GOTIT)) THEN
C                                       Source not deselected
               IF ((NID+1).GT.MAXID) GO TO 310
               NID = NID + 1
               ID(NID) = IDSOU
               SOLIST(NID) = SOUNAM
               END IF
         GO TO 320
C                                       Too many sources selected
 310        WRITE (MSGTXT,1300) MAXID
            IRET = 5
            GO TO 990
 320     CONTINUE
 500     CONTINUE
C                                       Close Source table
      CALL TABIO ('CLOS', 0, I4, SOUNAM, BUFFER, IRET)
         GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SOURNU: ERROR ',I3,' READING CATBLK FOR SOURCE TABLE')
 1100 FORMAT ('SOURNU: ERROR ',I3,' INITIALIZING SOURCE TABLE')
 1120 FORMAT ('SOURNU: ERROR ',I3,' READING SOURCE TABLE')
 1300 FORMAT ('SOURNU: MORE SOURCES SELECTED THAN MAX (',I5,')')
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
      SUBROUTINE AFRF (DATA, NNNIN, XARR, DARGM, AMPP, RATE, FI, DDAMP,
     *   DDRATE, DDFI, IERR)
C-----------------------------------------------------------------------
C   Estimates the values of frequency, initial phase and amplitude of
C   a complex exponent given (with noise) in the data set.
C   The DATA are determined on non aquidistant array XARR
C   (XARR(I) > XARR(I-1)) and can be approximated by:
C   DATA(I) = A * EXP (J*TWOPI*RATE*(XARR(I)-XARR(1)) + FI)
C   where   RATE is unknown rate of phase change;
C           FI   is unknown phase at the first data point;
C           A    is unknown amplitude.
C   The data are averaged at the given interval transforming possible
C   uneven distribution of the data to the even one with the step DARGM.
C
C   A two step process is used. The first is an Fourie analisys (FFT).
C   The final solution of the amplitude, frequency and phase is found by
C   least square method.
C-----------------------------------------------------------------------
C   Programmer  L.R. Kogan
C-----------------------------------------------------------------------
C   Inputs:
C      DATA     R(2*NNNIN)    input array of data
C      NNNIN    I             Number of complex points in the DATA
C      XARR     R(NNNIN)      array of argument of input data
C      DARGM    R             Element of space of input data
C   Outputs
C      AMPP     R             found amplitude
C      RATE     R             found frequency
C      FI       R             found phase of the most left point of
C                             the data, in radian
C      DDAMP    R             rms of amplitude
C      DDRATE   R             rms of frequency
C      DDFI     R             rms of phase
C      IERR     I             error
C-----------------------------------------------------------------------
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INTEGER   NNNIN, I, LESOL, IK, K, ITER, IERR, ICNT, NIN, ISIGN,
     *   NNN, NNN2
      REAL DATA(*), RATE, FI, AMPP, TAMP, R(3), MATR(9), NOBS, SUM, SSQ,
     *   SOL(3),VX(3), SSQRES, VARRES, VARY, FIT,DRE, DIM, ARG,
     *   SINK, COSK, DDAMP, DDRATE,DDFI, XARR(*), DATAIN(20000),
     *   DARGM, DAT(20000), START, STOP, WORK(4096), TWOP, TWOPP
      IERR = 0
C      IF (NNNIN .GT. 1024)  THEN
C         IERR = 1
C         WRITE (MSGTXT,1000) NNNIN
C         CALL MSGWRT (8)
C         GO TO 999
C         END IF
C
C                                       average at the given interval
      START = XARR(1)
      STOP = START + DARGM
      ICNT = 1
      NIN = 0
      DO 10 I = 1, NNNIN
         DAT(2*I - 1) = 0.0
         DAT(2*I) = 0.0
   10    CONTINUE
      DO 30 I = 1, NNNIN
   20    IF (XARR(I) .GE. 0.99999*STOP) THEN
            IF (NIN .GT. 0) THEN
               DAT(2*ICNT - 1) = DAT(2*ICNT - 1) / NIN
               DAT(2*ICNT) = DAT(2*ICNT) / NIN
               END IF
            NIN = 0
            ICNT = ICNT + 1
            START = STOP
            STOP = START + DARGM
            GO TO 20
            END IF
         NIN = NIN + 1
         DAT(2*ICNT - 1) = DAT(2*ICNT - 1) + DATA(2*I - 1)
         DAT(2*ICNT) = DAT(2*ICNT) + DATA(2*I)
         IF (I .EQ. NNNIN) THEN
            DAT(2*ICNT - 1) = DAT(2*ICNT - 1) / NIN
            DAT(2*ICNT) = DAT(2*ICNT) / NIN
            END IF
   30    CONTINUE
C
      NNN = ICNT
      ISIGN = -1
      NNN2 = 2*NNN
C                                       prepare DATA for FFT with
C                                       zero padding
      DO 40 I = 1, NNN
         DATAIN(2*I-1) = DAT(2*I-1)
         DATAIN(2*I) = DAT(2*I)
         DAT(2*(I+NNN)-1) = 0.0
         DAT(2*(I+NNN)) = 0.0
  40     CONTINUE
C                                       FFT
      CALL FOURG (DAT, NNN2, ISIGN, WORK)
C                                       find max of amplitude
      AMPP = DAT(1)*DAT(1) + DAT(2)*DAT(2)
      DO 50 I = 1, NNN2
         TAMP = DAT(2*I-1)*DAT(2*I-1) + DAT(2*I)*DAT(2*I)
         IF (TAMP .GE. AMPP) THEN
            AMPP = TAMP
            RATE = (I-1.0) / (2.0*NNN)
            IF (I .GT. NNN) RATE = RATE - 1.0
            FI = ATAN2 (DAT(2*I), DAT(2*I-1))
            END IF
   50    CONTINUE
      AMPP = SQRT(AMPP) / NNN
C                                       RATE is in parts of window
      RATE = RATE*2.0*NNN
C --------------------------------------------------------------------
C                   L E A S T    S Q U A R E
C
C        precise AMP, RATE and FI using non linear least square
C              fitting a  complex exponent to the given data.
C                We have NNN2 = 2*NNN points of data
C             (real and image for each point of data)
C         We have 3 unknown magnitudes: ampl., rate and phase
C --------------------------------------------------------------------
      TWOPP = TWOPI / (2.0*NNN)
      ITER = 1
  100 CONTINUE
C                                       Force result vector R(3),
C                                       matrix M(3*3) to zero
      DO 120 I = 1, 3
         R(I) = 0.0
         DO 110 K = 1, 3
            IK = K + (I - 1)*3
            MATR (IK) = 0.0
 110        CONTINUE
 120     CONTINUE
      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0
C                                       Prepare result vector R(3)
C                                       and matrix MATR(3*3)
C                                       for routine LEASQR

      DO 140 I = 1, NNN
C                                       take only the points.NE.0
         IF ((DATAIN(2*I-1).EQ.0.0) .AND.
     *      (DATAIN(2*I).EQ.0.0)) GOTO 140
         NOBS = NOBS + 1
         DRE = DATAIN (2*I -1)
         DIM = DATAIN (2*I)
         TWOP = TWOPP * (I-1)
C                                       difference of DATA and model
         ARG = TWOP * RATE + FI
         SINK = SIN(ARG)
         COSK = COS(ARG)
         DRE = DRE - AMPP * COSK
         DIM = DIM - AMPP * SINK
         SUM = SUM + DRE + DIM
         SSQ = SSQ + DRE*DRE + DIM*DIM
C
C                                       1-ampl; 2-rate; 3-fi.
         R(1) =  R(1) + DRE * COSK
     *      + DIM * SINK
         R(2) =  R(2) - DRE * AMPP * SINK * TWOP
     *      + DIM * AMPP * COSK * TWOP
         R(3) =  R(3) - DRE * AMPP*SINK
     *      + DIM * AMPP*COSK
C                                       calculate upper/right
C                                       triangle of MATR
         MATR(1) = MATR(1) + COSK*COSK + SINK*SINK
C         MATR(2) = MATR(2) - COSK*AMPP*SINK*TWOP
C     *      + SINK*AMPP*COSK*TWOP
C         MATR(3) = MATR(3) - COSK*AMPP*SINK + SINK*AMPP*COSK
         MATR(4) = MATR(4) - AMPP*SINK*TWOP*COSK
     *      + AMPP*COSK*TWOP*SINK
         MATR(5) = MATR(5) + AMPP*SINK*TWOP*AMPP*SINK*TWOP
     *      + AMPP*COSK*TWOP*AMPP*COSK*TWOP
C         MATR(6) = MATR(6) + AMPP*SINK*TWOP*AMPP*SINK
C     *      + AMPP*COSK*TWOP*AMPP*COSK
         MATR(7) = MATR(7) - AMPP*SINK*COSK + AMPP*COSK*SINK
         MATR(8) = MATR(8) + AMPP*SINK*AMPP*SINK*TWOP
     *      + AMPP*COSK*AMPP*COSK*TWOP
         MATR(9) = MATR(9) + AMPP*SINK*AMPP*SINK
     *      + AMPP*COSK*AMPP*COSK
 140     CONTINUE
      NOBS = 2*NOBS
C                                       NOBS need to be real for LEASQR
      CALL LEASQR (3, NOBS, SUM, SSQ, R, MATR, SOL, VX, SSQRES,
     *   VARRES, VARY, FIT, LESOL)
C                                       find the solution
      AMPP = AMPP + SOL(1)
      RATE = RATE + SOL(2)
      FI = FI + SOL(3)
      ITER = ITER + 1
C      IF (ITER .LE. 2) GO TO 100
      IF (ITER .LE. 4) GO TO 100
      DDAMP = SQRT (VX(1))
      DDRATE = SQRT (VX(2))
      DDFI = SQRT (VX(3))
C
      RATE = RATE /(NNN2*DARGM)
C-----------------------------------------------------------------------
C
 999  RETURN
C-----------------------------------------------------------------------
C1000 FORMAT ('AFRF: The number of points ',I6,' is more MAX=1024')
      END
      SUBROUTINE DHMS (TIME, DAY, H, M, SEC)
C-----------------------------------------------------------------------
C  Subtract DAY, HOUR, MINUTE, SECOND from TIME in DAYs
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   Inputs:
C      TIME     R             Time in days
C   Outputs
C      DAY      I             Day of the time
C      H        I             Hour
C      M        I             Minute
C      S        R             Second
C-----------------------------------------------------------------------
      INTEGER DAY, H, M
      REAL TIME, SEC, HR, MR
C-----------------------------------------------------------------------
      DAY = TIME
      HR = (TIME - DAY) * 24.0
      H = HR
      MR = (HR - H) * 60
      M = MR
      SEC = (MR - M) * 60
C
 999  RETURN
      END




