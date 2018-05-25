LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=36)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'SOURCES',
     *   'QUAL', 'CALCODE', 'TIMERANG', 'SELBAND', 'SELFREQ', 'FREQID',
     *   'BIF', 'EIF', 'BCHAN', 'ECHAN', 'SUBARRAY', 'DOCALIB',
     *   'GAINUSE', 'DOPOL', 'PDVER', 'BLVER', 'FLAGVER', 'DOBAND',
     *   'BPVER', 'SMOOTH', 'UVRANGE', 'SOLINT', 'SCANLENG', 'REFANT',
     *   'OPTYPE', 'DOROBUST', 'DOCAT', 'DOCRT', 'OUTPRINT', 'PRTLEV',
     *    'BADDISK'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR,
     *   OOAINT, OOACAR, OOARE, OOARE, OOARE, OOAINT,
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOARE,
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOAINT,
     *   OOAINT, OOARE, OOARE, OOARE, OOARE, OOAINT,
     *   OOACAR, OOALOG, OOALOG, OOAINT, OOACAR, OOAINT,
     *   OOAINT/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 16,30,
     *   1,1, 4,1, 8,1, 1,1, 1,1, 1,1,
     *   1,1, 1,1, 1,1, 1,1, 1,1, 1,1,
     *   1,1, 1,1, 1,1, 1,1, 1,1, 1,1,
     *   1,1, 3,1, 2,1, 1,1, 1,1, 1,1,
     *   4,1, 1,1, 1,1, 1,1, 48,1, 1,1,
     *   10,1/
LOCAL END
LOCAL INCLUDE 'OUTPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NPARMO
      PARAMETER (NPARMO=1)
      CHARACTER AVONAM(NPARMO)*8
LOCAL END
LOCAL INCLUDE 'OUTPUTDATA.INC'
      DATA AVONAM /'ARRAY2'/
LOCAL END
LOCAL INCLUDE 'XXTAB.INC'
C                                       Column indices
C                                         (1) time1
C                                         (2) time2
C                                         (3) Source
C                                         (4) NSAMP Vector amplitudes
C                                         (5) NSAMP vector sigmas
C                                         (6) NSAMP Scalar amplitudes
C                                         (7) NSAMP vector sigmas
C                                         (8) NSAMP weights
C                                         (9) NSAMP closure failures
C                                       NSAMP = Cant * Cif * Cpol
C                                       Table label, column labels
      INTEGER NCOL
      PARAMETER (NCOL = 9)
C
      INTEGER   COLIDX(NCOL), NSAMP, NCORS, XXTYPE
C
      COMMON /TABXX/ COLIDX, NSAMP, NCORS, XXTYPE
LOCAL END
      PROGRAM FINDR
C-----------------------------------------------------------------------
C! Find normal values to guide FLAGR
C# Task OOP calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2004-2007, 2009-2011, 2015
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
C   Determines normal data amplitudes and rms's.
C   This task takes the rms in the Real part and the rms in the
C   Imaginary part as two samples of the Amplitude rms.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UVIN*32, PRINT*32, INPUT*32, OUTPUT*32
      INTEGER   IRET, IERR
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'OUTPUT.INC'
      INCLUDE 'OUTPUTDATA.INC'
      DATA PRGM /'FINDR '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL FINDRI (PRGM, INPUT, UVIN, PRINT, OUTPUT, IRET)
C                                       Check interference
      IF (IRET.LE.0) CALL FINDRO (INPUT, UVIN, PRINT, OUTPUT, IRET)
C                                       Return adverbs if RQUICK
      CALL OUTQAV (IRET, NPARMO, AVONAM, OUTPUT, IERR)
C                                       Return adverbs if .NOT.RQUICK
C                                       Close down files, etc.
      CALL OUT2AV (IRET, NPARMO, AVONAM, OUTPUT, IERR)
C
 999  STOP
      END
      SUBROUTINE FINDRI (PRGN, INPUT, UVIN, PRINT, OUTPUT, IERR)
C-----------------------------------------------------------------------
C   FINDRI gets input parameters for FINDR and creates the input uvdata
C   object.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      INPUT   C*?   Task inputs object
C      UVIN    C*?   Input multisource uv data object.
C      PRINT   C*?   Print object
C      OUTPUT  C*?   Output adverb object
C      IERR    I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER PRGN*(*), INPUT*(*), UVIN*(*), PRINT*(*), OUTPUT*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INTEGER   NKEY, NKEY2
C                                       NKEY=no. adverbs to copy to
C                                       UVIN
      PARAMETER (NKEY=30)
C                                       NKEY2=no. adverbs for PRINT
      PARAMETER (NKEY2=2)
      INTEGER   DIM(7), TYPE, FGVER, DUMMY, DISKI, CNOI, FGV, DOCRT,
     *   AVODIM(7)
      REAL      SOLINT, XDOCAL, ZERO(40)
      LOGICAL   LTEMP
      CHARACTER INK(NKEY)*8, OUTK(NKEY)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, STOKES*4, CDUMMY*1, FGIN*32
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'OUTPUT.INC'
      INCLUDE 'OUTPUTDATA.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for UVIN
      DATA INK /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'GAINUSE', 'DOPOL', 'BLVER', 'DOBAND', 'BPVER', 'SMOOTH',
     *   'FLAGVER',  'TIMERANG', 'UVRANGE', 'SOURCES', 'QUAL',
     *   'SELBAND', 'SELFREQ', 'FREQID', 'CALCODE', 'SUBARRAY',
     *   'BIF', 'EIF', 'BCHAN', 'ECHAN', 'PRTLEV', 'DOCAT',
     *   'SCANLENG', 'REFANT', 'PDVER', 'DOROBUST'/
      DATA OUTK /'NAME', 'CLASS', 'IMSEQ', 'DISK',
     *   'CALEDIT.CLUSE', 'CALEDIT.DOPOL', 'CALEDIT.BLVER',
     *   'CALEDIT.DOBAND', 'CALEDIT.BPVER', 'CALEDIT.SMOOTH',
     *   'CALEDIT.FGVER', 'CALEDIT.TIMRNG', 'CALEDIT.UVRNG',
     *   'CALEDIT.SOURCS', 'CALEDIT.SELQUA', 'CALEDIT.SELBAN',
     *   'CALEDIT.SELFRQ', 'CALEDIT.FRQSEL', 'CALEDIT.SELCOD',
     *   'CALEDIT.SUBARR', 'CALEDIT.BIF', 'CALEDIT.EIF',
     *   'CALEDIT.BCHAN', 'CALEDIT.ECHAN', 'PRTLEV', 'DOCAT',
     *   'SCANLENG', 'REFANT', 'CALEDIT.PDVER', 'DOROBUST'/
C                                       Adverbs for PRINT
      DATA INK2  /'DOCRT', 'OUTPRINT'/
      DATA OUTK2 /'DOCRT', 'LPFILE'/
      DATA ZERO /40 * 0.0/
C-----------------------------------------------------------------------
C                                       Startup - not resume AIPS
      INPUT = 'Input'
      CALL AV2INT (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, INPUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET (INPUT, 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INPUT, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create UVIN
      UVIN = 'Input UVdata'
      CALL CREATE (UVIN, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ (INPUT, NKEY, INK, OUTK, UVIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       DOCALIB
      CALL OGET ('Input', 'DOCALIB', TYPE, DIM, XDOCAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      LTEMP = XDOCAL.GT.0.0
      CALL OPUT (UVIN, 'CALEDIT.DOCAL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      LTEMP = (XDOCAL.GT.0.0) .AND. (XDOCAL.LE.99.0)
      CALL OPUT (UVIN, 'CALEDIT.DOWTCL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Select all Stokes'
      STOKES = ' '
      DIM(1) = LEN (STOKES)
      DIM(2) = 1
      CALL OPUT (UVIN, 'CALEDIT.STOKES', OOACAR, DIM, DUMMY, STOKES,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open UVIN to be sure it's OK.
      CALL OOPEN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBDSKC (UVIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (UVIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default FLAGVER
      CALL OGET (INPUT, 'FLAGVER', TYPE, DIM, FGVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find highest FG table.
      FGIN = 'Temporary FG table for FINDRI'
      FGV = 1
      CALL UV2TAB (UVIN, FGIN, 'FG', FGV, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TBLHIV (FGIN, FGV, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       use highest for in
      IF (FGVER.EQ.0) FGVER = FGV
      IF (FGVER.GT.FGV) FGVER = FGV
      CALL OPUT (INPUT, 'FLAGVER', OOAINT, DIM, FGVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVIN, 'CALEDIT.FGVER', OOAINT, DIM, FGVER, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default SOLINT (60 sec)
      CALL OGET (INPUT, 'SOLINT', TYPE, DIM, SOLINT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (SOLINT.LE.0.0) SOLINT = 60.0
      CALL OPUT (INPUT, 'SOLINT', OOARE, DIM, SOLINT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVIN, 'TIMEAVG', OOARE, DIM, SOLINT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDES (FGIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Printer object
      IF (DOCRT.NE.0) THEN
         PRINT = 'Printer object'
         CALL PRTCRE (PRINT, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
         CALL IN2OBJ (INPUT, NKEY2, INK2, OUTK2, PRINT, IERR)
         IF (IERR.NE.0) GO TO 999
         DIM(1) = LEN (UVIN)
         DIM(2) = 1
         CALL PRTPUT (PRINT, 'PRINTOBJ', OOACAR, DIM, DUMMY, UVIN,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         PRINT = ' '
         END IF
C                                       Output object
      OUTPUT = 'Output adverbs'
      CALL OBCREA (OUTPUT, 'INPUTS', IERR)
      IF (IERR.NE.0) GO TO 999
      AVODIM(1) = 20
      AVODIM(2) = 2
      AVODIM(3) = 1
      CALL OPUT (OUTPUT, AVONAM(1), OOARE, AVODIM, ZERO, CDUMMY, IERR)
C
 999  RETURN
      END
      SUBROUTINE FINDRO (INPUT, UVIN, PRINT, OUTPUT, IERR)
C-----------------------------------------------------------------------
C   Process data: allocates memory, calls various operations
C   Inputs:
C      INPUT   C*?   Task input object.
C      UVIN    C*?   Input multisource uv object
C      PRINT   C*?   Print object
C      OUTPUT  C*?   output object
C   Output:
C      IERR    I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INPUT*(*), UVIN*(*), PRINT*(*), OUTPUT*(*)
      INTEGER   IERR
C
      INTEGER   TDIM
      PARAMETER (TDIM = 100)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TYPE, DIM(7), DUMMY, CCHAN, CIF, CPOL, NAXIS(7), INDEX,
     *   CANT, IS1, IS2, ANVER, MXA, COUNT(MAXANT,MAXANT,MAXIF,2),
     *   NTIME, IDUMMY, NV, COUNTA(MAXANT,MAXANT,MAXIF,2),
     *   THEIUF(28*MAXANT*MAXANT*MAXIF)
      LOGICAL   EXISTS
      CHARACTER OPTYPE*4, CDUMMY*1, TABLE*32
      DOUBLE PRECISION SUMV(MAXANT,MAXANT,MAXIF,2),
     *   SUMVS(MAXANT,MAXANT,MAXIF,2), SUMS(MAXANT,MAXANT,MAXIF,2),
     *   SUMSS(MAXANT,MAXANT,MAXIF,2), SUMWT(MAXANT,MAXANT,MAXIF,2),
     *   SUMWTA(MAXANT,MAXANT,MAXIF,2)
      REAL      ANTD(MAXANT,MAXIF,2,6), THEBUF(28*MAXANT*MAXANT*MAXIF),
     *   RUMV(MAXANT,MAXANT,MAXIF,4), RUMVS(MAXANT,MAXANT,MAXIF,4),
     *   RUMS(MAXANT,MAXANT,MAXIF,4), RUMSS(MAXANT,MAXANT,MAXIF,4),
     *   RUMWT(MAXANT,MAXANT,MAXIF,4), RUMWTA(MAXANT,MAXANT,MAXIF,4)
      EQUIVALENCE (THEBUF, THEIUF, SUMV, RUMV), (SUMVS, RUMVS),
     *   (SUMS, RUMS), (SUMSS, RUMSS), (SUMWT, RUMWT), (SUMWTA, RUMWTA)
      COMMON /SUMBUF/ SUMV, SUMVS, SUMS, SUMSS, SUMWT, SUMWTA, COUNT,
     *   COUNTA
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'XXTAB.INC'
C-----------------------------------------------------------------------
C                                       opcode
      CALL OGET (INPUT, 'OPTYPE', TYPE, DIM, DUMMY, OPTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       data parameters
      CALL UVDGET (UVIN, 'NAXIS', TYPE, DIM, NAXIS, DUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVDFND (UVIN, 2, 'STOKES', INDEX, IERR)
      IF (IERR.NE.0) GO TO 999
      CPOL = NAXIS(INDEX)
      CPOL = MIN (2, CPOL)
      CALL UVDFND (UVIN, 2, 'FREQ', INDEX, IERR)
      IF (IERR.NE.0) GO TO 999
      CCHAN = NAXIS(INDEX)
      CALL UVDFND (UVIN, 2, 'IF', INDEX, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.1)) GO TO 999
      CIF = 1
      IF (IERR.EQ.0) CIF = NAXIS(INDEX)
C                                       find maximum antenna number
      TABLE = 'Temporary AN table for FINDR'
      CANT = 0
      CALL OUVGET (UVIN, 'CALEDIT.SUBARR', TYPE, DIM, ANVER, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      IF (ANVER.GT.0) THEN
         IS1 = ANVER
         IS2 = ANVER
      ELSE
         IS1 = 1
         IS2 = 99
         END IF
      DO 20 ANVER = 1,99
         CALL UV2TAB (UVIN, TABLE, 'AN', ANVER, IERR)
         EXISTS = .FALSE.
         IF (IERR.EQ.0) CALL TABEXI (TABLE, EXISTS, IERR)
         MXA = 0
         IF ((IERR.EQ.0) .AND. (EXISTS)) CALL ANTNO (TABLE, ANVER, MXA,
     *      IERR)
         CALL TABDES (TABLE, IDUMMY)
         IF ((IERR.EQ.0) .AND. (EXISTS)) THEN
            CANT = MAX (CANT, MXA)
         ELSE
            GO TO 30
            END IF
 20      CONTINUE
 30   IF (CANT.LE.0) CANT = MAXANT
C
      NSAMP = CANT * CIF * CPOL
      NCORS = CIF * CPOL
C                                       Vector diff - antenna
      IF (OPTYPE.EQ.'VDIF') THEN
         XXTYPE = 2
         NTIME = (14 * 2 * MAXIF * MAXANT * MAXANT) / (5*NSAMP + NCORS)
         NV = NSAMP * NTIME
         CALL AMVDIF (UVIN, PRINT, OUTPUT, CANT, CIF, CPOL, CCHAN,
     *      NTIME, RUMV, RUMWT, COUNT, RUMS, ANTD, THEBUF(1),
     *      THEBUF(NV+1), THEBUF(2*NV+1), THEIUF(5*NV+1),
     *      THEBUF(4*NV+1), THEBUF(3*NV+1), IERR)
C                                       Vector diff - baseline
      ELSE IF (OPTYPE.EQ.'VRFI') THEN
         NSAMP = CANT * NSAMP
         NTIME = (14 * 2 * MAXIF * MAXANT * MAXANT - NSAMP) / (3*NSAMP)
         NV = NSAMP * NTIME
         CALL AMVRFI (UVIN, PRINT, OUTPUT, CANT, CIF, CPOL, CCHAN,
     *      NTIME, THEBUF(1), THEBUF(NV+1), THEBUF(2*NV+1),
     *      THEBUF, THEBUF(3*NV+1), IERR)
C                                       the rest
      ELSE
         XXTYPE = 1
         NTIME = (14 * 2 * MAXIF * MAXANT * MAXANT) / (6 * NSAMP)
         CALL ANTIME (UVIN, PRINT, OUTPUT, CANT, CIF, CPOL, CCHAN,
     *      NTIME, OPTYPE, SUMV, SUMVS, SUMS, SUMSS, SUMWT, COUNT,
     *      SUMWTA, COUNTA, ANTD, THEBUF, IERR)
         END IF
C                                       Error
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR FINDING RMS IN ' // UVIN
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
      END
      SUBROUTINE ANTIME (UVIN, PRINT, OUTPUT, CANT, CIF, CPOL, CCHAN,
     *   CTIME, OPTYPE, SUMV, SUMVS, SUMS, SUMSS, SUMWT, COUNT, SUMWTA,
     *   COUNTA, ANTD, TD, IERR)
C-----------------------------------------------------------------------
C   Look for interference based on a high real or imaginary rms in time
C   interval TIMEAVG.
C   Inputs:
C      UVIN      C*?   Name of input uvdata object.
C      PRINT     C*?   Print object
C      OUTPUT    C*?   Output object
C      CANT      I     Maximum antenna number
C      CPOL      I     Number of polarizations
C      CIF       I     Number of IFs
C      CCHAN     I     Number of spactral channels
C      CTIME     I     Limit on number of times
C      OPTYPE    C*4   'GAIN' convert all amps to gains and do all
C                      sources together
C   Scratch variables
C      SUM, SUMSQ, SUMWT, COUNT, AMPS, RMSS
C      Note AMPS cannot be used when SUMV, SUMVS, SUMS, SUMSS, SUMWT,
C      COUNT are being used
C   Inputs attached to UVIN:
C      MAXRMS    R(*)  Maximum RMS allowed, constant plus amplitude
C                      coefficient.
C      MAXBAD    R(2)  Maximum allowed fraction of bad baselines.
C      TIMEAVG   R     Time in seconds for clipping interval
C      DOIFS     L     Flag all IFs if one bad
C      DOSTOKES  L     Flas all Stokes if 1 bad
C      KILLONES  L     Kill 1-sample integrations
C      PRTLEV    I     Print level for debugging
C   Output:
C      IERR      I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), PRINT*(*), OUTPUT*(*), OPTYPE*4
      INTEGER   CANT, CIF, CPOL, CCHAN, CTIME, IERR
      INTEGER   COUNT(CANT,CANT,CIF,CPOL), COUNTA(CANT,CANT,CIF,CPOL)
      DOUBLE PRECISION SUMV(CANT,CANT,CIF,CPOL),
     *   SUMVS(CANT,CANT,CIF,CPOL), SUMS(CANT,CANT,CIF,CPOL),
     *   SUMSS(CANT,CANT,CIF,CPOL), SUMWT(CANT,CANT,CIF,CPOL),
     *   SUMWTA(CANT,CANT,CIF,CPOL)
      REAL      ANTD(CANT,CIF,CPOL,6), TD(CANT,CIF,CPOL,6,*)
C
      REAL      TCLIP
      LOGICAL   DOCAT
      INTEGER   PRTLEV
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NSORC, NTIME, LMAX
      PARAMETER (NSORC = 5000, NTIME = 15001)
      PARAMETER (LMAX = ((MAXANT) * (MAXANT)))
C
      INTEGER   TYPE, DIM(7), DUMMY, ANT1, ANT2, ENDVIS, VISNO, CURSOU,
     *   THISOU, TBVER, DOCRT, I, J, LS, XS, NACROS, ILOCU, ILOCV, IT,
     *   ILOCW, ILOCT, ILOCB, K,ILOCSU, ILOCFQ, ILOCA1, ILOCA2, ILOCSA,
     *   JLOCC, JLOCS, JLOCF, JLOCR, JLOCD, JLOCIF, LA,INCS, INCF,
     *   INCIF, LP, LIF, IP, NPRINT, THISUB, THIFRQ, CURSUB, CURFRQ,
     *   NZERO, XXROW, LT, II, SCNT(NSORC), SNUMS(NTIME), MTIME,
     *   SINUM(NSORC), SCOUNT, BADANT(MAXANT), QUAL(NSORC), NOFIVE(5),
     *   NOFTEN(5), ITRIM, AVODIM(3), JJ, KK, NPP(2)
      LOGICAL   DONE, DOSOU, QUIT, DOCAL, DOROBU
      CHARACTER SUTAB*32, SUNAME(NSORC)*16, LINE*132, CDUMMY*1,
     *   XXFILE*32, DTYPE(5)*10
      REAL      AMP, RMS,  RP(50), TEPS, W, CURTIM, LSTIME, ENDTIM,
     *   BEGTIM, VIS(3,MAXCIF), SASUM(NSORC), SRSUM(NSORC), DV(5),
     *   SAASUM(NSORC), SRRSUM(NSORC), TIMR(2,NTIME), RV(5),
     *   PP(2,MAXIF,MAXANT,5), PR(2,MAXIF,MAXANT,5), NOVER(12),
     *   ARRAY2(20,2), LIST(2*MAXANT*MAXIF), FRAC(MAXANT,MAXIF,2),
     *   FRACNT(12), FRATOT, TDUMMY(NTIME,4), LSUMV(LMAX), LSUMVS(LMAX),
     *   LSUMS(LMAX), LSUMSS(LMAX), LSUMWT(LMAX), WP(2), SP(2), SPP(2)
      EQUIVALENCE (ARRAY2(1,1), DV),  (ARRAY2(6,1), RV),
     *   (ARRAY2(1,2), NOVER)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /TBUFS/ TIMR, SNUMS, TDUMMY
      INCLUDE 'OUTPUT.INC'
      INCLUDE 'OUTPUTDATA.INC'
      DATA DTYPE /'Vector amp', 'Vector rms', 'Scalar amp',
     *   'Scalar rms', 'Weight'/
C-----------------------------------------------------------------------
      XXFILE = ' '
      TEPS = 0.02 / (3600.0 * 24.0)
      CALL FILL (NSORC, 0, SCNT)
      CALL RFILL (NSORC, 0.0, SASUM)
      CALL RFILL (NSORC, 0.0, SRSUM)
      CALL RFILL (NSORC, 0.0, SAASUM)
      CALL RFILL (NSORC, 0.0, SRRSUM)
      CALL RFILL (40, 0.0, ARRAY2)
      SCOUNT = 0
C                                       Open printer
      NPRINT = 0
      IF (PRINT.NE.' ') THEN
         CALL OOPEN (PRINT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'NACROS', TYPE, DIM, NACROS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         DOCRT = 0
         END IF
      THISOU = 0
      THISUB = 0
      THIFRQ = 0
      CURSUB = 0
      CURFRQ = 0
C                                       Create and open a scratch table:
      XXFILE = 'Temporary results table'
      CALL UV2TAB (UVIN, XXFILE, 'XX', 1, IERR)
      IF (IERR.NE.0) THEN
         XXFILE = ' '
         GO TO 990
         END IF
      CALL OXXINI (XXFILE, 'WRIT', XXROW, IERR)
      IF (IERR.NE.0) THEN
         XXFILE = ' '
         GO TO 990
         END IF
C                                       Retrieve pointers into the data:
C                                       pointers into the data:
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 999
      INCS = INCS / 3
      INCF = INCF / 3
      INCIF = INCIF / 3
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get parameters=
      CALL OUVGET (UVIN, 'DOROBUST', TYPE, DIM, DOROBU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'DOCAT', TYPE, DIM, DOCAT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'TIMEAVG', TYPE, DIM, TCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      TCLIP = TCLIP / 86400.0
C                                       Processing info
      CALL OUVGET (UVIN, 'CALEDIT.DOCAL', TYPE, DIM, DOCAL, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      DOSOU = (ILOCSU.GT.0)
      IERR = 0
C                                       Source table/name
      IF (DOSOU) THEN
         SUTAB = 'SoUrce table for ANTIME'
         TBVER = 1
         CALL UV2TAB (UVIN, SUTAB, 'SU', TBVER, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       For single source use 'OBJECT'
      ELSE
         CALL UVDGET (UVIN, 'OBJECT', TYPE, DIM, DUMMY, SUNAME, IERR)
         IF (IERR.NE.0) GO TO 990
         QUAL(1) = 0
         END IF
C                                       Clear accumulators:
C                                       (1,*) =  count
C                                       (2,*) =  sum r then max rms**2
C                                       (3,*) =  sum**2 r then rms**2
C                                       (4,*) =  sum imaginary
C                                       (5,*) =  sum**2 imaginary
C                                       (6,*) =  sum amplitude
      NZERO = CANT * CANT * CIF * CPOL
      CALL DFILL (NZERO, 0.0D0, SUMV)
      CALL DFILL (NZERO, 0.0D0, SUMVS)
      CALL DFILL (NZERO, 0.0D0, SUMS)
      CALL DFILL (NZERO, 0.0D0, SUMSS)
      CALL DFILL (NZERO, 0.0D0, SUMWT)
      CALL DFILL (NZERO, 0.0D0, SUMWTA)
      CALL FILL (NZERO, 0, COUNT)
      CALL FILL (NZERO, 0, COUNTA)
C                                       Initialize visibility count
      DONE = .FALSE.
      VISNO = 0
C                                       Swallow input collecting
C                                       statistics.
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VIS, IERR)
         IF (IERR.LT.0) THEN
            DONE = .TRUE.
            IERR = 0
            END IF
         IF (IERR.GT.0) GO TO 990
         VISNO = VISNO + 1
 110     CURTIM = RP(ILOCT)
         IF (DOSOU) THEN
            CURSOU = RP(ILOCSU) + 0.5
         ELSE
            CURSOU = 0
            END IF
         IF (ILOCFQ.GT.0) CURFRQ = RP(ILOCFQ) + 0.5
C                                       Get antenna numbers
         IF (ILOCB.GT.0) THEN
            ANT1 = (RP(ILOCB) / 256.0) + 0.001
            ANT2 = (RP(ILOCB) - ANT1 * 256) + 0.001
            CURSUB = (RP(ILOCB) - 256 * ANT1 - ANT2) * 100.0 + 1.1
         ELSE
            ANT1 = RP(ILOCA1) + 0.001
            ANT2 = RP(ILOCA2) + 0.001
            CURSUB = RP(ILOCSA) + 0.001
            END IF
C                                       Initial selection.
         IF (VISNO.EQ.1) THEN
            THIFRQ = CURFRQ
            THISUB = CURSUB
            THISOU = CURSOU
            BEGTIM = CURTIM
            ENDTIM = CURTIM + TCLIP
            END IF
C                                       Finished with data or interval?
         IF (DONE .OR. (CURTIM.GT.ENDTIM) .OR. (CURSOU.NE.THISOU)
     *      .OR. (CURSUB.NE.THISUB)) GO TO 150
C                                       Set last vis of interval
         ENDVIS = VISNO
C                                       Set last time
         LSTIME = CURTIM
C                                       Baseline index
         DO 130 LP = 1,CPOL
            DO 125 LIF = 1,CIF
               IP = (LP-1) * INCS + (LIF-1) * INCIF + 1
C                                       amplitude
               CALL AVERAG (CCHAN, VIS(1,IP), INCF, ANT1, ANT2, .TRUE.,
     *            DOROBU, NPP, WP, SP, SPP)
               COUNTA(ANT1,ANT2,LIF,LP) = COUNTA(ANT1,ANT2,LIF,LP) +
     *            NPP(1)
               SUMWTA(ANT1,ANT2,LIF,LP) = SUMWTA(ANT1,ANT2,LIF,LP) +
     *            WP(1)
               SUMS(ANT1,ANT2,LIF,LP) = SUMS(ANT1,ANT2,LIF,LP) +
     *            SP(1)
               SUMSS(ANT1,ANT2,LIF,LP) = SUMSS(ANT1,ANT2,LIF,LP) +
     *            SPP(1)
C                                       real & imaginary
               CALL AVERAG (CCHAN, VIS(1,IP), INCF, ANT1, ANT2, .FALSE.,
     *            DOROBU, NPP, WP, SP, SPP)
               COUNT(ANT1,ANT2,LIF,LP) = COUNT(ANT1,ANT2,LIF,LP) +
     *            NPP(1)
               SUMWT(ANT1,ANT2,LIF,LP) = SUMWT(ANT1,ANT2,LIF,LP) +
     *            WP(1)
               SUMV(ANT1,ANT2,LIF,LP) = SUMV(ANT1,ANT2,LIF,LP) +
     *            SP(1)
               SUMVS(ANT1,ANT2,LIF,LP) = SUMVS(ANT1,ANT2,LIF,LP) +
     *            SPP(1)
               COUNT(ANT2,ANT1,LIF,LP) = COUNT(ANT2,ANT1,LIF,LP) +
     *            NPP(2)
               SUMWT(ANT2,ANT1,LIF,LP) = SUMWT(ANT2,ANT1,LIF,LP) +
     *            WP(2)
               SUMV(ANT2,ANT1,LIF,LP) = SUMV(ANT2,ANT1,LIF,LP) +
     *            SP(2)
               SUMVS(ANT2,ANT1,LIF,LP) = SUMVS(ANT2,ANT1,LIF,LP) +
     *            SPP(2)
 125           CONTINUE
 130        CONTINUE
C                                       Next vis until done
         IF (.NOT.DONE) GO TO 100
 150     IERR = 0
C                                       Get statistics
       DO 170 LIF = 1,CIF
          DO 165 LP = 1,CPOL
             CALL FILL (CANT, 0, BADANT)
             KK = 0
             DO 160 ANT2 = 1,CANT
               DO 155 ANT1 = 1,CANT
                  KK = KK + 1
                  IF (SUMWT(ANT1,ANT2,LIF,LP).GT.0.0) THEN
                     SUMV(ANT1,ANT2,LIF,LP) = (SUMV(ANT1,ANT2,LIF,LP)
     *                  / SUMWT(ANT1,ANT2,LIF,LP)) ** 2
                     SUMVS(ANT1,ANT2,LIF,LP) = SUMVS(ANT1,ANT2,LIF,LP)
     *                  / SUMWT(ANT1,ANT2,LIF,LP)
                     END IF
                  IF (SUMWTA(ANT1,ANT2,LIF,LP).GT.0.0) THEN
                     SUMS(ANT1,ANT2,LIF,LP) = (SUMS(ANT1,ANT2,LIF,LP)
     *                  / SUMWTA(ANT1,ANT2,LIF,LP)) ** 2
                     SUMSS(ANT1,ANT2,LIF,LP) = SUMSS(ANT1,ANT2,LIF,LP)
     *                  / SUMWTA(ANT1,ANT2,LIF,LP)
                     END IF
                  IF (COUNT(ANT1,ANT2,LIF,LP).GT.1) THEN
                     RMS = SUMVS(ANT1,ANT2,LIF,LP) -
     *                  SUMV(ANT1,ANT2,LIF,LP)
                     RMS = (RMS * COUNT(ANT1,ANT2,LIF,LP)) /
     *                  (COUNT(ANT1,ANT2,LIF,LP) - 1.0)
                     RMS = ABS (RMS)
                     LSUMV(KK) = SUMV(ANT1,ANT2,LIF,LP)
                     LSUMVS(KK) = RMS
                     LSUMWT(KK) = (SUMWT(ANT1,ANT2,LIF,LP)
     *                  / COUNT(ANT1,ANT2,LIF,LP)) ** 2
                  ELSE
                     LSUMVS(KK) = FBLANK
                     LSUMV(KK) = FBLANK
                     LSUMWT(KK) = FBLANK
                     END IF
                  IF (COUNTA(ANT1,ANT2,LIF,LP).GT.1) THEN
                     IF (ANT1.LE.ANT2) THEN
                        RMS = SUMSS(ANT1,ANT2,LIF,LP) -
     *                     SUMS(ANT1,ANT2,LIF,LP)
                        RMS = (RMS * COUNTA(ANT1,ANT2,LIF,LP)) /
     *                     (COUNTA(ANT1,ANT2,LIF,LP) - 1.0)
                        RMS = ABS (RMS)
                        LSUMS(KK) = SUMS(ANT1,ANT2,LIF,LP)
                        LSUMSS(KK) = RMS
                     ELSE
                        LSUMSS(KK) = FBLANK
                        LSUMS(KK) = FBLANK
                        END IF
                  ELSE
                     LSUMSS(KK) = FBLANK
                     LSUMS(KK) = FBLANK
                     END IF
                  BADANT(ANT1) = MAX (BADANT(ANT1),
     *               COUNT(ANT1,ANT2,LIF,LP))
                  BADANT(ANT2) = MAX (BADANT(ANT2),
     *               COUNT(ANT1,ANT2,LIF,LP))
 155              CONTINUE
 160           CONTINUE
C                                       solve for antenna amplitude
            CALL ASOLVE (PRTLEV, 'amp', BEGTIM, LSTIME, CANT, 3.0,
     *         LSUMV, ANTD(1,LIF,LP,1), ANTD(1,LIF,LP,6))
            CALL ASOLVE (PRTLEV, 'amp', BEGTIM, LSTIME, CANT, 3.0,
     *         LSUMS, ANTD(1,LIF,LP,3), FRAC)
C                                       solve for antenna RMS
            CALL ASOLVE (PRTLEV, 'rms', BEGTIM, LSTIME, CANT, 3.0,
     *         LSUMVS, ANTD(1,LIF,LP,2), FRAC)
            CALL ASOLVE (PRTLEV, 'rms', BEGTIM, LSTIME, CANT, 3.0,
     *         LSUMSS, ANTD(1,LIF,LP,4), FRAC)
C                                       solve for antenna weight
            CALL ASOLVE (PRTLEV, 'rms', BEGTIM, LSTIME, CANT, 3.0,
     *         LSUMWT, ANTD(1,LIF,LP,5), FRAC)
C                                       mark all 1-sample
            DO 162 ANT1 = 1,CANT
               IF (BADANT(ANT1).EQ.1) ANTD(ANT1,LIF,LP,1) = -1.E6
 162           CONTINUE
 165        CONTINUE
 170     CONTINUE
C                                       local source number
      J = 0
      DO 175 I = 1,SCOUNT
         IF (THISOU.EQ.SINUM(I)) J = I
 175     CONTINUE
      IF (J.EQ.0) THEN
         SCOUNT = SCOUNT + 1
         J = SCOUNT
         SINUM(J) = THISOU
         END IF
C                                       Stuff into array
      DO 195 LA = 1,CANT
         DO 190 LIF = 1,CIF
            DO 185 LP = 1,CPOL
               IF ((ANTD(LA,LIF,LP,2).NE.FBLANK) .AND.
     *            (ANTD(LA,LIF,LP,2).GT.0.0) .AND.
     *            (ANTD(LA,LIF,LP,1).NE.FBLANK) .AND.
     *            (ANTD(LA,LIF,LP,1).GT.0.0)) THEN
                  SCNT(J) = SCNT(J) + 1
                  SRSUM(J) = SRSUM(J) + ANTD(LA,LIF,LP,2)
                  SASUM(J) = SASUM(J) + ANTD(LA,LIF,LP,1)
                  SRRSUM(J) = SRRSUM(J) + ANTD(LA,LIF,LP,2) ** 2
                  SAASUM(J) = SAASUM(J) + ANTD(LA,LIF,LP,1) ** 2
                  END IF
 185           CONTINUE
 190        CONTINUE
 195     CONTINUE
C                                       write to scratch table
      CALL OTABXX (XXFILE, 'WRIT', XXROW, BEGTIM, LSTIME, J,
     *   ANTD(1,1,1,1), ANTD(1,1,1,2), ANTD(1,1,1,3), ANTD(1,1,1,4),
     *   ANTD(1,1,1,5), ANTD(1,1,1,6), COUNT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Clear accumulators:
      CALL DFILL (NZERO, 0.0D0, SUMV)
      CALL DFILL (NZERO, 0.0D0, SUMVS)
      CALL DFILL (NZERO, 0.0D0, SUMS)
      CALL DFILL (NZERO, 0.0D0, SUMSS)
      CALL DFILL (NZERO, 0.0D0, SUMWT)
      CALL DFILL (NZERO, 0.0D0, SUMWTA)
      CALL FILL (NZERO, 0, COUNT)
      CALL FILL (NZERO, 0, COUNTA)
C                                       Another clipping interval?
      IF (.NOT.DONE) THEN
         THISOU = CURSOU
         BEGTIM = CURTIM
         ENDTIM = CURTIM + TCLIP
         THIFRQ = CURFRQ
         THISUB = CURSUB
         GO TO 110
         END IF
C-----------------------------------------------------------------------
C                                       Okay - close table and re-read
C                                       to fill memory
      CALL TABCLO (XXFILE, IERR)
      IF (IERR.NE.0) GO TO 990
      MTIME = XXROW - 1
      IF ((MTIME.GT.CTIME) .OR. (MTIME.GT.NTIME)) THEN
         LP = MIN (NTIME, CTIME)
         WRITE (MSGTXT,1200) MTIME, LP
         CALL MSGWRT (7)
         MTIME = LP
         END IF
      CALL OXXINI (XXFILE, 'READ', XXROW, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 215 IP = 1,MTIME
         CALL OTABXX (XXFILE, 'READ', XXROW, TIMR(1,IP), TIMR(2,IP),
     *      SNUMS(IP), TD(1,1,1,1,IP), TD(1,1,1,2,IP), TD(1,1,1,3,IP),
     *      TD(1,1,1,4,IP), TD(1,1,1,5,IP), TD(1,1,1,6,IP), COUNT, IERR)
         IF (IERR.NE.0) GO TO 990
 215     CONTINUE
      CALL TABCLO (XXFILE, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL RFILL (12, 0.0, FRACNT)
      FRATOT = 0.0
C                                       average source sums
      DO 220 LS = 1,SCOUNT
         IF (SCNT(LS).GT.0) THEN
            SRSUM(LS) = SRSUM(LS) / SCNT(LS)
            SASUM(LS) = SASUM(LS) / SCNT(LS)
            SRRSUM(LS) = SRRSUM(LS) / SCNT(LS)
            SAASUM(LS) = SAASUM(LS) / SCNT(LS)
            END IF
 220     CONTINUE
C                                       get source info, scale clips
      DO 230 LS = 1,SCOUNT
C                                       Get source name, qualifier
         THISOU = SINUM(LS)
         IF (DOSOU) THEN
            CALL SOUNFO (SUTAB, THISOU, 'SOURCE', TYPE, DIM, DUMMY,
     *         SUNAME(LS), IERR)
            IF (IERR.NE.0) GO TO 990
            CALL SOUNFO (SUTAB, THISOU, 'QUAL', TYPE, DIM, QUAL(LS),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       tell user
         IF ((OPTYPE.EQ.'GAIN') .AND. (SASUM(LS).GT.0.0)) THEN
            WRITE (MSGTXT,1220) SUNAME(LS), QUAL(LS), SASUM(LS)
            LINE = MSGTXT
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (QUIT) DOCRT = 0
               END IF
            IF (DOCRT.LE.0) CALL MSGWRT (4)
            END IF
 230     CONTINUE
C                                       scale to gains
      IF (OPTYPE.EQ.'GAIN') THEN
         DO 270 IP = 1,MTIME
            LS = SNUMS(IP)
            DO 260 LA = 1,CANT
               DO 250 LIF = 1,CIF
                  DO 240 LP = 1,CPOL
                     IF ((TD(LA,LIF,LP,1,IP).NE.FBLANK) .AND.
     *                  (TD(LA,LIF,LP,1,IP).GT.0.0)) TD(LA,LIF,LP,1,IP)
     *                  = TD(LA,LIF,LP,1,IP) / SASUM(LS)
                     IF ((TD(LA,LIF,LP,3,IP).NE.FBLANK) .AND.
     *                  (TD(LA,LIF,LP,3,IP).GT.0.0)) TD(LA,LIF,LP,3,IP)
     *                  = TD(LA,LIF,LP,3,IP) / SASUM(LS)
 240                 CONTINUE
 250              CONTINUE
 260           CONTINUE
 270        CONTINUE
         SCOUNT = 1
         END IF
C                                       outer loops: antenna, source
      DO 500 XS = 1,SCOUNT
         LS = XS
C                                       find correct amp, rms
         IF ((PRTLEV.GE.2) .AND. (DOCRT.NE.0)) THEN
            LINE = ' '
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) GO TO 990
            LINE = 'List all data values'
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
C                                       Page labels
            LINE = 'Antenna-based amplitudes, rmses, weights'
            I = ITRIM (LINE)
            IF (OPTYPE.EQ.'GAIN') THEN
               LINE(I+1:) = ' for all sources'
            ELSE
               LINE(I+1:) = ' for ' // SUNAME(LS)
               END IF
            DIM(1) = LEN (LINE)
            DIM(2) = 1
            CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, LINE, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            LINE = 'An IF P Time    V amp    V rms      '
     *         // 'S amp    S rms    Weight'
            CALL OPUT (PRINT, 'TITLE2', OOACAR, DIM, DUMMY, LINE, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
         DO 350 LA = 1,CANT
            DO 340 LIF = 1,CIF
               DO 330 LP = 1,CPOL
                  DO 325 IT = 1,5
                     II = 0
                     DO 320 IP = 1,MTIME
                        IF (OPTYPE.EQ.'GAIN') LS = SNUMS(IP)
                        IF (SNUMS(IP).EQ.LS) THEN
                           IF ((PRTLEV.GE.2) .AND. (IT.EQ.1) .AND.
     *                        (TD(LA,LIF,LP,1,IP).NE.FBLANK) .AND.
     *                        (TD(LA,LIF,LP,1,IP).GT.0.0) .AND.
     *                        (DOCRT.NE.0)) THEN
                              WRITE (LINE,1310) LA, LIF, LP, IP,
     *                           (TD(LA,LIF,LP,LT,IP), LT = 1,6)
                              CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                              IF (IERR.NE.0) GO TO 990
                              IF (QUIT) DOCRT = 0
                              END IF
                           IF ((TD(LA,LIF,LP,IT,IP).NE.FBLANK) .AND.
     *                        (TD(LA,LIF,LP,IT,IP).GE.0.0)) THEN
                              II = II + 1
                              LIST(II) = TD(LA,LIF,LP,IT,IP)
C                                       closure error statistics
                              IF ((IT.EQ.1) .AND.
     *                           (TD(LA,LIF,LP,6,IP).NE.FBLANK)) THEN
                                 IF (TD(LA,LIF,LP,6,IP).LE.0.0) THEN
                                    JJ = 1
                                 ELSE
                                    JJ = TD(LA,LIF,LP,6,IP) / 0.1 + 2
                                    JJ = MIN (JJ, 12)
                                    END IF
                                 FRATOT = FRATOT + 1.0
                                 FRACNT(JJ) = FRACNT(JJ) + 1.0
                                 END IF
                              END IF
                           END IF
 320                    CONTINUE
                     IF (II.GT.0) THEN
                        CALL ROBUST (LIST, II, DV(IT), RV(IT))
                        PP(LP,LIF,LA,IT) = DV(IT)
                        PR(LP,LIF,LA,IT) = RV(IT)
                     ELSE
                        PP(LP,LIF,LA,IT) = 0.0
                        PR(LP,LIF,LA,IT) = 1.E12
                        END IF
 325                 CONTINUE
 330              CONTINUE
 340           CONTINUE
 350        CONTINUE
C                                       get global averages reliably
         DO 390 IT = 1,5
            II = 0
            DO 380 LA = 1,CANT
               DO 370 LIF = 1,CIF
                  DO 360 LP = 1,CPOL
                     IF ((PR(LP,LIF,LA,IT).LT.1.E9)) THEN
                        II = II + 1
                        LIST(II) = PP(LP,LIF,LA,IT)
                        END IF
 360                 CONTINUE
 370              CONTINUE
 380           CONTINUE
            IF (II.GT.0) THEN
               CALL ROBUST (LIST, II, DV(IT), RV(IT))
            ELSE
               DV(IT)= 0.0
               RV(IT) = 0.0
               END IF
 390        CONTINUE
C                                       Report overall numbers
C                                       Page labels
         IF (DOCRT.NE.0) THEN
            LINE = ' '
            DIM(1) = LEN (LINE)
            DIM(2) = 1
            CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, LINE, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) GO TO 990
            LINE = 'Robust average over all IFs and polarizations of' //
     *         ' the robust averages over time'
            CALL OPUT (PRINT, 'TITLE2', OOACAR, DIM, DUMMY, LINE, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
         DO 395 IT = 1,5
            IF (DOCRT.LE.0) THEN
               IF (OPTYPE.EQ.'GAIN') THEN
                  WRITE (MSGTXT,1390) DTYPE(IT), DV(IT), RV(IT),
     *               'All sources', ' '
               ELSE
                  WRITE (MSGTXT,1390) DTYPE(IT), DV(IT), RV(IT),
     *               'source=', SUNAME(LS)
                  END IF
               CALL MSGWRT (4)
               END IF
            IF (DOCRT.NE.0) THEN
               IF (OPTYPE.EQ.'GAIN') THEN
                  WRITE (LINE,1391) DTYPE(IT), DV(IT), RV(IT),
     *               'All sources', ' '
               ELSE
                  WRITE (LINE,1391) DTYPE(IT), DV(IT), RV(IT),
     *               'source=', SUNAME(LS)
                  END IF
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (QUIT) DOCRT = 0
               END IF
 395        CONTINUE
C                                       report particular numbers
         IF ((PRTLEV.GE.1) .AND. (PRINT.NE.' ') .AND. (DOCRT.NE.0)) THEN
            LINE = ' '
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) GO TO 990
            LINE = 'List all robust averages with their uncertainties'
            I = ITRIM (LINE)
            IF (OPTYPE.EQ.'GAIN') THEN
               LINE(I+1:) = ' for all sources'
            ELSE
               LINE(I+1:) = ' for ' // SUNAME(LS)
               END IF
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
C                                       Page labels
            LINE = 'IF P An  Vector amp    Vector rms  '
     *         // '  Scalar amp    Scalar rms         Weight'
            DIM(1) = LEN (LINE)
            DIM(2) = 1
            CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, LINE, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            IF (OPTYPE.EQ.'GAIN') THEN
               LINE = 'All sources'
            ELSE
               LINE = SUNAME(LS)
               END IF
            LINE(16:) = '+-'
            LINE(30:) = '+-'
            LINE(44:) = '+-'
            LINE(58:) = '+-'
            LINE(76:) = '+-'
            CALL OPUT (PRINT, 'TITLE2', OOACAR, DIM, DUMMY, LINE, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
C                                       loop over data
            DO 420 LIF = 1,CIF
               DO 410 LP = 1,CPOL
                  DO 400 LA = 1,CANT
                     IF ((PR(LP,LIF,LA,3).LT.1.E9) .OR.
     *                  (PR(LP,LIF,LA,1).LT.1.E7) .OR.
     *                  (PR(LP,LIF,LA,2).LT.1.E7)) THEN
                        WRITE (LINE,1400) LIF, LP, LA,
     *                     (PP(LP,LIF,LA,IT), PR(LP,LIF,LA,IT),
     *                     IT = 1,5)
                        CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                        IF (IERR.NE.0) GO TO 990
                        IF (QUIT) THEN
                           DOCRT = 0
                           GO TO 425
                           END IF
                        END IF
 400                 CONTINUE
 410              CONTINUE
 420           CONTINUE
            END IF
C                                       page titles
 425     IF ((PRTLEV.GE.1) .AND. (PRINT.NE.' ') .AND. (DOCRT.NE.0)) THEN
            LINE = ' '
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) GO TO 990
            LINE = 'Number samples more than 5 and 10 sigma from mean'
     *         // ' by type and orphan samples'
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            LINE = 'IF P An Total  Vector amp  Vector rms '
     *         // ' Scalar amp  Scalar rms      Weight  Orphans'
            DIM(1) = LEN (LINE)
            DIM(2) = 1
            CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, LINE, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            IF (OPTYPE.EQ.'GAIN') THEN
               LINE = 'All sources'
            ELSE
               LINE = SUNAME(LS)
               END IF
            LINE(21:) = '5  10'
            LINE(33:) = '5  10'
            LINE(45:) = '5  10'
            LINE(57:) = '5  10'
            LINE(69:) = '5  10'
            CALL OPUT (PRINT, 'TITLE2', OOACAR, DIM, DUMMY, LINE, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
         DO 460 LIF = 1,CIF
            DO 450 LP = 1,CPOL
               DO 440 LA = 1,CANT
                  CALL FILL (5, 0, NOFIVE)
                  CALL FILL (5, 0, NOFTEN)
                  K = 0
                  J = 0
                  DO 430 IP = 1,MTIME
                     IF (OPTYPE.EQ.'GAIN') LS = SNUMS(IP)
                     IF (SNUMS(IP).EQ.LS) THEN
                        IF (TD(LA,LIF,LP,1,IP).NE.FBLANK) THEN
                           K = K + 1
                           IF (TD(LA,LIF,LP,1,IP).LE.0.0) THEN
                              J = J + 1
                           ELSE
                              DO 428 IT = 1,5
                                 IF (PR(LP,LIF,LA,IT).GT.0.0) THEN
                                    W = ABS (TD(LA,LIF,LP,IT,IP) -
     *                                 PP(LP,LIF,LA,IT)) /
     *                                 PR(LP,LIF,LA,IT)
                                    IF (W.GT.5.0) NOFIVE(IT) =
     *                                 NOFIVE(IT) + 1
                                    IF (W.GT.10.) NOFTEN(IT) =
     *                                 NOFTEN(IT) + 1
                                    END IF
 428                             CONTINUE
                              END IF
                           END IF
                        END IF
 430                 CONTINUE
                  IF ((PRTLEV.GE.1) .AND. (PRINT.NE.' ') .AND.
     *               (DOCRT.NE.0)) THEN
                     I = NOFIVE(1) + NOFIVE(2) + NOFIVE(3) + NOFIVE(4) +
     *                  NOFIVE(5) + J
                     IF (I.GT.0) THEN
                        WRITE (LINE,1430) LIF, LP, LA, K, (NOFIVE(I),
     *                     NOFTEN(I), I = 1,5), J
                        CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                        IF (IERR.NE.0) GO TO 990
                        IF (QUIT) DOCRT = 0
                        END IF
                     END IF
 440              CONTINUE
 450           CONTINUE
 460        CONTINUE
         CALL RFILL (12, 0.0, NOVER)
C                                       page titles
         IF ((PRTLEV.GE.1) .AND. (PRINT.NE.' ') .AND. (DOCRT.NE.0)) THEN
            LINE = ' '
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) GO TO 990
            LINE = 'Number samples more than 10 and 20 sigma from '
     *         // 'overall mean by type and orphan samples'
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            LINE = 'IF P An Total  Vector amp  Vector rms '
     *         // ' Scalar amp  Scalar rms      Weight  Orphans'
            DIM(1) = LEN (LINE)
            DIM(2) = 1
            CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, LINE, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            IF (OPTYPE.EQ.'GAIN') THEN
               LINE = 'All sources'
            ELSE
               LINE = SUNAME(LS)
               END IF
            LINE(20:) = '10  20'
            LINE(32:) = '10  20'
            LINE(44:) = '10  20'
            LINE(56:) = '10  20'
            LINE(68:) = '10  20'
            CALL OPUT (PRINT, 'TITLE2', OOACAR, DIM, DUMMY, LINE, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
         DO 490 LIF = 1,CIF
            DO 485 LP = 1,CPOL
               DO 480 LA = 1,CANT
                  CALL FILL (5, 0, NOFIVE)
                  CALL FILL (5, 0, NOFTEN)
                  K = 0
                  J = 0
                  DO 475 IP = 1,MTIME
                     IF (OPTYPE.EQ.'GAIN') LS = SNUMS(IP)
                     IF (SNUMS(IP).EQ.LS) THEN
                        IF (TD(LA,LIF,LP,1,IP).NE.FBLANK) THEN
                           K = K + 1
                           IF (TD(LA,LIF,LP,1,IP).LE.0.0) THEN
                              J = J + 1
                           ELSE
                              DO 470 IT = 1,5
                                 IF (RV(IT).GT.0.0) THEN
                                    W = ABS (TD(LA,LIF,LP,IT,IP)-DV(IT))
     *                                 / RV(IT)
                                    IF (W.GT.10.0) NOFIVE(IT) =
     *                                 NOFIVE(IT) + 1
                                    IF (W.GT.20.) NOFTEN(IT) =
     *                                 NOFTEN(IT) + 1
                                    END IF
 470                             CONTINUE
                              END IF
                           END IF
                        END IF
 475                 CONTINUE
C                                       sum for final return
                  NOVER(1) = NOVER(1) + NOFIVE(1)
                  NOVER(2) = NOVER(2) + NOFIVE(2)
                  NOVER(3) = NOVER(3) + NOFIVE(3)
                  NOVER(4) = NOVER(4) + NOFIVE(4)
                  NOVER(5) = NOVER(5) + NOFIVE(5)
                  NOVER(6) = NOVER(6) + NOFTEN(1)
                  NOVER(7) = NOVER(7) + NOFTEN(2)
                  NOVER(8) = NOVER(8) + NOFTEN(3)
                  NOVER(9) = NOVER(9) + NOFTEN(4)
                  NOVER(10) = NOVER(10) + NOFTEN(5)
                  NOVER(11) = NOVER(11) + J
                  NOVER(12) = NOVER(12) + K
C                                       print
                  IF ((PRTLEV.GE.1) .AND. (PRINT.NE.' ') .AND.
     *               (DOCRT.NE.0)) THEN
                     I = NOFIVE(1) + NOFIVE(2) + NOFIVE(3) + NOFIVE(4) +
     *                  NOFIVE(5) + J
                     IF (I.GT.0) THEN
                        WRITE (LINE,1430) LIF, LP, LA, K, (NOFIVE(I),
     *                     NOFTEN(I), I = 1,5), J
                        CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                        IF (IERR.NE.0) GO TO 990
                        IF (QUIT) DOCRT = 0
                        END IF
                     END IF
 480              CONTINUE
 485           CONTINUE
 490        CONTINUE
C                                       print
         IF ((PRTLEV.GE.1) .AND. (PRINT.NE.' ') .AND. (DOCRT.NE.0)) THEN
            J = NOVER(11) + 0.01
            K = NOVER(12) + 0.01
            NOFIVE(1) = NOVER(1) + 0.01
            NOFIVE(2) = NOVER(2) + 0.01
            NOFIVE(3) = NOVER(3) + 0.01
            NOFIVE(4) = NOVER(4) + 0.01
            NOFIVE(5) = NOVER(5) + 0.01
            NOFTEN(1) = NOVER(6) + 0.01
            NOFTEN(2) = NOVER(7) + 0.01
            NOFTEN(3) = NOVER(8) + 0.01
            NOFTEN(4) = NOVER(9) + 0.01
            NOFTEN(5) = NOVER(10) + 0.01
            WRITE (LINE,1430) LIF, LP, LA, K, (NOFIVE(I), NOFTEN(I),
     *         I = 1,5), J
            LINE(1:7) = 'Totals '
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (QUIT) DOCRT = 0
            END IF
C                                       print closure info
         IF (FRATOT.LE.0.0) FRATOT = 1.0
         DO 494 J = 1,12
            FRACNT(J) = FRACNT(J) / FRATOT
            IF ((J.GE.2) .AND. (J.LE.11)) ARRAY2(9+J,1) = FRACNT(J)
 494        CONTINUE

         IF ((PRTLEV.GE.1) .AND. (PRINT.NE.' ') .AND. (DOCRT.NE.0)) THEN
            LINE = ' '
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (QUIT) DOCRT = 0
            IF (DOCRT.NE.0) THEN
               LINE = 'Fraction of closure error in range:'
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (QUIT) DOCRT = 0
               END IF
            IF (DOCRT.NE.0) THEN
               WRITE (LINE,1494) 0.0, FRACNT(1)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (QUIT) DOCRT = 0
               END IF
            DO 495 J = 2,11
               IF (DOCRT.NE.0) THEN
                  AMP = 0.1 * (J-2)
                  RMS = AMP + 0.1
                  WRITE (LINE,1495) AMP, RMS, FRACNT(J)
                  CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                  IF (IERR.NE.0) GO TO 990
                  IF (QUIT) DOCRT = 0
                  END IF
 495           CONTINUE
            IF (DOCRT.NE.0) THEN
               WRITE (LINE,1494) 1.0, FRACNT(12)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (QUIT) DOCRT = 0
               END IF
            END IF
C                                       store away the answers for AIPS
         IF (XS.EQ.1) THEN
            AVODIM(1) = 20
            AVODIM(2) = 2
            AVODIM(3) = 1
            CALL OPUT (OUTPUT, AVONAM(1), OOARE, AVODIM, ARRAY2,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 500     CONTINUE
C-----------------------------------------------------------------------
C                                       Close file
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.DOCAT) THEN
         CALL TABZAP (XXFILE, IERR)
         XXFILE = ' '
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Close printer
      IF (PRINT.NE.' ') THEN
         CALL OCLOSE (PRINT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Delete temporary table
      IF (DOSOU) THEN
         CALL DESTRY (SUTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'ANTIME: ERROR IN DATA VALUE FINDING FOR ' // UVIN
      CALL MSGWRT (7)
      IF ((.NOT.DOCAT) .AND. (XXFILE.NE.' ')) CALL TABZAP (XXFILE, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('READ',I6,' TIMES, WILL DO ONLY',I5)
 1220 FORMAT ('Scale ',A,':',I5.5,' by rel flux',F10.5)
 1310 FORMAT (I2,I3,I2,I5,F11.5,F9.5,F11.5,F9.5,1PE11.3,0PF7.4)
 1390 FORMAT ('Overall av ',A,'=',1PE10.3,' +-',1PE10.3,1X,A,1X,A)
 1391 FORMAT (A,' =',1PE12.3,'  +-',1PE11.3,4X,A,1X,A)
 1400 FORMAT (I2,I2,I3,F7.3,3F7.4,F7.3,3F7.4,1X,2(1PE10.3))
 1430 FORMAT (I2,I2,I3,I6,5(2X,I6,I4),I9)
 1494 FORMAT ('Equal ',F4.1,F9.6)
 1495 FORMAT (F4.1,' -',F4.1,F9.6)
      END
      SUBROUTINE AVERAG (NC, VIS, INCF, ANT1, ANT2, AONLY, DOROBU,
     *   NP, WP, SP, SSP)
C-----------------------------------------------------------------------
C   AVERAG does normal or robust average of a spectrum
C   Inputs:
C      NC       I        Number channels
C      VIS      R(3,*)   Visibilities
C      INCF     I        Increment between frequencies in vis
C      ANT1     I        Antenna 1 number
C      ANT2     I        Antenna 2 number
C      AONLY    L        Do amplitude only
C      DOROBU   L        Do robust average
C   Outputs:
C      NP       I(2)     Count of included samples
C      WP       R(2)     Sum of weights
C      SP       R(2)     weighted sum (amp or Re/Im)
C      SSP      R(2)     weighted sum square (of amp or of Re/Im)
C-----------------------------------------------------------------------
      INTEGER   NC, INCF, ANT1, ANT2, NP(2)
      REAL      VIS(3,*), WP(2), SP(2), SSP(2)
      LOGICAL   AONLY, DOROBU
C
      INTEGER   I, J, ITER, NITER, NR, NI
      REAL      W, FACT(7)
      DOUBLE PRECISION SUMR, SUMI, SUMRS, SUMIS, WR, WI, AVR, AVI, RMR,
     *   RMI, AMP
      DATA FACT /6.0, 5.0, 4.0, 3.5, 3.1, 2.7, 3.0/
C-----------------------------------------------------------------------
      NITER = 1
      IF (DOROBU) NITER = 7
C                                       iterate
      AVR = 0.0D0
      AVI = 0.0D0
      RMR = 1.0D10
      RMI = 1.0D10
      IF ((AONLY) .OR. (ANT1.EQ.ANT2)) THEN
         DO 20 ITER = 1,NITER
            NR = 0
            SUMR = 0.0D0
            SUMRS = 0.0D0
            WR = 0.0D0
            J = 1 - INCF
            DO 10 I = 1,NC
               J = J + INCF
               W = VIS(3,J)
               IF (W.GT.0) THEN
                  IF (AONLY) THEN
                     AMP = SQRT (VIS(1,J)*VIS(1,J) + VIS(2,J)*VIS(2,J))
                  ELSE
                     AMP = VIS(1,J)
                     END IF
                  IF (ABS(AMP-AVR).LT.RMR) THEN
                     NR = NR + 1
                     WR = WR + W
                     SUMR = SUMR + W * AMP
                     SUMRS = SUMRS + W * AMP * AMP
                     END IF
                  END IF
 10            CONTINUE
            IF (NR.GT.0) THEN
               AVR = SUMR / WR
               RMR = SUMRS / WR - AVR * AVR
               RMR = SQRT (MAX (0.0D0, RMR)) * FACT(ITER)
               END IF
            IF ((NR.LT.3) .OR. (RMR.LE.0.0D0)) GO TO 25
 20         CONTINUE
 25      NP(1) = NR
         NP(2) = 0
         WP(1) = WR
         WP(2) = 0.
         SP(1) = SUMR
         SP(2) = 0.
         SSP(1) = SUMRS
         SSP(2) = 0.
      ELSE
         DO 40 ITER = 1,NITER
            NR = 0
            NI = 0
            SUMR = 0.0D0
            SUMI = 0.0D0
            SUMRS = 0.0D0
            SUMIS = 0.0D0
            WR = 0.0D0
            WI = 0.0D0
            J = 1 - INCF
            DO 30 I = 1,NC
               J = J + INCF
               W = VIS(3,J)
               IF (W.GT.0) THEN
                  IF (ABS(VIS(1,J)-AVR).LT.RMR) THEN
                     NR = NR + 1
                     WR = WR + W
                     SUMR = SUMR + W * VIS(1,J)
                     SUMRS = SUMRS + W * VIS(1,J) * VIS(1,J)
                     END IF
                  IF (ABS(VIS(2,J)-AVI).LT.RMI) THEN
                     NI = NI + 1
                     WI = WI + W
                     SUMI = SUMI + W * VIS(2,J)
                     SUMIS = SUMIS + W * VIS(2,J) * VIS(2,J)
                     END IF
                  END IF
 30            CONTINUE
            IF ((NR.GT.0) .AND. (NI.GT.0)) THEN
               AVR = SUMR / WR
               RMR = SUMRS / WR - AVR * AVR
               RMR = SQRT (MAX (0.0D0, RMR)) * FACT(ITER)
               AVI = SUMI / WI
               RMI = SUMIS / WI - AVI * AVI
               RMI = SQRT (MAX (0.0D0, RMI)) * FACT(ITER)
               END IF
            IF ((NR.LT.3) .OR. (RMR.LE.0.0D0)) GO TO 45
            IF ((NI.LT.3) .OR. (RMI.LE.0.0D0)) GO TO 45
 40         CONTINUE
 45      NP(1) = NR
         NP(2) = NI
         WP(1) = WR
         WP(2) = WI
         SP(1) = SUMR
         SP(2) = SUMI
         SSP(1) = SUMRS
         SSP(2) = SUMIS
         END IF
C
 999  RETURN
      END
      SUBROUTINE AMVDIF (UVIN, PRINT, OUTPUT, CANT, CIF, CPOL, CCHAN,
     *   CTIME, SUM, SUMWT, COUNT, WORK, ANTD, AMPS, DIFS, WTS, REFAS,
     *   FRAS, WORKS, IERR)
C-----------------------------------------------------------------------
C   Look for bad data via amplitude of the vector difference of the
C   current sample from a running mean of the samples
C   Inputs:
C      UVIN      C*?   Name of input uvdata object.
C      PRINT     C*?   Print object
C      OUTPUT    C*?   Output parameter object
C      CANT      I     Maximum antenna number
C      CPOL      I     Number of polarizations
C      CIF       I     Number of IFs
C      CCHAN     I     Number of spactral channels
C      CTIME     I     Limit on number of times
C   Scratch variables
C      SUM, SUMSQ, SUMWT, COUNT, AMPS, DIFS
C      Note AMPS, DIFS, WTS cannot be used when
C           SUM, WORK, SUMWT, COUNT are being used
C   Inputs attached to UVIN:
C      MAXRMS    R(*)  Maximum RMS allowed, constant plus amplitude
C                      coefficient.
C      MAXBAD    R(2)  Maximum allowed fraction of bad baselines.
C      TIMEAVG   R     Time in seconds for clipping interval
C      DOIFS     L     Flag all IFs if one bad
C      DOSTOKES  L     Flas all Stokes if 1 bad
C      AMPONLY   L     Use amplitude not Real/Imag
C      KILLONES  L     Kill 1-sample integrations
C      PRTLEV    I     Print level for debugging
C   Output:
C      IERR      I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), PRINT*(*), OUTPUT*(*)
      INTEGER   CANT, CIF, CPOL, CCHAN, CTIME, IERR
      INTEGER   COUNT(CANT,CANT,CIF,CPOL), REFAS(CIF,CPOL,*)
      REAL      SUM(CANT,CANT,CIF,CPOL), WORK(CANT,CANT,CIF,CPOL),
     *   SUMWT(CANT,CANT,CIF,CPOL), ANTD(CANT,CIF,CPOL,6),
     *   AMPS(CANT,CIF,CPOL,*), DIFS(CANT,CIF,CPOL,*),
     *   WTS(CANT,CIF,CPOL,*), FRAS(CANT,CIF,CPOL,*), WORKS(*)
C
      REAL      TCLIP, SCNTIM
      LOGICAL   DOCAT
      INTEGER   PRTLEV, REFANT
C
      INTEGER   NSORC, NTIME
      PARAMETER (NSORC = 5000, NTIME = 15001)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TYPE, DIM(7), DUMMY, ANT1, ANT2, ENDVIS, VISNO, CURSOU,
     *   THISOU, TBVER,DOCRT, I, J, LS, NACROS, ILOCU, ILOCV, ILOCW,
     *   ILOCT, ILOCB, ILOCSU, ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC,
     *   JLOCS, JLOCF, JLOCR, JLOCD, JLOCIF, LA, INCS, INCF, INCIF, LP,
     *   LIF, IP, THISUB, THIFRQ, CURSUB, CURFRQ, NZERO, XXROW, NP, KP,
     *   K, JJ, SNUMS(NTIME), MTIME, SINUM(NSORC), SCOUNT, QUAL, MINNO,
     *  BADANT(MAXANT), NREF(MAXIF*2), MODE, NREFS(MAXANT), LIST(NTIME),
     *   KP1, KP2, AVODIM(3), NOTEN(3), NOTWEN(3), NPP(2)
      LOGICAL   DONE, DOSOU, QUIT, DOROBU
      CHARACTER SUTAB*32, SUNAME*16, LINE*132, CDUMMY*1, XXFILE*32,
     *   DTYPE(5)*10
      REAL      RP(50), TEPS, CURTIM, LSTIME, ENDTIM, BEGTIM,
     *   VIS(3,MAXCIF), TIMR(2,NTIME), RE(NTIME), IM(NTIME), TI(NTIME),
     *   MERE, MEIM, MEDIUM, SCNT2, ARRAY2(20,2), FRAC(MAXANT,MAXIF,2),
     *   FRACNT(12), FRATOT, MEDIAN, WP(2), SP(2), SPP(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /TBUFS/ TIMR, RE, IM, TI, SNUMS, LIST
      INCLUDE 'OUTPUT.INC'
      INCLUDE 'OUTPUTDATA.INC'
      DATA DTYPE /'Amplitude', 'Vector dif', ' ', ' ', 'Weight'/
C-----------------------------------------------------------------------
      XXFILE = ' '
      TEPS = 0.02 / (3600.0 * 24.0)
      SCOUNT = 0
      MINNO = 3
      MODE = 10
C                                       Open printer
      IF (PRINT.NE.' ') THEN
         CALL OOPEN (PRINT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'NACROS', TYPE, DIM, NACROS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         DOCRT = 0
         END IF
      THISOU = 0
      THISUB = 0
      THIFRQ = 0
      CURSUB = 0
      CURFRQ = 0
C                                       Create and open a scratch table:
      XXFILE = 'Temporary results table'
      CALL UV2TAB (UVIN, XXFILE, 'XX', 1, IERR)
      IF (IERR.NE.0) THEN
         XXFILE = ' '
         GO TO 990
         END IF
      CALL OXXINI (XXFILE, 'WRIT', XXROW, IERR)
      IF (IERR.NE.0) THEN
         XXFILE = ' '
         GO TO 990
         END IF
C                                       Retrieve pointers into the data:
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 999
      INCS = INCS / 3
      INCF = INCF / 3
      INCIF = INCIF / 3
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get parameters
      CALL OUVGET (UVIN, 'DOROBUST', TYPE, DIM, DOROBU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'REFANT', TYPE, DIM, REFANT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (REFANT.LE.0) REFANT = 1
      CALL OUVGET (UVIN, 'DOCAT', TYPE, DIM, DOCAT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'SCANLENG', TYPE, DIM, SCNTIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      SCNTIM = SCNTIM / 86400.0
      SCNT2 = SCNTIM / 2.0
      CALL OUVGET (UVIN, 'TIMEAVG', TYPE, DIM, TCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      TCLIP = TCLIP / 86400.0
C                                       Processing info
      DOSOU = (ILOCSU.GT.0)
C                                       Source table/name
      IF (DOSOU) THEN
         SUTAB = 'SoUrce table for ANTIME'
         TBVER = 1
         CALL UV2TAB (UVIN, SUTAB, 'SU', TBVER, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       For single source use 'OBJECT'
      ELSE
         CALL UVDGET (UVIN, 'OBJECT', TYPE, DIM, DUMMY, SUNAME, IERR)
         IF (IERR.NE.0) GO TO 990
         QUAL = 0
         END IF
C                                       Clear accumulators:
      NZERO = CANT * CANT * CIF * CPOL
      CALL RFILL (NZERO, 0.0, SUM)
      CALL RFILL (NZERO, 0.0, SUMWT)
      CALL FILL (NZERO, 0, COUNT)
C                                       Initialize visibility count
      DONE = .FALSE.
      VISNO = 0
C                                       Swallow input collecting
C                                       statistics.
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VIS, IERR)
         IF (IERR.LT.0) THEN
            DONE = .TRUE.
            IERR = 0
            END IF
         IF (IERR.GT.0) GO TO 990
         VISNO = VISNO + 1
 110     CURTIM = RP(ILOCT)
         IF (DOSOU) THEN
            CURSOU = RP(ILOCSU) + 0.5
         ELSE
            CURSOU = 0
            END IF
         IF (ILOCFQ.GT.0) CURFRQ = RP(ILOCFQ) + 0.5
C                                       Get antenna numbers
         IF (ILOCB.GT.0) THEN
            ANT1 = (RP(ILOCB) / 256.0) + 0.001
            ANT2 = (RP(ILOCB) - ANT1 * 256) + 0.001
            CURSUB = (RP(ILOCB) - 256 * ANT1 - ANT2) * 100.0 + 1.1
         ELSE
            ANT1 = RP(ILOCA1) + 0.001
            ANT2 = RP(ILOCA2) + 0.001
            CURSUB = RP(ILOCSA) + 0.001
            END IF
C                                       Initial selection.
         IF (VISNO.EQ.1) THEN
            THIFRQ = CURFRQ
            THISUB = CURSUB
            THISOU = CURSOU
            BEGTIM = CURTIM
            ENDTIM = CURTIM + TCLIP
            END IF
C                                       Finished with data or interval?
         IF (DONE .OR. (CURTIM.GT.ENDTIM) .OR. (CURSOU.NE.THISOU)
     *      .OR. (CURSUB.NE.THISUB)) GO TO 150
C                                       Set last vis of interval
         ENDVIS = VISNO
C                                       Set last time
         LSTIME = CURTIM
C                                       Baseline index
         DO 130 LP = 1,CPOL
            DO 125 LIF = 1,CIF
               IP = (LP-1) * INCS + (LIF-1) * INCIF + 1
               CALL AVERAG (CCHAN, VIS(1,IP), INCF, ANT1, ANT2, .FALSE.,
     *            DOROBU, NPP, WP, SP, SPP)
               COUNT(ANT1,ANT2,LIF,LP) = COUNT(ANT1,ANT2,LIF,LP) +
     *            NPP(1)
               SUMWT(ANT1,ANT2,LIF,LP) = SUMWT(ANT1,ANT2,LIF,LP) +
     *            WP(1)
               SUM(ANT1,ANT2,LIF,LP) = SUM(ANT1,ANT2,LIF,LP) + SP(1)
               COUNT(ANT2,ANT1,LIF,LP) = COUNT(ANT2,ANT1,LIF,LP) +
     *            NPP(2)
               SUMWT(ANT2,ANT1,LIF,LP) = SUMWT(ANT2,ANT1,LIF,LP) +
     *            WP(2)
               SUM(ANT2,ANT1,LIF,LP) = SUM(ANT2,ANT1,LIF,LP) + SP(2)
 125           CONTINUE
 130        CONTINUE
C                                       Next vis until done
         IF (.NOT.DONE) GO TO 100
 150     IERR = 0
C                                       Average and square
       DO 170 LIF = 1,CIF
          DO 165 LP = 1,CPOL
             CALL FILL (CANT, 0, BADANT)
             DO 160 ANT2 = 1,CANT
               DO 155 ANT1 = 1,CANT
                  IF (SUMWT(ANT1,ANT2,LIF,LP).GT.0.0) THEN
                     SUM(ANT1,ANT2,LIF,LP) = SUM(ANT1,ANT2,LIF,LP)
     *                  / SUMWT(ANT1,ANT2,LIF,LP)
                     SUMWT(ANT1,ANT2,LIF,LP) = (SUMWT(ANT1,ANT2,LIF,LP)
     *                  / COUNT(ANT1,ANT2,LIF,LP)) ** 2
                  ELSE
                     SUMWT(ANT1,ANT2,LIF,LP) = FBLANK
                     SUM(ANT1,ANT2,LIF,LP) = FBLANK
                     END IF
 155              CONTINUE
 160           CONTINUE
C                                       solve for antenna weight**2
            CALL ASOLVE (PRTLEV, 'rms', BEGTIM, LSTIME, CANT, 3.0,
     *         SUMWT(1,1,LIF,LP), ANTD(1,LIF,LP,3), FRAC)
C                                       solve for antenna real/imag
            KP = (LP - 1) * CIF + LIF
            CALL VSOLVE (SUM(1,1,LIF,LP), SUMWT(1,1,LIF,LP), CANT,
     *         REFANT, MODE, MINNO, 2.5, ANTD(1,LIF,LP,1),
     *         ANTD(1,LIF,LP,2), NREF(KP), ANTD(1,LIF,LP,6), WORK,
     *         PRTLEV, IERR)
 165        CONTINUE
 170     CONTINUE
C                                       local source number
      J = 0
      DO 175 I = 1,SCOUNT
         IF (THISOU.EQ.SINUM(I)) J = I
 175     CONTINUE
      IF (J.EQ.0) THEN
         SCOUNT = SCOUNT + 1
         J = SCOUNT
         SINUM(J) = THISOU
         END IF
C                                       write to scratch table
C                                       ???????
      CALL OTABXX (XXFILE, 'WRIT', XXROW, BEGTIM, LSTIME, J,
     *   ANTD(1,1,1,1), ANTD(1,1,1,2), ANTD(1,1,1,3), ANTD(1,1,1,4),
     *   WORKS, ANTD(1,1,1,6), NREF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Clear accumulators:
      CALL RFILL (NZERO, 0.0, SUM)
      CALL RFILL (NZERO, 0.0, SUMWT)
      CALL FILL (NZERO, 0, COUNT)
C                                       Another clipping interval?
      IF (.NOT.DONE) THEN
         THISOU = CURSOU
         BEGTIM = CURTIM
         ENDTIM = CURTIM + TCLIP
         THIFRQ = CURFRQ
         THISUB = CURSUB
         GO TO 110
         END IF
C-----------------------------------------------------------------------
C                                       Okay - close table and re-read
C                                       to fill memory
      CALL TABCLO (XXFILE, IERR)
      IF (IERR.NE.0) GO TO 990
      MTIME = XXROW - 1
      IF ((MTIME.GT.CTIME) .OR. (MTIME.GT.NTIME)) THEN
         LP = MIN (NTIME, CTIME)
         WRITE (MSGTXT,1200) MTIME, LP
         CALL MSGWRT (7)
         MTIME = LP
         END IF
      CALL OXXINI (XXFILE, 'READ', XXROW, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 200 IP = 1,MTIME
C                                       ?????????????????
         CALL OTABXX (XXFILE, 'READ', XXROW, TIMR(1,IP), TIMR(2,IP),
     *      SNUMS(IP), AMPS(1,1,1,IP), DIFS(1,1,1,IP), WTS(1,1,1,IP),
     *      WORKS, WORKS, FRAS(1,1,1,IP), REFAS(1,1,IP), IERR)
         IF (IERR.NE.0) GO TO 990
 200     CONTINUE
      CALL TABCLO (XXFILE, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL RFILL (12, 0.0, FRACNT)
      FRATOT = 0.0
C                                       outer loops: antenna, source
      DO 500 LS = 1,SCOUNT
C                                       Get source name, qualifier
         THISOU = SINUM(LS)
         IF (DOSOU) THEN
            CALL SOUNFO (SUTAB, THISOU, 'SOURCE', TYPE, DIM, DUMMY,
     *         SUNAME, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL SOUNFO (SUTAB, THISOU, 'QUAL', TYPE, DIM, QUAL,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       Compute amp vect diff in DIFS
C                                       Compute amp in AMPS
         DO 280 LIF = 1,CIF
            DO 270 LP = 1,CPOL
C                                       See about re-referencing
               CALL FILL (CANT, 0, NREFS)
               DO 210 IP = 1,MTIME
                  IF (SNUMS(IP).EQ.LS) THEN
                     LA = REFAS(LIF,LP,IP)
                     IF ((LA.GT.0) .AND. (LA.LE.CANT)) NREFS(LA) =
     *                  NREFS(LA) + 1
                     END IF
 210              CONTINUE
               REFANT = 0
               I = 0
               J = 0
               DO 220 LA = 1,CANT
                  IF (NREFS(LA).GT.I) THEN
                     REFANT = LA
                     I = NREFS(LA)
                     END IF
                  IF (NREFS(LA).GT.0) J = J + 1
 220              CONTINUE
C                                       re-referencing required
               IF ((REFANT.GT.0) .AND. (J.GT.1)) THEN
                  DO 230 LA = 1,CANT
                     IF ((NREFS(LA).GT.0) .AND. (LA.NE.REFANT))
     *                  CALL REREF (CANT, CIF, CPOL, MTIME, LIF, LP, LS,
     *                  REFANT, LA, SNUMS, TIMR, AMPS, DIFS, REFAS)
 230                 CONTINUE
                  END IF
C                                       do vect difference
               DO 260 LA = 1,CANT
C                                       list points
                  NP = 0
                  DO 240 IP = 1,MTIME
                     IF (SNUMS(IP).EQ.LS) THEN
                        IF ((AMPS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                     (DIFS(LA,LIF,LP,IP).NE.FBLANK)) THEN
                           NP = NP + 1
                           TI(NP) = (TIMR(1,IP) + TIMR(2,IP)) / 2.0
                           RE(NP) = AMPS(LA,LIF,LP,IP)
                           IM(NP) = DIFS(LA,LIF,LP,IP)
                           LIST(NP) = IP
                           END IF
                        END IF
 240                 CONTINUE
C                                       finally do it
                  DO 250 KP = 1,NP
C                                       find range of points
                     KP1 = KP
                     KP2 = KP
 245                 IF ((KP1.GT.1) .AND. (TI(KP)-TI(KP1-1).LE.SCNT2))
     *                  THEN
                        KP1 = KP1 - 1
                        GO TO 245
                        END IF
 246                 IF ((KP2.LT.NP) .AND.
     *                  (TI(KP2+1)-TI(KP1).LE.SCNTIM)) THEN
                        KP2 = KP2 + 1
                        GO TO 246
                        END IF
                     I = KP2 - KP1 + 1
C                                       vector diff and amp
                     MERE = MEDIUM (RE(KP1), IM(KP1), I)
                     MEIM = MEDIUM (IM(KP1), RE(KP1), I)
                     IP = LIST(KP)
                     DIFS(LA,LIF,LP,IP) = SQRT ((RE(KP)-MERE)**2 +
     *                  (IM(KP)-MEIM)**2)
                     AMPS(LA,LIF,LP,IP) = SQRT (RE(KP)**2 + IM(KP)**2)
 250                 CONTINUE
 260              CONTINUE
 270           CONTINUE
 280        CONTINUE
C                                       use robust to get average amp
         CALL RFILL (40, 0.0, ARRAY2)
         KP = 0
         DO 320 IP = 1,MTIME
            IF (SNUMS(IP).EQ.LS) THEN
               DO 315 LP = 1,CPOL
                  DO 310 LIF = 1,CIF
                     DO 305 LA = 1,CANT
                        IF (AMPS(LA,LIF,LP,IP).NE.FBLANK) THEN
                           KP = KP + 1
                           WORKS(KP) = AMPS(LA,LIF,LP,IP)
C                                       closure ststs
                           IF (FRAS(LA,LIF,LP,IP).NE.FBLANK) THEN
                              IF (FRAS(LA,LIF,LP,IP).LE.0.0) THEN
                                 JJ = 1
                              ELSE
                                 JJ = FRAS(LA,LIF,LP,IP) / 0.1 + 2
                                 JJ = MIN (JJ, 12)
                                 END IF
                              FRATOT = FRATOT + 1.0
                              FRACNT(JJ) = FRACNT(JJ) + 1.0
                              END IF
                           END IF
 305                    CONTINUE
 310                 CONTINUE
 315              CONTINUE
               END IF
 320        CONTINUE
         MERE = MEDIUM (WORKS, WORKS, KP)
         ARRAY2(1,2) = MERE
         CALL ROBUST (WORKS, KP, ARRAY2(1,1), ARRAY2(6,1))
C                                       rms
         DO 325 IP = 1,KP
            IF (WORKS(IP).NE.0.0) WORKS(IP) = ABS (WORKS(IP) - MERE)
 325        CONTINUE
         MEIM = MEDIAN (KP, WORKS)
         ARRAY2(6,2) = 1.5 * SQRT (2.0) * MEIM
C                                       use median to get average dif
         KP = 0
         DO 350 IP = 1,MTIME
            IF (SNUMS(IP).EQ.LS) THEN
               DO 345 LP = 1,CPOL
                  DO 340 LIF = 1,CIF
                     DO 335 LA = 1,CANT
                        IF (DIFS(LA,LIF,LP,IP).NE.FBLANK) THEN
                           KP = KP + 1
                           WORKS(KP) = DIFS(LA,LIF,LP,IP)
                           END IF
 335                    CONTINUE
 340                 CONTINUE
 345              CONTINUE
               END IF
 350        CONTINUE
         MERE = MEDIAN (KP, WORKS)
         ARRAY2(2,2) = MERE
         CALL ROBUST (WORKS, KP, ARRAY2(2,1), ARRAY2(7,1))
C                                       rms
         DO 355 IP = 1,KP
            WORKS(IP) = ABS (WORKS(IP) - MERE)
 355        CONTINUE
         MEIM = MEDIAN (KP, WORKS)
         ARRAY2(7,2) = 1.5 * SQRT (2.0) * MEIM
C                                       use median to get average wt
         KP = 0
         DO 380 IP = 1,MTIME
            IF (SNUMS(IP).EQ.LS) THEN
               DO 375 LP = 1,CPOL
                  DO 370 LIF = 1,CIF
                     DO 365 LA = 1,CANT
                        IF ((WTS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                     (WTS(LA,LIF,LP,IP).GT.0.0)) THEN
                           KP = KP + 1
                           WORKS(KP) = WTS(LA,LIF,LP,IP)
                           END IF
 365                    CONTINUE
 370                 CONTINUE
 375              CONTINUE
               END IF
 380        CONTINUE
         MERE = MEDIUM (WORKS, WORKS, KP)
         ARRAY2(5,2) = MERE
         CALL ROBUST (WORKS, KP, ARRAY2(5,1), ARRAY2(10,1))
C                                       rms
         DO 385 IP = 1,KP
            IF (WORKS(IP).NE.0.0) WORKS(IP) = ABS (WORKS(IP) - MERE)
 385        CONTINUE
         MEIM = MEDIAN (KP, WORKS)
         ARRAY2(10,2) = 1.5 * SQRT (2.0) * MEIM
C                                       Median estimates discarded
C                                       print summary
         IF (PRINT.NE.' ') THEN
            LINE = ' '
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            LINE = 'Overall robust-estimated averages for ' //
     *         SUNAME
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            DO 360 I = 1,5
               WRITE (LINE,1385) DTYPE(I), ARRAY2(I,1), ARRAY2(I+5,1)
               IF ((ARRAY2(I,1).GT.0.0) .AND. (DOCRT.NE.0)) THEN
                  CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                  IF (QUIT) DOCRT = 0
                  IF (IERR.NE.0) GO TO 990
                  END IF
 360           CONTINUE
            END IF
C                                       page titles
         CALL RFILL (20, 0.0, ARRAY2(1,2))
         IF ((PRTLEV.GE.1) .AND. (PRINT.NE.' ') .AND. (DOCRT.NE.0)) THEN
            LINE = ' '
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (QUIT) DOCRT = 0
            IF (IERR.NE.0) GO TO 990
            LINE = 'Number samples more than 10 and 20 sigma from '
     *         // 'overall mean by type'
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            LINE = 'IF P An   Total    Vector amp    Vector dif '
     *         // '       Weight'
            DIM(1) = LEN (LINE)
            DIM(2) = 1
            CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, LINE, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            LINE = SUNAME
            LINE(20:) = 'X 10  X 20'
            LINE(34:) = 'X 10  X 20'
            LINE(48:) = 'X 10  X 20'
            CALL OPUT (PRINT, 'TITLE2', OOACAR, DIM, DUMMY, LINE, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (DOCRT.NE.0) THEN
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (QUIT) DOCRT = 0
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
C                                       number 10, 20 * rms
         DO 420 LP = 1,CPOL
            DO 415 LIF = 1,CIF
               DO 410 LA = 1,CANT
                  K = 0
                  J = 0
                  CALL FILL (3, 0, NOTEN)
                  CALL FILL (3, 0, NOTWEN)
                  DO 405 IP = 1,MTIME
                     IF (SNUMS(IP).EQ.LS) THEN
                        IF (AMPS(LA,LIF,LP,IP).NE.FBLANK) THEN
                           K = K + 1
                           IF (ABS(AMPS(LA,LIF,LP,IP)-ARRAY2(1,1))
     *                        .GT.10.0*ARRAY2(6,1)) THEN
                              NOTEN(1) = NOTEN(1) + 1
                              IF (ABS(AMPS(LA,LIF,LP,IP)-ARRAY2(1,1))
     *                           .GT.20.0*ARRAY2(6,1))
     *                           NOTWEN(1) = NOTWEN(1) + 1
                              END IF
                           END IF
                        IF (DIFS(LA,LIF,LP,IP).NE.FBLANK) THEN
                           IF (ABS(DIFS(LA,LIF,LP,IP)-ARRAY2(2,1))
     *                        .GT.10.0*ARRAY2(7,1)) THEN
                              NOTEN(2) = NOTEN(2) + 1
                              IF (ABS(DIFS(LA,LIF,LP,IP)-ARRAY2(2,1))
     *                           .GT.20.0*ARRAY2(7,1))
     *                           NOTWEN(2) = NOTWEN(2) + 1
                              END IF
                           END IF
                        IF ((WTS(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                     (WTS(LA,LIF,LP,IP).GT.0.0)) THEN
                           IF (ABS(WTS(LA,LIF,LP,IP)-ARRAY2(5,1))
     *                        .GT.10.0*ARRAY2(10,1)) THEN
                              NOTEN(3) = NOTEN(3) + 1
                              IF (ABS(WTS(LA,LIF,LP,IP)-ARRAY2(5,1))
     *                           .GT.20.0*ARRAY2(10,1))
     *                           NOTWEN(3) = NOTWEN(3) + 1
                              END IF
                           END IF
                        END IF
 405                 CONTINUE
                  ARRAY2(1,2) = ARRAY2(1,2) + NOTEN(1)
                  ARRAY2(2,2) = ARRAY2(2,2) + NOTEN(2)
                  ARRAY2(5,2) = ARRAY2(5,2) + NOTEN(3)
                  ARRAY2(6,2) = ARRAY2(6,2) + NOTWEN(1)
                  ARRAY2(7,2) = ARRAY2(7,2) + NOTWEN(2)
                  ARRAY2(10,2) = ARRAY2(10,2) + NOTWEN(3)
                  ARRAY2(12,2) = ARRAY2(12,2) + K
C                                       print
                  IF ((PRTLEV.GE.1) .AND. (PRINT.NE.' ') .AND.
     *               (DOCRT.NE.0)) THEN
                     I = NOTEN(1) + NOTEN(2) + NOTEN(3)
                     IF (I.GT.0) THEN
                        WRITE (LINE,1400) LIF, LP, LA, K, (NOTEN(I),
     *                     NOTWEN(I), I = 1,3)
                        CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                        IF (IERR.NE.0) GO TO 990
                        IF (QUIT) DOCRT = 0
                        END IF
                     END IF
 410              CONTINUE
 415           CONTINUE
 420        CONTINUE
C                                       print
         IF ((PRTLEV.GE.1) .AND. (PRINT.NE.' ') .AND. (DOCRT.NE.0)) THEN
            K = ARRAY2(12,2) + 0.01
            NOTEN(1) = ARRAY2(1,2) + 0.01
            NOTEN(2) = ARRAY2(2,2) + 0.01
            NOTEN(3) = ARRAY2(5,2) + 0.01
            NOTWEN(1) = ARRAY2(6,2) + 0.01
            NOTWEN(2) = ARRAY2(7,2) + 0.01
            NOTWEN(3) = ARRAY2(10,2) + 0.01
            WRITE (LINE,1400) LIF, LP, LA, K, (NOTEN(I), NOTWEN(I),
     *         I = 1,3)
            LINE(1:7) = 'Totals '
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (QUIT) DOCRT = 0
            END IF
C                                       print closure info
         IF (FRATOT.LE.0.0) FRATOT = 1.0
         DO 440 J = 1,12
            FRACNT(J) = FRACNT(J) / FRATOT
            IF ((J.GE.2) .AND. (J.LE.11)) ARRAY2(9+J,1) = FRACNT(J)
 440        CONTINUE

         IF ((PRTLEV.GE.1) .AND. (PRINT.NE.' ') .AND. (DOCRT.NE.0)) THEN
            LINE = ' '
            CALL PRTWRI (PRINT, LINE, QUIT, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (QUIT) DOCRT = 0
            IF (DOCRT.NE.0) THEN
               LINE = 'Fraction of closure error in range:'
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (QUIT) DOCRT = 0
               END IF
            IF (DOCRT.NE.0) THEN
               WRITE (LINE,1440) 0.0, FRACNT(1)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (QUIT) DOCRT = 0
               END IF
            DO 445 J = 2,11
               IF (DOCRT.NE.0) THEN
                  MERE = 0.1 * (J-2)
                  MEIM = MERE + 0.1
                  WRITE (LINE,1445) MERE, MEIM, FRACNT(J)
                  CALL PRTWRI (PRINT, LINE, QUIT, IERR)
                  IF (IERR.NE.0) GO TO 990
                  IF (QUIT) DOCRT = 0
                  END IF
 445           CONTINUE
            IF (DOCRT.NE.0) THEN
               WRITE (LINE,1440) 1.0, FRACNT(12)
               CALL PRTWRI (PRINT, LINE, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (QUIT) DOCRT = 0
               END IF
            END IF
C                                       store away the answers for AIPS
         IF (LS.EQ.1) THEN
            AVODIM(1) = 20
            AVODIM(2) = 2
            AVODIM(3) = 1
            CALL OPUT (OUTPUT, AVONAM(1), OOARE, AVODIM, ARRAY2,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 500     CONTINUE
C-----------------------------------------------------------------------
C                                       Close file
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.DOCAT) THEN
         CALL TABZAP (XXFILE, IERR)
         XXFILE = ' '
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Delete temporary table
      IF (DOSOU) THEN
         CALL DESTRY (SUTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Close printer
      IF (PRINT.NE.' ') THEN
         CALL OCLOSE (PRINT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'AMVDIF: ERROR IN DATA VALUE FINDING FOR ' // UVIN
      CALL MSGWRT (7)
      IF ((.NOT.DOCAT) .AND. (XXFILE.NE.' ')) CALL TABZAP (XXFILE, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('READ',I6,' TIMES, WILL DO ONLY',I5)
 1385 FORMAT ('Antenna ',A,2(2X,1PE13.5,'  +-',1PE13.5))
 1400 FORMAT (I2,I2,I3,I8,3(I8,I6))
 1440 FORMAT ('Equal ',F4.1,F9.6)
 1445 FORMAT (F4.1,' -',F4.1,F9.6)
      END
      SUBROUTINE AMVRFI (UVIN, PRINT, OUTPUT, CANT, CIF, CPOL, CCHAN,
     *   CTIME, SUM, SUMWT, VALS, LIST, COUNT, IERR)
C-----------------------------------------------------------------------
C   Look for bad data via amplitude of the vector difference of the
C   current sample from a running mean of the samples - baseline based
C   Inputs:
C      UVIN      C*?   Name of input uvdata object.
C      PRINT     C*?   Print object
C      OUTPUT    C*?   Output adverb object
C      CANT      I     Maximum antenna number
C      CPOL      I     Number of polarizations
C      CIF       I     Number of IFs
C      CCHAN     I     Number of spactral channels
C      CTIME     I     Limit on number of times
C   Scratch variables
C      SUM, SUMWT, VALS, COUNT.
C      LIST and SUM overlap
C   Inputs attached to UVIN:
C      MAXRMS    R(*)  Maximum RMS allowed, constant plus amplitude
C                      coefficient.
C      MAXBAD    R(2)  Maximum allowed fraction of bad baselines.
C      TIMEAVG   R     Time in seconds for clipping interval
C      DOIFS     L     Flag all IFs if one bad
C      DOSTOKES  L     Flas all Stokes if 1 bad
C      PRTLEV    I     Print level for debugging
C   Output:
C      IERR      I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), PRINT*(*), OUTPUT*(*)
      INTEGER   CANT, CIF, CPOL, CCHAN, CTIME, IERR
      REAL      SUM(CANT,CANT,CIF,CPOL,*), SUMWT(CANT,CANT,CIF,CPOL,*),
     *   COUNT(CANT,CANT,CIF,CPOL), VALS(CANT,CANT,CIF,CPOL,*), LIST(*)
C
      REAL      TCLIP, SCNTIM
      INTEGER   PRTLEV
C
      INTEGER   NTIME
      PARAMETER (NTIME = 15001)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TYPE, DIM(7), DUMMY, ANT1, ANT2, ENDVIS, VISNO, CURSOU,
     *   THISOU, TBVER, DOCRT, BEGT(4), ENDT(4), NACROS, ILOCU, ILOCV,
     *   ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ, ILOCA1, ILOCA2, ILOCSA,
     *   JLOCC, JLOCS, JLOCF, JLOCR, JLOCD, JLOCIF, INCS, INCF, INCIF,
     *   LP, LIF, IP, THISUB, THIFRQ, CURSUB, CURFRQ, NZERO, QUAL, NP,
     *   KP, KP1, KP2, IP1, IP2, IPMAX, II, NLIST, NOTEN(3), NOTWEN(3),
     *   NARR, ITER, AVODIM(3), NPP(2)
      LOGICAL   DONE, DOSOU, QUIT, DOROBU
      CHARACTER SUTAB*32, SUNAME*16, CDUMMY*1, DTYPE(3)*10
      REAL      RP(50), TEPS, CURTIM, LSTIME, ENDTIM, BEGTIM, TEMP,
     *   VIS(3,MAXCIF), TIMR(2,NTIME), RE(NTIME), IM(NTIME), MERE, MEIM,
     *   MEDIAN, SCNT2, T2, ARRAY2(20,2), AVG, RMS, PARMA(3), PARMR(3),
     *   TDUMMY(NTIME,3), WP(2), SP(2), SPP(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /TBUFS/ TIMR, RE, IM, TDUMMY
      INCLUDE 'OUTPUT.INC'
      INCLUDE 'OUTPUTDATA.INC'
      DATA DTYPE /'Amplitude', 'Vector dif', 'Weight'/
C-----------------------------------------------------------------------
      TEPS = 0.02 / (3600.0 * 24.0)
      CALL RFILL (40, 0.0, ARRAY2)
      NARR = 0
C                                       Open printer
      IF (PRINT.NE.' ') THEN
         CALL OOPEN (PRINT, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (PRINT, 'NACROS', TYPE, DIM, NACROS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         DOCRT = 0
         END IF
      THISOU = 0
      THISUB = 0
      THIFRQ = 0
      CURSUB = 0
      CURFRQ = 0
C                                       Retrieve pointers into the data:
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 999
      INCS = INCS / 3
      INCF = INCF / 3
      INCIF = INCIF / 3
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get parameters
      CALL OUVGET (UVIN, 'DOROBUST', TYPE, DIM, DOROBU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'SCANLENG', TYPE, DIM, SCNTIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      SCNTIM = SCNTIM / 86400.0
      SCNT2 = SCNTIM / 2.0
      CALL OUVGET (UVIN, 'TIMEAVG', TYPE, DIM, TCLIP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      TCLIP = TCLIP / 86400.0
C                                       Processing info
      DOSOU = (ILOCSU.GT.0)
C                                       Source table/name
      IF (DOSOU) THEN
         SUTAB = 'SoUrce table for ANTIME'
         TBVER = 1
         CALL UV2TAB (UVIN, SUTAB, 'SU', TBVER, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       For single source use 'OBJECT'
      ELSE
         CALL UVDGET (UVIN, 'OBJECT', TYPE, DIM, DUMMY, SUNAME, IERR)
         IF (IERR.NE.0) GO TO 990
         QUAL = 0
         END IF
C                                       Clear accumulators:
C                                       (1,*) =  count
C                                       (2,*) =  sum r then max rms**2
C                                       (3,*) =  sum**2 r then rms**2
C                                       (4,*) =  sum imaginary
C                                       (5,*) =  sum**2 imaginary
C                                       (6,*) =  sum amplitude
      IP1 = 1
      IP2 = 1
      IP = 1
      NZERO = CANT * CANT * CIF * CPOL
      CALL RFILL (NZERO, 0.0, SUM(1,1,1,1,IP))
      CALL RFILL (NZERO, 0.0, SUMWT(1,1,1,1,IP))
      CALL RFILL (NZERO, 0.0, COUNT)
C                                       Initialize visibility count
      DONE = .FALSE.
      VISNO = 0
C                                       page title
      MSGTXT = '        time range        type        value'
     *   // '     +- rms        X 10  X 20'
      IF (DOCRT.NE.0) THEN
         DIM(1) = LEN (MSGTXT)
         DIM(2) = 1
         CALL OPUT (PRINT, 'TITLE1', OOACAR, DIM, DUMMY, MSGTXT, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
         IF (QUIT) DOCRT = 0
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL MSGWRT (5)
         END IF
C                                       Swallow input collecting
C                                       statistics.
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VIS, IERR)
         IF (IERR.LT.0) THEN
            DONE = .TRUE.
            IERR = 0
            END IF
         IF (IERR.GT.0) GO TO 990
         VISNO = VISNO + 1
 110     CURTIM = RP(ILOCT)
         IF (DOSOU) THEN
            CURSOU = RP(ILOCSU) + 0.5
         ELSE
            CURSOU = 0
            END IF
         IF (ILOCFQ.GT.0) CURFRQ = RP(ILOCFQ) + 0.5
C                                       Get antenna numbers
         IF (ILOCB.GT.0) THEN
            ANT1 = (RP(ILOCB) / 256.0) + 0.001
            ANT2 = (RP(ILOCB) - ANT1 * 256) + 0.001
            CURSUB = (RP(ILOCB) - 256 * ANT1 - ANT2) * 100.0 + 1.1
         ELSE
            ANT1 = RP(ILOCA1) + 0.001
            ANT2 = RP(ILOCA2) + 0.001
            CURSUB = RP(ILOCSA) + 0.001
            END IF
C                                       Initial selection.
         IF (VISNO.EQ.1) THEN
            THIFRQ = CURFRQ
            THISUB = CURSUB
            THISOU = CURSOU
            BEGTIM = CURTIM
            ENDTIM = CURTIM + TCLIP
            END IF
C                                       Finished with data or interval?
         IF (DONE .OR. (CURTIM.GT.ENDTIM) .OR. (CURSOU.NE.THISOU)
     *      .OR. (CURSUB.NE.THISUB)) GO TO 150
C                                       Set last vis of interval
         ENDVIS = VISNO
C                                       Set last time
         LSTIME = CURTIM
C                                       Baseline index
         DO 130 LP = 1,CPOL
            DO 125 LIF = 1,CIF
               II = (LP-1) * INCS + (LIF-1) * INCIF + 1
               CALL AVERAG (CCHAN, VIS(1,II), INCF, ANT1, ANT2, .FALSE.,
     *            DOROBU, NPP, WP, SP, SPP)
               COUNT(ANT1,ANT2,LIF,LP) = COUNT(ANT1,ANT2,LIF,LP) +
     *            NPP(1)
               SUMWT(ANT1,ANT2,LIF,LP,IP) = SUMWT(ANT1,ANT2,LIF,LP,IP) +
     *            WP(1)
               SUM(ANT1,ANT2,LIF,LP,IP) = SUM(ANT1,ANT2,LIF,LP,IP) +
     *            SP(1)
               COUNT(ANT2,ANT1,LIF,LP) = COUNT(ANT2,ANT1,LIF,LP) +
     *            NPP(2)
               SUMWT(ANT2,ANT1,LIF,LP,IP) = SUMWT(ANT2,ANT1,LIF,LP,IP) +
     *            WP(2)
               SUM(ANT2,ANT1,LIF,LP,IP) = SUM(ANT2,ANT1,LIF,LP,IP) +
     *            SP(2)
 125           CONTINUE
 130        CONTINUE
C                                       Next vis until done
         IF (.NOT.DONE) GO TO 100
C                                       save times at end of interval
 150   TIMR(1,IP) = BEGTIM
       TIMR(2,IP) = LSTIME
C                                       Average
       DO 170 LIF = 1,CIF
          DO 165 LP = 1,CPOL
             DO 160 ANT2 = 1,CANT
               DO 155 ANT1 = 1,CANT
                  IF (SUMWT(ANT1,ANT2,LIF,LP,IP).GT.0.0) THEN
                     SUM(ANT1,ANT2,LIF,LP,IP) = SUM(ANT1,ANT2,LIF,LP,IP)
     *                  / SUMWT(ANT1,ANT2,LIF,LP,IP)
                     SUMWT(ANT1,ANT2,LIF,LP,IP) =
     *                  SUMWT(ANT1,ANT2,LIF,LP,IP) /
     *                  COUNT(ANT1,ANT2,LIF,LP)
                     END IF
 155              CONTINUE
 160           CONTINUE
 165        CONTINUE
 170     CONTINUE
C                                       process what we have
      IF ((DONE) .OR. (CURSOU.NE.THISOU) .OR. (IP.GE.CTIME) .OR.
     *   (IP.GT.NTIME) .OR. (CURSUB.NE.THISUB)) THEN
         IPMAX = IP
C                                       do to end
         IF ((DONE) .OR. (CURSOU.NE.THISOU) .OR. (CURSUB.NE.THISUB))
     *      THEN
            IP2 = IPMAX
C                                       will need to find the max to do
         ELSE
            TEMP = (TIMR(1,IPMAX) + TIMR(2,IPMAX)) / 2.0
            DO 175 IP = IP1,IPMAX
               IF ((TEMP-(TIMR(1,IP)+TIMR(2,IP))/2.0).GT.SCNT2) IP2 = IP
 175           CONTINUE
            END IF
         IF (DOSOU) THEN
            CALL SOUNFO (SUTAB, THISOU, 'SOURCE', TYPE, DIM, DUMMY,
     *         SUNAME, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL SOUNFO (SUTAB, THISOU, 'QUAL', TYPE, DIM, QUAL,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       loop over times of interest
         DO 240 IP = IP1,IP2
            CALL RFILL (NZERO, FBLANK, VALS(1,1,1,1,IP))
C                                       time range to cover here
            TEMP = (TIMR(1,IP) + TIMR(2,IP)) / 2.0
            KP1 = IP
            KP2 = IP
 180        IF (KP1.GT.1) THEN
               T2 = (TIMR(1,KP1-1) + TIMR(2,KP1-1)) / 2.0
               IF (TEMP-T2.LE.SCNT2) THEN
                  KP1 = KP1 - 1
                  GO TO 180
                  END IF
               END IF
 185        TEMP = (TIMR(1,KP1) + TIMR(2,KP1)) / 2.0
            IF (KP2.LT.IPMAX) THEN
               T2 = (TIMR(1,KP2+1) + TIMR(2,KP2+1)) / 2.0
               IF (T2-TEMP.LE.SCNTIM) THEN
                  KP2 = KP2 + 1
                  GO TO 185
                  END IF
            ELSE
               TEMP = (TIMR(1,KP2) + TIMR(2,KP2)) / 2.0
 186           IF (KP1.GT.1) THEN
                  T2 = (TIMR(1,KP1-1) + TIMR(2,KP1-1)) / 2.0
                  IF (TEMP-T2.LE.SCNTIM) THEN
                     KP1 = KP1 - 1
                     GO TO 186
                     END IF
                  END IF
               END IF
C                                       find all correlators Vdif
            DO 230 ANT1 = 1,CANT-1
               DO 225 ANT2 = ANT1+1,CANT
                  DO 220 LIF = 1,CIF
                     DO 215 LP = 1,CPOL
                        IF (SUMWT(ANT1,ANT2,LIF,LP,IP).GT.0.0) THEN
                           NP = 0
                           DO 210 KP = KP1,KP2
                              IF (SUMWT(ANT1,ANT2,LIF,LP,KP).GT.0.0)
     *                           THEN
                                 NP = NP + 1
                                 RE(NP) = SUM(ANT1,ANT2,LIF,LP,KP)
                                 IM(NP) = SUM(ANT2,ANT1,LIF,LP,KP)
                                 END IF
 210                          CONTINUE
                           MERE = MEDIAN (NP, RE)
                           MEIM = MEDIAN (NP, IM)
                           VALS(ANT1,ANT2,LIF,LP,IP) = SQRT
     *                        ((SUM(ANT1,ANT2,LIF,LP,IP)-MERE)**2 +
     *                        (SUM(ANT2,ANT1,LIF,LP,IP)-MEIM)**2)
                           VALS(ANT2,ANT1,LIF,LP,IP) = SQRT
     *                        (SUM(ANT1,ANT2,LIF,LP,IP)**2 +
     *                        SUM(ANT2,ANT1,LIF,LP,IP)**2)
                           END IF
 215                    CONTINUE
 220                 CONTINUE
 225              CONTINUE
 230           CONTINUE
 240        CONTINUE
         CALL FILL (3, 0, NOTEN)
         CALL FILL (3, 0, NOTWEN)
         DO 280 ITER = 0,3
            NLIST = 0
            DO 270 IP = IP1,IP2
               DO 260 ANT1 = 1,CANT-1
                  DO 255 ANT2 = ANT1+1,CANT
                     DO 250 LIF = 1,CIF
                        DO 245 LP = 1,CPOL
                           IF (SUMWT(ANT1,ANT2,LIF,LP,IP).GT.0.0) THEN
                              NLIST = NLIST + 1
                              IF (ITER.EQ.0) THEN
                                 LIST(NLIST) = VALS(ANT2,ANT1,LIF,LP,IP)
                              ELSE IF (ITER.EQ.1) THEN
                                 TEMP = ABS (LIST(NLIST)-AVG)
                                 IF (TEMP.GT.10.*RMS) NOTEN(ITER) =
     *                              NOTEN(ITER) + 1
                                 IF (TEMP.GT.20.*RMS) NOTWEN(ITER) =
     *                              NOTWEN(ITER) + 1
                                 LIST(NLIST) = VALS(ANT1,ANT2,LIF,LP,IP)
                              ELSE IF (ITER.EQ.2) THEN
                                 TEMP = ABS (LIST(NLIST)-AVG)
                                 IF (TEMP.GT.10.*RMS) NOTEN(ITER) =
     *                              NOTEN(ITER) + 1
                                 IF (TEMP.GT.20.*RMS) NOTWEN(ITER) =
     *                              NOTWEN(ITER) + 1
                                 LIST(NLIST) =
     *                              SUMWT(ANT1,ANT2,LIF,LP,IP)
                              ELSE
                                 TEMP = ABS (LIST(NLIST)-AVG)
                                 IF (TEMP.GT.10.*RMS) NOTEN(ITER) =
     *                              NOTEN(ITER) + 1
                                 IF (TEMP.GT.20.*RMS) NOTWEN(ITER) =
     *                              NOTWEN(ITER) + 1
                                 END IF
                              END IF
 245                       CONTINUE
 250                    CONTINUE
 255                 CONTINUE
 260              CONTINUE
 270           CONTINUE
            IF (ITER.LT.3) THEN
               CALL ROBUST (LIST, NLIST, AVG, RMS)
               PARMA(ITER+1) = AVG
               PARMR(ITER+1) = RMS
               END IF
 280        CONTINUE
         NARR = NARR + 1
         ARRAY2(1,1) = ARRAY2(1,1) + PARMA(1)
         ARRAY2(2,1) = ARRAY2(2,1) + PARMA(2)
         ARRAY2(5,1) = ARRAY2(5,1) + PARMA(3)
         ARRAY2(6,1) = ARRAY2(6,1) + PARMR(1)
         ARRAY2(7,1) = ARRAY2(7,1) + PARMR(2)
         ARRAY2(10,1) = ARRAY2(10,1) + PARMR(3)
         ARRAY2(1,2) = ARRAY2(1,2) + NOTEN(1)
         ARRAY2(2,2) = ARRAY2(2,2) + NOTEN(2)
         ARRAY2(5,2) = ARRAY2(5,2) + NOTEN(3)
         ARRAY2(6,2) = ARRAY2(6,2) + NOTWEN(1)
         ARRAY2(7,2) = ARRAY2(7,2) + NOTWEN(2)
         ARRAY2(10,2) = ARRAY2(10,2) + NOTWEN(3)
         CALL TODHMS (TIMR(1,IP1), BEGT)
         CALL TODHMS (TIMR(2,IP2), ENDT)
C                                       report
         DO 290 ITER = 1,3
            WRITE (MSGTXT,1280) BEGT, ENDT, DTYPE(ITER), PARMA(ITER),
     *         PARMR(ITER), NOTEN(ITER), NOTWEN(ITER)
            IF (DOCRT.EQ.0) THEN
               CALL MSGWRT (5)
            ELSE
               CALL PRTWRI (PRINT, MSGTXT, QUIT, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (QUIT) DOCRT = 0
               END IF
 290        CONTINUE
C                                       shift buffers down
         IF (IP2.LT.IPMAX) THEN
            KP1 = IP2 + 1
            TEMP = (TIMR(1,KP1) + TIMR(2,KP1)) / 2.0
 335        IF (KP1.GT.1) THEN
               T2 = (TIMR(1,KP1-1) + TIMR(2,KP1-1)) / 2.0
               IF (TEMP-T2.LE.SCNT2) THEN
                  KP1 = KP1 - 1
                  GO TO 335
                  END IF
               END IF
C                                       shift down
            IP1 = IP2 - KP1 + 2
            NP = 0
            DO 340 IP = KP1,IPMAX
               NP = NP + 1
               CALL RCOPY (NZERO, SUM(1,1,1,1,IP), SUM(1,1,1,1,NP))
               CALL RCOPY (NZERO, SUMWT(1,1,1,1,IP), SUMWT(1,1,1,1,NP))
               CALL RCOPY (2, TIMR(1,IP), TIMR(1,NP))
 340           CONTINUE
            IP = NP
         ELSE
            IP = 0
            IP1 = 1
            END IF
         END IF
C                                       advance for next buffer
      IF (.NOT.DONE) THEN
         IP = IP + 1
         CALL RFILL (NZERO, 0.0, SUM(1,1,1,1,IP))
         CALL RFILL (NZERO, 0.0, SUMWT(1,1,1,1,IP))
         CALL FILL (NZERO, 0, COUNT)
         THISOU = CURSOU
         BEGTIM = CURTIM
         ENDTIM = CURTIM + TCLIP
         THIFRQ = CURFRQ
         THISUB = CURSUB
         GO TO 110
         END IF
C-----------------------------------------------------------------------
C                                       send back data
      NARR = MAX (1, NARR)
      ARRAY2(1,1) = ARRAY2(1,1) / NARR
      ARRAY2(2,1) = ARRAY2(2,1) / NARR
      ARRAY2(5,1) = ARRAY2(5,1) / NARR
      ARRAY2(6,1) = ARRAY2(6,1) / NARR
      ARRAY2(7,1) = ARRAY2(7,1) / NARR
      ARRAY2(10,1) = ARRAY2(10,1) / NARR
      AVODIM(1) = 20
      AVODIM(2) = 2
      AVODIM(3) = 1
      CALL OPUT (OUTPUT, AVONAM(1), OOARE, AVODIM, ARRAY2, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close file
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete temporary table
      IF (DOSOU) THEN
         CALL DESTRY (SUTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Close printer
      IF (PRINT.NE.' ') THEN
         CALL OCLOSE (PRINT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'AMVRFI: ERROR IN DATA VALUE FINDING FOR ' // UVIN
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1280 FORMAT (I2,'/',2(I2.2,':'),I2.2,' -',I2,'/',2(I2.2,':'),I2.2,2X,
     *   A,1PE11.3,' +-',1PE10.3,I6,I6)
      END
      SUBROUTINE OXXINI (TABLE, OPCODE, XXROW, IERR)
C-----------------------------------------------------------------------
C   Initialize a weights scratch table (type XX) for reading or writing,
C   creating it if necessary.
C   Inputs:
C      TABLE  C*(*)     Name of table object:  NAME, CLASS, IMSEQ, DISK
C                       and VER should be set.
C      OPCODE C*4       Operation code
C                         'READ' - open for reading
C                         'WRIT' - open for writing (create if
C                                  necessary)
C
C   Outputs:
C      XXROW  I         Next row to read or write
C      IERR   I         Return code: 0 => successful
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   XXROW, IERR
C
      INCLUDE 'XXTAB.INC'
      CHARACTER LABEL*56, COLLAB(NCOL,2)*24, COLUNT(NCOL,2)*8
      INTEGER   COLTYP(NCOL,2), COLDIM(NCOL), LCOL
      INTEGER   TYPE, DIM(3), IDUMMY
      CHARACTER DUMMY
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LABEL / 'AMP/RMS/WT SCRATCH TABLE'/
      DATA COLLAB / 'TIME1', 'TIME2', 'SOURCE', 'VECTOR AMP',
     *   'VECTOR RMS', 'SCALAR AMP', 'SCALAR RMS', 'WEIGHT', 'CLOSURE',
     *   'TIME1', 'TIME2', 'SOURCE', 'REAL', 'IMAG', 'WEIGHT',
     *   'CLOSURE', 'REF ANT', ' ' /
      DATA COLUNT / 'DAYS', 'DAYS', ' ', 4*'Jy', '1/Jy**2', ' ',
     *    'DAYS', 'DAYS', ' ', 2*'Jy', '1/Jy**2', ' ', ' ', ' ' /
      DATA COLTYP / OOARE, OOARE, OOAINT, 6*OOARE,
     *     OOARE, OOARE, OOAINT, 4*OOARE, 2*OOAINT /
      DATA COLDIM / NCOL*1 /
C-----------------------------------------------------------------------
      LCOL = NCOL + 1 - XXTYPE
C                                       Fill in table type:
      TYPE = OOACAR
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'TBLTYPE', TYPE, DIM, IDUMMY, 'XX', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Initialize table data if
C                                       writing:
      IF (OPCODE.EQ.'WRIT') THEN
         TYPE = OOACAR
         DIM(1) = 56
         DIM(2) = 1
         DIM(3) = 0
         CALL TABPUT (TABLE, 'LABEL', TYPE, DIM, IDUMMY, LABEL, IERR)
         IF (IERR.NE.0) GO TO 999
         TYPE = OOAINT
         DIM(1) = 1
         CALL TABPUT (TABLE, 'NCOL', TYPE, DIM, LCOL, DUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABPUT (TABLE, 'VER', TYPE, DIM, 0, DUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         TYPE = OOACAR
         DIM(1) = 24
         DIM(2) = LCOL
         CALL TABPUT (TABLE, 'COLABEL', TYPE, DIM, IDUMMY,
     *      COLLAB(1,XXTYPE), IERR)
         IF (IERR.NE.0) GO TO 999
         DIM(1) = 8
         CALL TABPUT (TABLE, 'COLUNIT', TYPE, DIM, IDUMMY,
     *      COLUNT(1,XXTYPE), IERR)
         IF (IERR.NE.0) GO TO 999
         TYPE = OOAINT
         DIM(1) = LCOL
         DIM(2) = 1
         CALL TABPUT (TABLE, 'COLTYPE', TYPE, DIM, COLTYP(1,XXTYPE),
     *      DUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         COLDIM(4) = NSAMP
         COLDIM(5) = NSAMP
         COLDIM(6) = NSAMP
         COLDIM(7) = NSAMP
         COLDIM(8) = NSAMP
         COLDIM(9) = NSAMP
         IF (XXTYPE.EQ.2) COLDIM(8) = NCORS
         CALL TABPUT (TABLE, 'COLDIM', TYPE, DIM, COLDIM, DUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Initialize table:
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'CANNOT OPEN WEIGHTS SCRATCH (XX) TABLE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Get column indices
      CALL TABCOL (TABLE, LCOL, COLLAB(1,XXTYPE), COLIDX, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get next row:
      IF (OPCODE.EQ.'READ') THEN
         XXROW = 1
      ELSE
         TYPE = OOAINT
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL TABGET (TABLE, 'NROW', TYPE, DIM, XXROW, DUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         XXROW = XXROW + 1
         END IF
C
 999  RETURN
      END
      SUBROUTINE OTABXX (TABLE, OPCODE, XXROW, TB, TE, SU, VAMP, VRMS,
     *   SAMP, SRMS, WT, CLOS, REFA, IERR)
C-----------------------------------------------------------------------
C   Read or write a record to a amp/rms scratch table.
C   Inputs:
C      TABLE   C*(*)   Name of table object
C      OPCODE  C*4     Operation code: 'READ' or 'WRIT'
C   Input/output:
C      XXROW   I       Row number to read or write (incremented on exit)
C      TB      R       Start time
C      TE      R       End time
C      SU      I       source number
C      VAMP    R(*)    Amplitude values (Nant, Nif, Npol)
C      VRMS    R(*)    RMS values (Nant, Nif, Npol)
C      SAMP    R(*)    Amplitude values (Nant, Nif, Npol)
C      SRMS    R(*)    RMS values (Nant, Nif, Npol) (XTYPE = 1 only)
C      WT      R(*)    Weight values (Nant, Nif, Npol) (XTYPE = 1 only)
C      REFA    i(*)    Refant (Nif, Npol) (XTYPE = 2 only)
C   Output:
C      IERR    I       Return code: 0 => successful
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   XXROW, SU, REFA(*), IERR
      REAL      TB, TE, VAMP(*), VRMS(*), SAMP(*), SRMS(*), WT(*),
     *   CLOS(*)
C
      INTEGER   TYPE, DIM(3), DUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'XXTAB.INC'
C-----------------------------------------------------------------------
      IF (OPCODE.EQ.'WRIT') THEN
         TYPE = OOAINT
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL TABDPT (TABLE, XXROW, COLIDX(3), TYPE, DIM, SU, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         TYPE = OOARE
         CALL TABDPT (TABLE, XXROW, COLIDX(1), TYPE, DIM, TB, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDPT (TABLE, XXROW, COLIDX(2), TYPE, DIM, TE, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         DIM(1) = NSAMP
         CALL TABDPT (TABLE, XXROW, COLIDX(4), TYPE, DIM, VAMP, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDPT (TABLE, XXROW, COLIDX(5), TYPE, DIM, VRMS, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDPT (TABLE, XXROW, COLIDX(6), TYPE, DIM, SAMP, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         IF (XXTYPE.EQ.1) THEN
            CALL TABDPT (TABLE, XXROW, COLIDX(7), TYPE, DIM, SRMS,
     *         DUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABDPT (TABLE, XXROW, COLIDX(8), TYPE, DIM, WT, DUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABDPT (TABLE, XXROW, COLIDX(9), TYPE, DIM, CLOS,
     *         DUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
         ELSE
            CALL TABDPT (TABLE, XXROW, COLIDX(7), TYPE, DIM, CLOS,
     *         DUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
            TYPE = OOAINT
            DIM(1) = NCORS
            CALL TABDPT (TABLE, XXROW, COLIDX(8), TYPE, DIM, REFA,
     *         DUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
      ELSE
         CALL TABDGT (TABLE, XXROW, COLIDX(1), TYPE, DIM, TB, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (TABLE, XXROW, COLIDX(2), TYPE, DIM, TE, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (TABLE, XXROW, COLIDX(3), TYPE, DIM, SU, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (TABLE, XXROW, COLIDX(4), TYPE, DIM, VAMP, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (TABLE, XXROW, COLIDX(5), TYPE, DIM, VRMS, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (TABLE, XXROW, COLIDX(6), TYPE, DIM, SAMP, DUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         IF (XXTYPE.EQ.1) THEN
            CALL TABDGT (TABLE, XXROW, COLIDX(7), TYPE, DIM, SRMS,
     *         DUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABDGT (TABLE, XXROW, COLIDX(8), TYPE, DIM, WT, DUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABDGT (TABLE, XXROW, COLIDX(9), TYPE, DIM, CLOS,
     *         DUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
         ELSE
            CALL TABDGT (TABLE, XXROW, COLIDX(7), TYPE, DIM, CLOS,
     *         DUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABDGT (TABLE, XXROW, COLIDX(8), TYPE, DIM, REFA,
     *         DUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C
      XXROW = XXROW + 1
C
 999  RETURN
      END
      SUBROUTINE REREF (CANT, CIF, CPOL, MTIME, LIF, LP, LS, REFANT,
     *   ANT, SNUMS, TIMR, CREAL, CIMAG, REFAS)
C-----------------------------------------------------------------------
C   Re-references from ANT to REFANT
C   Inputs:
C      CANT      I     Maximum antenna number
C      CIF       I     Number of IFs
C      CPOL      I     Number of polarizations
C      MTIME     I     Number of times
C      LIF       I     Current IF
C      LP        I     Current polarization
C      LS        I     Current source
C      REFANT    I     Desired reference antenna
C      ANT       I     Reference antenna to be undone
C      SNUMS     I(*)  Source numbers vs time
C      TIMR      R(*)  Time range (start/stop, time)
C   In/out:
C      CREAL     R(*)  Real (ant,if,pol,time)
C      CIMAG     R(*)  Imag (ant,if,pol,time)
C      REFAS     I(*)  Refants (if,pol,time)
C-----------------------------------------------------------------------
      INTEGER   CANT, CIF, CPOL, MTIME, LIF, LP, LS, REFANT, ANT,
     *   SNUMS(*), REFAS(CIF,CPOL,*)
      REAL      TIMR(2,*), CREAL(CANT,CIF,CPOL,*),
     *   CIMAG(CANT,CIF,CPOL,*)
C
      INTEGER   IP, NP, LIST(1001), IP1, IP2, KP1, KP2, LA
      REAL      RE1, RE2, IM1, IM2, TI1, TI2, W1, W2, TI, RE, IM, AMP,
     *   TRE, TIM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       make a list of matching times
      NP = 0
      DO 10 IP = 1,MTIME
         IF (SNUMS(IP).EQ.LS) THEN
            IF ((REFAS(LIF,LP,IP).EQ.REFANT) .AND.
     *         (CREAL(ANT,LIF,LP,IP).NE.FBLANK) .AND.
     *         ((CREAL(ANT,LIF,LP,IP).NE.0.0) .OR.
     *         (CIMAG(ANT,LIF,LP,IP).NE.0.0))) THEN
               NP = NP + 1
               LIST(NP) = IP
            ELSE IF ((REFAS(LIF,LP,IP).EQ.ANT) .AND.
     *         (CREAL(REFANT,LIF,LP,IP).NE.FBLANK) .AND.
     *         ((CREAL(REFANT,LIF,LP,IP).NE.0.0) .OR.
     *         (CIMAG(REFANT,LIF,LP,IP).NE.0.0))) THEN
               NP = NP + 1
               LIST(NP) = IP
               END IF
            END IF
 10      CONTINUE
C                                       no connection
      IF (NP.LE.0) THEN
         WRITE (MSGTXT,1010) ANT, REFANT, LIF, LP
         CALL MSGWRT (6)
C                                       re-reference
      ELSE
C                                       initial interp values
         IP1 = 1
         IP2 = MIN (2, NP)
         KP1 = LIST(IP1)
         KP2 = LIST(IP2)
         IF (REFAS(LIF,LP,KP1).EQ.REFANT) THEN
            RE1 = CREAL(REFANT,LIF,LP,KP1)
            IM1 = CIMAG(REFANT,LIF,LP,KP1)
         ELSE
            RE1 = CREAL(ANT,LIF,LP,KP1)
            IM1 = -CIMAG(ANT,LIF,LP,KP1)
            END IF
         IF (REFAS(LIF,LP,KP2).EQ.REFANT) THEN
            RE2 = CREAL(REFANT,LIF,LP,KP2)
            IM2 = CIMAG(REFANT,LIF,LP,KP2)
         ELSE
            RE2 = CREAL(ANT,LIF,LP,KP2)
            IM2 = -CIMAG(ANT,LIF,LP,KP2)
            END IF
         TI1 = (TIMR(1,KP1) + TIMR(2,KP1)) / 2.0
         TI2 = (TIMR(1,KP2) + TIMR(2,KP2)) / 2.0
C                                       loop through data
         DO 50 IP = 1,MTIME
            IF (SNUMS(IP).EQ.LS) THEN
               IF (REFAS(LIF,LP,IP).EQ.ANT) THEN
                  TI = (TIMR(1,IP) + TIMR(2,IP)) / 2.0
 20               IF ((TI.GE.TI1) .AND. (TI.LE.TI2)) THEN
                     IF (TI2.GT.TI1) THEN
                        W1 = (TI2 - TI) / (TI2 - TI1)
                     ELSE
                        W1 = 1.0
                        END IF
                  ELSE IF (TI.LT.TI1) THEN
                     W1 = 1.0
                  ELSE IF (IP2.GE.NP) THEN
                     W1 = 0.0
                  ELSE
                     RE1 = RE2
                     IM1 = IM2
                     TI1 = TI2
                     KP1 = KP2
                     IP1 = IP1 + 1
                     IP2 = MIN (IP2+1, NP)
                     KP2 = LIST(IP2)
                     IF (REFAS(LIF,LP,KP2).EQ.REFANT) THEN
                        RE2 = CREAL(REFANT,LIF,LP,KP2)
                        IM2 = CIMAG(REFANT,LIF,LP,KP2)
                     ELSE
                        RE2 = CREAL(ANT,LIF,LP,KP2)
                        IM2 = -CIMAG(ANT,LIF,LP,KP2)
                        END IF
                     TI2 = (TIMR(1,KP2) + TIMR(2,KP2)) / 2.0
                     GO TO 20
                     END IF
C                                       interpolate
                  W2 = 1.0 - W1
                  RE = W1 * RE1 + W2 * RE2
                  IM = W1 * IM1 + W2 * RE2
                  AMP = SQRT (RE * RE + IM * IM)
                  RE = RE / AMP
                  IM = IM / AMP
                  DO 30 LA = 1,CANT
                     IF ((CREAL(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                  (CIMAG(LA,LIF,LP,IP).NE.FBLANK) .AND.
     *                  ((CREAL(LA,LIF,LP,IP).NE.0.0) .OR.
     *                  (CIMAG(LA,LIF,LP,IP).NE.0.0))) THEN
                        TRE = CREAL(LA,LIF,LP,IP)
                        TIM = CIMAG(LA,LIF,LP,IP)
                        CREAL(LA,LIF,LP,IP) = TRE * RE - TIM * IM
                        CIMAG(LA,LIF,LP,IP) = TRE * IM + TIM * RE
                        END IF
 30                  CONTINUE
                  REFAS(LIF,LP,IP) = REFANT
                  END IF
               END IF
 50         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CANNOT REFERENCE ANT',I3,' TO',I3,' AT IF/POL',I3,I2)
      END
      REAL FUNCTION MEDIUM (A, B, LEN)
C-----------------------------------------------------------------------
C   The median of the first LEN numbers in A.  LEN should be an odd
C   number.
C
C   Input:
C      LEN       I         Length of array
C      A         R(*)      Data array
C      B         R(*)      Second array
C                             ignore points that have A(I)=B(I)=0.0
C   Usage example for remembrance
C
C      MEDRE = MEDIUM (RE, FILWID)
C                                       rms
C      DO 60 I = 1, FILWID
C         ADIFF(I) = ABS(RE(I) - MEDRE)
C 60      CONTINUE
C      SIGMA = ROOT2 * 1.5 * MEDIUM(ADIFF, FILWID)
C
C-----------------------------------------------------------------------
      REAL      A(*), B(*)
      INTEGER   LEN
C
      INTEGER   I, L
      REAL      X(50001), MEDIAN
C-----------------------------------------------------------------------
C                                       select
      L = 0
      DO 10 I = 1,LEN
         IF ((A(I).NE.0.0) .OR. (B(I).NE.0.0)) THEN
            L = L + 1
            X(L) = A(I)
            END IF
 10      CONTINUE
C                                       sort
      IF (L.GT.0) THEN
         MEDIUM = MEDIAN (L, X)
      ELSE
         MEDIUM = 0.0
         END IF
C
 999  RETURN
      END
